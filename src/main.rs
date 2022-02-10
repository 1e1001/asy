#![feature(trace_macros)]
#![feature(box_syntax)]
use std::{fs, fmt};
use std::sync::Arc;
use std::process::Command;

use serenity::Client;
use serenity::client::{EventHandler, Context};
use serenity::http::CacheHttp;
use serenity::model::channel::Message;
use serenity::model::gateway::Ready;
use serenity::prelude::TypeMapKey;
use strmap::StrMap;
use tokio::sync::RwLock;

mod lisp;
mod utils;
mod strmap;

#[derive(serde::Deserialize)]
struct EnvData {
	token: String,
	owner: u64,
}

// old code, could be useful someday
// fn channel_name(ch: Option<ChannelRef>) -> String {
// 	match ch {
// 		Some(ChannelRef::Public(server, channel)) => format!("{}#{}", server.name, channel.name),
// 		Some(ChannelRef::Group(group)) => format!("group {}", group.name()),
// 		Some(ChannelRef::Private(dm)) => format!("private {}",dm.recipient.name),
// 		None => "unknown".to_string()
// 	}
// }


pub struct GlobalData {
	user_id: u64,
	owner_id: u64,
	env: Arc<std::sync::RwLock<lisp::AsylEnv>>,
	map: StrMap,
}

impl GlobalData {
	fn new(env: EnvData) -> Self {
		let mut map = StrMap::new();
		Self {
			user_id: 0,
			owner_id: env.owner,
			env: Arc::new(std::sync::RwLock::new(lisp::default_env(&mut map))),
			map
		}
	}
}
impl TypeMapKey for GlobalData {
	type Value = Arc<RwLock<GlobalData>>;
}

pub struct Handler;

// force rust-analyzer to autocomplete my async trait
// probably the best macro i've ever made
macro_rules! handler {
	($(async fn $name:tt ($($arg:tt: $val:tt),*) $code:block)+) => {
		mod handler_impl {
			use super::*;
			$(pub async fn $name($($arg: $val),*) $code)+
		} #[async_trait::async_trait]
		impl EventHandler for Handler {
			$(async fn $name(&self, $($arg: $val),*) {
				handler_impl::$name($($arg),*).await
			})+
		}
	};
}

async fn get_data(ctx: &Context) -> Arc<RwLock<GlobalData>> {
	let data = ctx.data.read().await;
	data.get::<GlobalData>().unwrap().clone()
}

fn handle_err<T, E: std::error::Error>(f: Result<T, E>) -> Option<T> {
	match f {
		Ok(v) => Some(v),
		Err(e) => {
			log::error!("internal error: {}", e);
			None
		},
	}
}

async fn eval_print(context: &Context, msg: &Message, text: &str) {
	let lock = get_data(&context).await;
	let mut data = lock.write().await;
	let name_mapped = data.map.add(&msg.author.name);
	let text_mapped = data.map.add(text);
	match match lisp::tokenize(&mut data.map, name_mapped, text_mapped) {
		Ok(tokens) => match lisp::parse_all(&tokens) {
			Ok(parse) => {
				// todo: get some sort of user env here?
				// we need nested envs before that though
				let mut errors = vec![];
				let mut res = parse
					.iter()
					.map(|i| {
						match lisp::eval(i, &data.env.clone(), &mut data.map) {
							Ok(v) => v.to_string(),
							Err(v) => { errors.push(v); "<error>".to_string() },
						}
					})
					.collect::<Vec<_>>().join("\n");
				if errors.len() > 0 {
					res.push_str("\nerrors:\n");
					for i in errors {
						res.push_str(&format!("{}{}", i, i.print()));
					}
				}
				Some(res)
			},
			Err(err) => Some(format!("Parsing error:\n{}{}", err, err.print())),
		},
		Err(err) => Some(format!("Tokenizing error:\n{}{}", err, err.print()))
	} {
		Some(v) => { handle_err(msg.reply_ping(&context.http, v).await); },
		None => {},
	}
	data.map.gc();
}

async fn edit_or_reply(cache: impl CacheHttp, edit: Option<Message>, reply: &Message, c: impl fmt::Display) -> serenity::Result<()> {
	match edit {
		Some(mut v) => v.edit(cache, |m| m.content(c)).await,
		None => {
			reply.reply_ping(cache, c).await?;
			Ok(())
		},
	}
}

async fn run_command(context: &Context, msg: &Message, cmd: &[&str]) {
	let edit = handle_err(msg.reply_ping(&context.http, format!("`{}`...", cmd.join(" "))).await);
	let mut command = Command::new(cmd[0]);
	for i in &cmd[1..] {
		command.arg(i);
	}
	match command.output() {
		Ok(out) => {
			let content = format!("### stdout ###\n{}\n### stderr ###\n{}\n",
				String::from_utf8_lossy(&out.stdout),
				String::from_utf8_lossy(&out.stderr));
			handle_err(msg.channel_id.send_message(&context.http, |m| {
				m.content("done");
				m.reference_message(msg);
				m.add_file((content.as_bytes(), "logs.txt"));
				m
			}).await);
		},
		Err(e) => { handle_err(edit_or_reply(&context.http, edit, &msg, format!("`git pull`:\nerror: {}", e)).await); },
	}
}

handler! {
	async fn message(context: Context, msg: Message) {
		let (bot_id, owner_id) = {
			let read = get_data(&context).await;
			let lock = read.read().await;
			(lock.user_id, lock.owner_id)
		};
		if msg.author.id == bot_id { return }
		if msg.author.id == owner_id {
			if msg.content == "$asyl:env" {
				handle_err(msg.reply_ping(&context.http, format!("{:?}", get_data(&context).await.read().await.env)).await);
			} else if msg.content == "$asyl:intern" {
				handle_err(msg.reply_ping(&context.http, format!("{:?}", get_data(&context).await.read().await.map)).await);
			} else if msg.content == "$asyl:pull" {
				run_command(&context, &msg, &["/usr/bin/git", "pull"]).await;
			} else if msg.content == "$asyl:build" {
				run_command(&context, &msg, &["/usr/bin/cargo", "build"]).await;
			} else if msg.content == "$asyl:reboot" {
				handle_err(msg.reply_ping(&context.http, "cya!").await);
				std::process::exit(0);
			}
		}
		if msg.content.len() > 11 {
			let mut start_offset = 0;
			let mut values = vec![];
			// check for embedded ```asyl\n…```’s
			while let Some(start_idx) = msg.content[start_offset..].find("```asyl\n") {
				let off = start_offset + start_idx + 8;
				if let Some(end_idx) = msg.content[off..].find("```") {
					values.push(msg.content[off..off + end_idx].trim());
					start_offset = off + end_idx + 3;
				} else {
					break
				}
			}
			if values.len() > 0 {
				eval_print(&context, &msg, &values.join("\n")).await;
			}
		}
	}
	async fn ready(context: Context, ready: Ready) {
		log::info!("logged in as {}", ready.user.name);
		{
			let lock = get_data(&context).await;
			let mut global_data = lock.write().await;
			global_data.user_id = ready.user.id.0;
		}
	}
}

#[tokio::main]
async fn main() {
	env_logger::Builder::new()
		.filter_level(log::LevelFilter::Warn)
		.filter_module("asy::", log::LevelFilter::Trace)
		.init();
	let env_data: EnvData = toml::from_slice(&fs::read(".env.toml").expect("no env"))
		.expect("env parse fail");
	let mut client = Client::builder(&env_data.token)
		.event_handler(Handler)
		.await
		.expect("client fail");
	{
		let mut data = client.data.write().await;
		data.insert::<GlobalData>(Arc::new(RwLock::new(GlobalData::new(env_data))))
	}
	if let Err(err) = client.start().await {
		log::error!("client: {:?}", err);
	}
}
