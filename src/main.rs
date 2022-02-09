#![feature(trace_macros)]
use std::fs;
use std::sync::Arc;

use serenity::Client;
use serenity::client::{EventHandler, Context};
use serenity::model::channel::Message;
use serenity::model::gateway::Ready;
use serenity::prelude::TypeMapKey;
use tokio::sync::RwLock;

mod lisp;
mod utils;

#[derive(serde::Serialize, serde::Deserialize)]
struct EnvData {
	token: String,
	owner: u64,
}

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
	env: lisp::AsylEnv,
}

impl GlobalData {
	fn new() -> Self {
		Self {
			user_id: 0,
			env: lisp::default_env(),
		}
	}
}
impl TypeMapKey for GlobalData {
	type Value = Arc<RwLock<GlobalData>>;
}

pub struct Handler;

// trace_macros!(true);

macro_rules! handler {
	($(async fn $name:tt ($($arg:tt: $val:tt),*) $code:tt)+) => {
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

async fn eval_print(context: &Context, msg: &Message, text: &str) {
	match match lisp::tokenize(text) {
		Ok(tokens) => match lisp::parse_all(&tokens) {
			Ok(parse) => {
				let lock = get_data(&context).await;
				let env = &mut lock.write().await.env;
				let mut errors = vec![];
				let mut res = parse
					.iter()
					.map(|i| {
						match lisp::eval(i, env) {
							Ok(v) => v.0.to_string(),
							Err(v) => { errors.push(v); "<error>".to_string() },
						}
					})
					.collect::<Vec<_>>().join(" ");
				if errors.len() > 0 {
					res.push_str("\nerrors:\n");
					for i in errors {
						res.push_str(&format!("{}{}", i, i.print_span(&msg.author.name, text)));
					}
				}
				Some(res)
			},
			Err(err) => Some(format!("Error during parsing:\n{}{}", err, err.print_span(&msg.author.name, text))),
		},
		Err(err) => Some(format!("Error during tokenizing:\n{}{}", err, err.print_span(&msg.author.name, text)))
	} {
		Some(v) => {
			if let Err(err) = msg.reply_ping(&context.http, v).await {
				log::error!("message error: {}", err);
			}
		},
		None => {},
	}
}

handler! {
	async fn message(context: Context, msg: Message) {
		if msg.author.id == get_data(&context).await.read().await.user_id { return }
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
		data.insert::<GlobalData>(Arc::new(RwLock::new(GlobalData::new())))
	}
	if let Err(err) = client.start().await {
		log::error!("client: {:?}", err);
	}
}
