#![feature(trace_macros)]
use std::fs;
use std::sync::{Arc, Mutex};

use log::{info, error};
use serenity::Client;
use serenity::client::{EventHandler, Context};
use serenity::model::channel::Message;
use serenity::model::gateway::Ready;
use serenity::prelude::TypeMapKey;

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
}

impl GlobalData {
	fn new() -> Self {
		Self {
			user_id: 0,
		}
	}
}
impl TypeMapKey for GlobalData {
	type Value = Arc<Mutex<GlobalData>>;
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

fn loose_check(c: Option<char>) -> bool {
	match match c {
		Some(c) => c,
		None => ' ',
	} {
		'('|')' => true,
		'['|']' => true,
		'{'|'}' => true,
		_ => false,
	}
}

handler! {
	async fn message(context: Context, msg: Message) {
		if msg.author.id == this.ready.as_ref().unwrap().user.id { return }
		if msg.content.chars().count() > 2 {
			let mut content_chars = msg.content.chars();
			let first = content_chars.next();
			let last = content_chars.last();
			if loose_check(first) && loose_check(last) {
				match match lisp::tokenize(&msg.content) {
					Ok(tokens) => {
						match lisp::parse(&tokens) {
							Ok((parse, rest)) => {
								if rest.len() == 0 {
									Some(format!("{}", parse))
								} else {
									None
								}
							},
							Err(err) => Some(format!("parse error\n{:?}", err)),
						}
					},
					Err(err) => Some(format!("token error\n{:?}", err))
				} {
					Some(v) => {
						if let Err(err) = msg.reply_ping(&context.http, v).await {
							error!("message error: {}", err);
						}
					},
					None => {},
				}
			}
		}
	}
	async fn ready(context: Context, ready: Ready) {
		info!("logged in as {}", ready.user.name);
		{
			let data = context.data.read().await;
			data.get::<GlobalData>().unwrap().clone();
		}
		// todo here
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
		data.insert::<GlobalData>(Arc::new(Mutex::new(GlobalData::new())))
	}
	if let Err(err) = client.start().await {
		error!("client: {:?}", err);
	}
}
