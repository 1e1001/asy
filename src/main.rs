#![feature(trace_macros)]
use std::fs;
use std::sync::Arc;

use log::{info, error, debug};
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

handler! {
	async fn message(context: Context, msg: Message) {
		if msg.author.id == get_data(&context).await.read().await.user_id { return }
		if msg.content.len() > 0 && msg.content.as_bytes()[0] == b'$' {
			match match lisp::tokenize(&msg.content[1..]) {
				Ok(tokens) => match lisp::parse_all(&tokens) {
					Ok(parse) => {
						let lock = get_data(&context).await;
						let env = &mut lock.write().await.env;
						Some(parse
							.iter()
							.map(|i| {
								match lisp::eval(i, env) {
									Ok(v) => format!("{}", v),
									Err(v) => format!("<Error: {}>", v),
								}
							}).collect::<Vec<_>>().join(" "))
					},
					Err(err) => Some(format!("Error during parsing: \n{}", err)),
				},
				Err(err) => Some(format!("Error during tokenizing: \n{}", err))
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
	async fn ready(context: Context, ready: Ready) {
		info!("logged in as {}", ready.user.name);
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
		error!("client: {:?}", err);
	}
}
