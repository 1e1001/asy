use std::fs;

use discord::{Discord, State, ChannelRef, model::Event};
use log::{error, warn, debug, info};

mod parse;

#[derive(serde::Serialize, serde::Deserialize)]
struct EnvData {
	token: String,
	owner: u64,
}

fn channel_name(ch: Option<ChannelRef>) -> String {
	match ch {
		Some(ChannelRef::Public(server, channel)) => format!("{}#{}", server.name, channel.name),
		Some(ChannelRef::Group(group)) => format!("group {}", group.name()),
		Some(ChannelRef::Private(dm)) => format!("private {}",dm.recipient.name),
		None => "unknown".to_string()
	}
}

fn main() {
	env_logger::Builder::new()
		.filter_level(log::LevelFilter::Info)
		.filter_module("asy", log::LevelFilter::Trace)
		.init();
	let env_data: EnvData = toml::from_slice(&fs::read(".env.toml").expect("no env"))
		.expect("env parse fail");
	let discord = Discord::from_bot_token(&env_data.token)
		.expect("login fail");
	let (mut connection, ready) = discord.connect().expect("connect fail");
	let mut state = State::new(ready);
	info!("*hacker voice* i'm in");
	loop {
		let event = match connection.recv_event() {
			Ok(event) => event,
			Err(discord::Error::Closed(code, body)) => {
				error!("connection closed: status {:?}: {}", code, body);
				break;
			},
			Err(err) => {
				warn!("Got error: {:?}", err);
				continue;
			}
		};
		state.update(&event);
		match event {
			Event::MessageCreate(message) => {
				debug!("{} <{}> {}", channel_name(state.find_channel(message.channel_id)), message.author.name, message.content);
				// attempt to parse the message
			},
			_ => {}
		}
	}
}
