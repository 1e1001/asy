#![feature(trace_macros)]
#![feature(box_syntax)]
#![feature(bigint_helper_methods)]
use std::collections::HashMap;
use std::{fs, fmt};
use std::sync::Arc;
use std::process::Command;

use mod_load::Module;
use serenity::Client;
use serenity::client::bridge::gateway::event::ShardStageUpdateEvent;
use serenity::client::{EventHandler, Context};
use serenity::http::CacheHttp;
use serenity::model::channel::{Message, GuildChannel, ChannelCategory, Channel, Reaction, StageInstance, PartialGuildChannel};
use serenity::model::event::{ChannelPinsUpdateEvent, GuildMembersChunkEvent, InviteCreateEvent, InviteDeleteEvent, MessageUpdateEvent, PresenceUpdateEvent, ResumedEvent, TypingStartEvent, VoiceServerUpdateEvent, ThreadListSyncEvent, ThreadMembersUpdateEvent};
use serenity::model::gateway::{Presence, Ready};
use serenity::model::guild::{Guild, GuildUnavailable, Emoji, Member, Role, PartialGuild, ThreadMember};
use serenity::model::id::{GuildId, EmojiId, RoleId, ChannelId, MessageId};
use serenity::model::user::{User, CurrentUser};
use serenity::model::voice::VoiceState;
use serenity::prelude::TypeMapKey;
use serde_json::Value;
use tokio::sync::RwLock;

mod modules;
mod mod_load;
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
	modules: Vec<Arc<dyn Module>>,
}

impl GlobalData {
	fn new(env: EnvData) -> Self {
		Self {
			user_id: 0,
			owner_id: env.owner,
			modules: modules::gen_modules(),
		}
	}
}
impl TypeMapKey for GlobalData {
	type Value = Arc<RwLock<GlobalData>>;
}

pub struct Handler;

async fn trigger_modules(ctx: &Context) {
	let modules = {
		let lock = get_data(ctx).await;
		let data = lock.read().await;
		data.modules.clone()
	};
}

macro_rules! trigger_modules {
	($context:tt) => {trigger_modules(&$context)};
}

macro_rules! handler_code_impl {
	($name:tt ($context:ident: $context_ty:ty, $($arg:ident: $val:ty),* $(,)?) auto) => {
		pub async fn $name($context: $context_ty, $($arg: $val),*) {
			trigger_modules!($context);
		}
	};
	($name:tt ($($arg:ident: $val:ty),* $(,)?) $code:tt) => {
		pub async fn $name($($arg: $val),*) $code
	};
}

// force rust-analyzer to rust-analyze my async trait
// probably the best macro i've ever made
macro_rules! handler {
	($(async fn $name:tt ($($arg:ident: $val:ty),* $(,)?) $code:tt)+) => {
		mod handler_impl {
			use super::*;
			$(handler_code_impl!{$name($($arg: $val),*) $code})+
		} #[async_trait::async_trait]
		impl EventHandler for Handler {
			$(async fn $name(&self, $($arg: $val),*) {
				handler_impl::$name($($arg),*).await;
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

handler! { // hell :)
	async fn cache_ready(_ctx: Context, _guilds: Vec<GuildId>) auto
	async fn channel_create(_ctx: Context, _channel: &GuildChannel) auto
	async fn category_create(_ctx: Context, _category: &ChannelCategory) auto
	async fn category_delete(_ctx: Context, _category: &ChannelCategory) auto
	async fn channel_delete(_ctx: Context, _channel: &GuildChannel) auto
	async fn channel_pins_update(_ctx: Context, _pin: ChannelPinsUpdateEvent) auto
	async fn channel_update(_ctx: Context, _old: Option<Channel>, _new: Channel) auto
	async fn guild_ban_addition(_ctx: Context, _guild_id: GuildId, _banned_user: User) auto
	async fn guild_ban_removal(_ctx: Context, _guild_id: GuildId, _unbanned_user: User) auto
	async fn guild_create(_ctx: Context, _guild: Guild, _is_new: bool) auto
	async fn guild_delete(
		_ctx: Context,
		_incomplete: GuildUnavailable,
		_full: Option<Guild>,
	) auto
	async fn guild_emojis_update(
		_ctx: Context,
		_guild_id: GuildId,
		_current_state: HashMap<EmojiId, Emoji>,
	) auto
	async fn guild_integrations_update(_ctx: Context, _guild_id: GuildId) auto
	async fn guild_member_addition(_ctx: Context, _guild_id: GuildId, _new_member: Member) auto
	async fn guild_member_removal(
		_ctx: Context,
		_guild_id: GuildId,
		_user: User,
		_member_data_if_available: Option<Member>,
	) auto
	async fn guild_member_update(
		_ctx: Context,
		_old_if_available: Option<Member>,
		_new: Member,
	) auto
	async fn guild_members_chunk(_ctx: Context, _chunk: GuildMembersChunkEvent) auto
	async fn guild_role_create(_ctx: Context, _guild_id: GuildId, _new: Role) auto
	async fn guild_role_delete(
		_ctx: Context,
		_guild_id: GuildId,
		_removed_role_id: RoleId,
		_removed_role_data_if_available: Option<Role>,
	) auto
	async fn guild_role_update(
		_ctx: Context,
		_guild_id: GuildId,
		_old_data_if_available: Option<Role>,
		_new: Role,
	) auto
	async fn guild_unavailable(_ctx: Context, _guild_id: GuildId) auto
	async fn guild_update(
		_ctx: Context,
		_old_data_if_available: Option<Guild>,
		_new_but_incomplete: PartialGuild,
	) auto
	async fn invite_create(_ctx: Context, _data: InviteCreateEvent) auto
	async fn invite_delete(_ctx: Context, _data: InviteDeleteEvent) auto
	async fn message(ctx: Context, msg: Message) {
		let (bot_id, owner_id) = {
			let read = get_data(&ctx).await;
			let lock = read.read().await;
			(lock.user_id, lock.owner_id)
		};
		if msg.author.id == bot_id { return }
		// move this to core
		if msg.author.id == owner_id {
			if msg.content == "$asy:pull" {
				run_command(&ctx, &msg, &["/usr/bin/git", "pull"]).await;
			} else if msg.content == "$asy:build" {
				run_command(&ctx, &msg, &["/usr/bin/cargo", "build"]).await;
			} else if msg.content == "$asy:reboot" {
				handle_err(msg.reply_ping(&ctx.http, "cya!").await);
				std::process::exit(0);
			}
		}
		trigger_modules!(ctx);
	}
	async fn message_delete(
		_ctx: Context,
		_channel_id: ChannelId,
		_deleted_message_id: MessageId,
		_guild_id: Option<GuildId>,
	) auto
	async fn message_delete_bulk(
		_ctx: Context,
		_channel_id: ChannelId,
		_multiple_deleted_messages_ids: Vec<MessageId>,
		_guild_id: Option<GuildId>,
	) auto
	async fn message_update(
		_ctx: Context,
		_old_if_available: Option<Message>,
		_new: Option<Message>,
		_event: MessageUpdateEvent,
	) auto
	async fn reaction_add(_ctx: Context, _add_reaction: Reaction) auto
	async fn reaction_remove(_ctx: Context, _removed_reaction: Reaction) auto
	async fn reaction_remove_all(
		_ctx: Context,
		_channel_id: ChannelId,
		_removed_from_message_id: MessageId,
	) auto
	async fn presence_replace(_ctx: Context, _data: Vec<Presence>) auto
	async fn presence_update(_ctx: Context, _new_data: PresenceUpdateEvent) auto
	async fn ready(ctx: Context, ready: Ready) {
		log::info!("logged in as {}", ready.user.name);
		{
			let lock = get_data(&ctx).await;
			let mut global_data = lock.write().await;
			global_data.user_id = ready.user.id.0;
		}
		trigger_modules!(ctx);
	}
	async fn resume(_ctx: Context, _resume: ResumedEvent) auto
	async fn shard_stage_update(_ctx: Context, _data: ShardStageUpdateEvent) auto
	async fn typing_start(_ctx: Context, _data: TypingStartEvent) auto
	async fn unknown(_ctx: Context, _name: String, _raw: Value) auto
	async fn user_update(_ctx: Context, _old_data: CurrentUser, _new: CurrentUser) auto
	async fn voice_server_update(_ctx: Context, _data: VoiceServerUpdateEvent) auto
	async fn voice_state_update(
		_ctx: Context,
		_guild: Option<GuildId>,
		_old: Option<VoiceState>,
		_new: VoiceState,
	) auto
	async fn webhook_update(
		_ctx: Context,
		_guild_id: GuildId,
		_belongs_to_channel_id: ChannelId,
	) auto
	async fn stage_instance_create(_ctx: Context, _stage_instance: StageInstance) auto
	async fn stage_instance_update(_ctx: Context, _stage_instance: StageInstance) auto
	async fn stage_instance_delete(_ctx: Context, _stage_instance: StageInstance) auto
	async fn thread_create(_ctx: Context, _thread: GuildChannel) auto
	async fn thread_update(_ctx: Context, _thread: GuildChannel) auto
	async fn thread_delete(_ctx: Context, _thread: PartialGuildChannel) auto
	async fn thread_list_sync(_ctx: Context, _thread_list_sync: ThreadListSyncEvent) auto
	async fn thread_member_update(_ctx: Context, _thread_member: ThreadMember) auto
	async fn thread_members_update(
		_ctx: Context,
		_thread_members_update: ThreadMembersUpdateEvent,
	) auto
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
