use std::iter;
use std::ops::{RangeBounds, Bound};
use std::str::Chars;

use serenity::builder::CreateMessage;
use serenity::client::Context;
use serenity::model::id::ChannelId;
use serenity::prelude::SerenityError;
use serenity::model::channel::Message;

pub fn safe_string(t: &str) -> String {
  let mut out = String::new();
  for ch in t.chars() {
		match ch {
			'\\'|'*'|'_'|'`'|'>'|'<'|'~'|'|' => {
			out.push('\\');
			}, _ => {}
		}
		out.push(ch);
  } out
}

pub type Dual = (Option<usize>, Option<usize>);

pub fn range_to_dual(r: impl RangeBounds<usize>) -> Dual {
	match r.start_bound() {
		Bound::Excluded(_) => panic!("invalid range"),
		Bound::Included(start) => match r.end_bound() {
			Bound::Excluded(end) => (Some(*start), Some(end - 1)),
			Bound::Included(end) => (Some(*start), Some(*end)),
			Bound::Unbounded => (Some(*start), None),
		},
		Bound::Unbounded => match r.end_bound() {
			Bound::Excluded(end) => (None, Some(end - 1)),
			Bound::Included(end) => (None, Some(*end)),
			Bound::Unbounded => (None, None),
		},
	}
}

/// print a range of values
pub fn format_arg_range(r: Dual) -> String {
	match r.0 {
		Some(start) => match r.1 {
			Some(end) => if start == end {
				format!("{} argument{}", start, if start == 1 {""} else {"s"})
			} else {
				format!("{} – {} arguments", start, end)
			},
			None => format!("at least {} argument{}", start, if start == 1 {""} else {"s"}),
		},
		None => match r.1 {
			Some(end) => if end == 0 {
				"0 arguments".to_string()
			} else {
				format!("0 – {} argument{}", end, if end == 1 {""} else {"s"})
			}
			None => "any number of arguments".to_string(),
		},
	}
}

/// print a list of values
pub fn format_list(items: &[String], and: bool) -> String {
	if items.len() == 0 {
		"nothing".to_string()
	} else if items.len() == 1 {
		items[0].to_string()
	} else if items.len() == 2 {
		format!("{} or {}", items[0], items[1])
	} else {
		items.iter().rev().skip(1).rev().map(|v| format!("{}, ", v)).chain(iter::once(format!("{} {}", if and {"and"} else {"or"}, items[items.len() - 1]))).collect::<String>()
	}
}

pub fn fuck_err<T, E: std::error::Error>(f: Result<T, E>) -> Option<T> {
	match f {
		Ok(v) => Some(v),
		Err(e) => {
			log::error!("internal error: {}", e);
			None
		},
	}
}

pub enum PagerLocation {
	Reply(Message),
	Post(ChannelId),
}

impl PagerLocation {
	async fn post(&self, context: &Context, content: &str) -> Result<Message, SerenityError> {
		self.post_fn(context, |m| {
			m.content(content);
			m
		}).await
	}
	fn get_channel(&self) -> (ChannelId, Option<&Message>) {
		match self {
			PagerLocation::Reply(msg) => (msg.channel_id, Some(msg)),
			PagerLocation::Post(channel) => (*channel, None),
		}
	}
	fn no_reply(&self) -> Self {
		PagerLocation::Post(self.get_channel().0)
	}
	async fn post_fn<'a, F>(&self, context: &Context, content: F) -> Result<Message, SerenityError>
	where for<'b> F: FnOnce(&'b mut CreateMessage<'a>) -> &'b mut CreateMessage<'a>{
		let (channel, reference) = self.get_channel();
		channel.send_message(&context.http, |m| {
			if let Some(msg) = reference {
				m.reference_message(msg);
			}
			content(m);
			m
		}).await
	}
}

pub async fn pager(context: &Context, content: &str, location: &PagerLocation) -> Result<(), SerenityError> {
	const MAX_LEN: usize = 1998;
	fn append_line(line: Chars, len: usize, pages: &mut Vec<String>, line_len: &mut usize) {
		if len + *line_len > MAX_LEN {
			if pages.len() > 0 {
				pages[pages.len() - 1].push('…');
			}
			if len > MAX_LEN {
				pages.push(line.take(MAX_LEN).collect::<String>());
				*line_len = MAX_LEN;
				append_line(line, len - MAX_LEN, pages, line_len);
			} else {
				let mut data = line.collect::<String>();
				data.push('\n');
				pages.push(data);
				*line_len = len + 1;
			}
		} else if pages.len() > 0 {
			let mut data = line.collect::<String>();
			data.push('\n');
			pages[pages.len() - 1].push_str(&data);
			*line_len += len + 1;
		} else {
			let mut data = line.collect::<String>();
			data.push('\n');
			pages.push(data);
			*line_len = len + 1;
		}
	}
	let mut pages = vec![];
	let mut page_len = 0;
	for line in content.lines() {
		// we need to create the iterator twice unfortunately
		let len = line.chars().count();
		append_line(line.chars(), len, &mut pages, &mut page_len);
	}
	let mut iter = pages.iter();
	match iter.next() {
		Some(v) => location.post(context, v).await?,
		None => {
			location.post(context, "\u{00AD}").await?;
			return Ok(())
		}
	};
	let location = location.no_reply();
	for page in iter {
		location.post(context, page).await?;
	}
	Ok(())
}

// async fn edit_or_reply(cache: impl CacheHttp, edit: Option<Message>, reply: &Message, c: impl fmt::Display) -> serenity::Result<()> {
// 	match edit {
// 		Some(mut v) => v.edit(cache, |m| m.content(c)).await,
// 		None => {
// 			reply.reply_ping(cache, c).await?;
// 			Ok(())
// 		},
// 	}
// }
