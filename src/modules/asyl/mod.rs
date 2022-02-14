// mod lisp;

// pub use lisp::{AsylScope, AsylEnv, AsylRootScope, default_scope, tokenize, parse_all, eval};

use std::sync::Arc;

use serenity::model::channel::Message;
use serenity::model::gateway::Ready;
use tokio::sync::RwLock;

use serenity::client::{EventHandler, Context};

use crate::make_module;
use crate::mod_load::{Module, ModuleInfo, AutoModule};
use crate::strmap::StrMap;
use crate::utils::{fuck_err, pager, PagerLocation};

use self::builtin::default_scope;
use self::parse::{tokenize, parse_all};
use self::scope::{AsylScope, AsylEnv};

mod builtin;
mod error;
mod eval;
mod expr;
mod list;
mod number;
mod parse;
mod scope;

// async fn eval_print(context: &Context, msg: &Message, text: &str) {
// 	let lock = get_data(&context).await;
// 	let mut data = lock.write().await;
// 	let name_mapped = data.env.map.add(&msg.author.name);
// 	let text_mapped = data.env.map.add(text);
// 	match match asyl_old::tokenize(&mut data.env, name_mapped, text_mapped) {
// 		Ok(tokens) => match asyl_old::parse_all(&tokens, &mut data.env) {
// 			Ok(parse) => {
// 				// todo: get some sort of user env here?
// 				// we need nested envs before that though
// 				let mut errors = vec![];
// 				let mut res = parse
// 					.iter()
// 					.map(|i| {
// 						let time = data.env.time;
// 						match asyl_old::eval(i, &data.scope.clone(), &mut data.env, time) {
// 							Ok(v) => v.to_string(),
// 							Err(v) => { errors.push(v); "<error>".to_string() },
// 						}
// 					})
// 					.collect::<Vec<_>>().join("\n");
// 				if errors.len() > 0 {
// 					res.push_str("\nerrors:\n");
// 					for i in errors {
// 						res.push_str(&i.print());
// 					}
// 				}
// 				Some(res)
// 			},
// 			Err(err) => Some(format!("Parsing error:\n{}{}", err, err.print())),
// 		},
// 		Err(err) => Some(format!("Tokenizing error:\n{}{}", err, err.print()))
// 	} {
// 		Some(v) => { handle_err(msg.reply_ping(&context.http, v).await); },
// 		None => {},
// 	}
// 	data.env.map.gc();
// }

pub struct HandlerData {
	scope: Arc<RwLock<dyn AsylScope>>,
	env: AsylEnv,
}
pub struct Handler;
make_module!{Handler, HandlerData}
impl Module for Handler {
	fn module_info(&self) -> ModuleInfo {
		ModuleInfo {
			name: "asyl",
			desc: "lisp discord",
			manual: r##"
this document assumes you're familiar with lisps, if you aren't, cry about it.
asyl takes inspiration from racket, clojure, and bad ideas i have.
to use it, put code inside \`\`\`asyl … \`\`\` code blocks

here's a quick reference:
- `(fn (args…) body…)` a lambda function
- `(def name value)` define a variable
- `(def (name args…) body)` define function shorthand (de-sugars into `(def name (fn (args…) body…))`)
"##,
		}
	}
}
#[async_trait::async_trait]
impl EventHandler for Handler {
	async fn message(&self, ctx: Context, msg: Message) {
		if msg.content.len() <= 11 {
			return;
		}
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
			let text = values.join("\n");
			let mut data = self.get_data::<HandlerData>(&ctx).await.write().await;
			let name_mapped = data.env.map.add(&msg.author.name);
			let text_mapped = data.env.map.add(text);
			let mut errors = vec![];
			let mut res = match tokenize(&mut data.env, name_mapped, text_mapped) {
				Ok(tokens) => match parse_all(&tokens, &mut data.env) {
					Ok(parse) => {
						// todo: get some sort of user env here?
						// we need nested envs before that though
						let mut res = vec![];
						for expr in parse {
							let time = data.env.time;
							let (val, err) = match eval::eval(&expr, &data.scope.clone(), &mut data.env, time).await {
								Ok(v) => (v.to_string(), None),
								Err(v) => ("<error>".to_string(), Some(v)),
							};
							res.push(val);
							if let Some(v) = err {
								errors.push(v);
								// abort after the first error
								break
							}
						}
						let res = res.join("\n");
						res
					},
					Err(err) => {
						errors.push(err);
						"Parsing error".to_string()
					},
				},
				Err(err) => {
					errors.push(err);
					"Tokenization error".to_string()
				},
			};
			if errors.len() > 0 {
				res.push_str("\nerrors:\n");
				for i in errors {
					res.push_str(&i.print());
				}
			}
			// match  {
			// 	Some(v) => { handle_err(msg.reply_ping(&context.http, v).await); },
			// 	None => {},
			// }
			fuck_err(pager(&ctx, &res, &PagerLocation::Reply(msg)).await);
			data.env.map.gc();
		}
	}
	async fn ready(&self, ctx: Context, _: Ready) {
		let mut env = AsylEnv {
			time: 1,
			map: StrMap::new(),
		};
		let data = HandlerData {
			scope: default_scope(&mut env), env
		};
		self.load_data(&ctx, data);
		// let mut lock = self.get_data::<GlobalData>(&ctx).await.write().await;
		// *lock = GlobalData {
		// 	scope: default_scope(&mut env), env
		// }
	}
}
