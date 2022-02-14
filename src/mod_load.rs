use std::sync::Arc;

use serenity::prelude::TypeMapKey;
use tokio::sync::RwLock;

use serenity::client::{EventHandler, Context};

pub struct ModuleInfo {
	name: &'static str,
	desc: &'static str,
	manual: &'static str,
}
#[macro_export]
macro_rules! make_module {
	($module_type:ty, $data_type:ty) => {
		impl AutoModule for $module_type {}
		impl serenity::prelude::TypeMapKey for $data_type {
			type Value = Arc<RwLock<Self>>;
		}
	};
}
#[async_trait::async_trait]
pub trait AutoModule {
	async fn get_data<T: TypeMapKey>(&self, context: &Context) -> Arc<RwLock<T>>
	where T: TypeMapKey<Value = Arc<RwLock<T>>> + Send {
		let res = {
			let lock = context.data.read().await;
			lock.get::<T>()
		};
		match res {
			Some(v) => v.clone(),
			None => panic!("data type not in map!"),
		}
	}
	async fn load_data<T>(&self, context: &Context, value: T)
	where T: TypeMapKey<Value = Arc<RwLock<T>>> + Send {
		let mut lock = context.data.write().await;
		lock.insert::<T>(Arc::new(RwLock::new(value)));
	}
}
#[async_trait::async_trait]
pub trait Module: EventHandler {
	fn module_info(&self) -> ModuleInfo;
}
