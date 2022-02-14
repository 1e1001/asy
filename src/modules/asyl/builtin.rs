use std::collections::{HashMap, BTreeMap};
use std::sync::Arc;

use tokio::sync::RwLock;

use crate::modules::asyl::expr::AsylType;
use crate::strmap::MappedStr;

use super::expr::{AsylExpr, AsylExprValue};
use super::scope::{AsylScope, AsylScopeEntry, AsylEnv};

#[derive(Debug)]
pub struct AsylRootScope {
	data: HashMap<MappedStr, AsylExpr>,
}
impl AsylScope for AsylRootScope {
	fn get_ring(&self, name: &MappedStr) -> Option<&BTreeMap<usize, AsylScopeEntry>> {
		panic!("can't call get_ring on a RootScope");
	}
	fn get_var(&self, name: &MappedStr, index: usize) -> Option<(&AsylScopeEntry, usize)> {
		self.data.get(name).map(|i| (&AsylScopeEntry::Evaluated(i.clone()), 0))
	}
	fn set_var(&mut self, name: MappedStr, index: usize, value: AsylScopeEntry) {
		panic!("can't call set_var on a RootScope");
	}
	fn get_parent(&self) -> Option<&Arc<RwLock<dyn AsylScope>>> {
		None
	}
}

pub fn default_scope(env: &mut AsylEnv) -> Arc<RwLock<dyn AsylScope>> {
	let data = HashMap::new();
	macro_rules! insert {
		($key:expr, $($val:tt)*) => {
			data.insert(env.map.add($key), AsylExpr(AsylExprValue::$($val)*, None, None));
		}
	}
	insert!("'f", Bool(false));
	insert!("'t", Bool(true));
	insert!("'n", Null);
	insert!("'symbol", Type(AsylType::Symbol));
	insert!("'string", Type(AsylType::String));
	insert!("'number", Type(AsylType::Number));
	insert!("'bool",   Type(AsylType::Bool));
	insert!("'type",   Type(AsylType::Type));
	insert!("'list",   Type(AsylType::List));
	insert!("'fn",     Type(AsylType::Fn));
	insert!("'null",   Type(AsylType::Null));
	Arc::new(RwLock::new(AsylRootScope { data }))
}
