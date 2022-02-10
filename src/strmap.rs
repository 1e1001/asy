//! string interning map

use std::collections::HashMap;
use std::fmt;
use std::sync::{Arc, Weak};

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct MappedStr(Arc<String>);

impl MappedStr {
	pub fn get_ref(&self) -> &str {
		&self.0
	}
}

impl fmt::Debug for MappedStr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{:?}", self.0)
	}
}

impl ToString for MappedStr {
	fn to_string(&self) -> String { self.0.to_string() }
}

pub struct StrMap {
	data: HashMap<String, Weak<String>>,
}

impl fmt::Debug for StrMap {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut tuple = f.debug_tuple("StrMap");
		for (k, _) in &self.data {
			tuple.field(k);
		}
		tuple.finish()
	}
}


impl StrMap {
	pub fn new() -> Self {
		Self {
			data: HashMap::new(),
		}
	}
	pub fn add<T: Into<String>>(&mut self, v: T) -> MappedStr {
		let v = v.into();
		match match self.data.get(&v) {
			Some(r) => match r.upgrade() {
				Some(r) => Some(r),
				None => None,
			},
			None => None,
		} {
			Some(r) => MappedStr(r),
			None => {
				let res = Arc::new(v.clone());
				self.data.insert(v, Arc::downgrade(&res));
				MappedStr(res)
			},
		}
		// self.data.entry(v.clone()).or_insert(MappedStr(Arc::new(v))).clone()
	}
	pub fn gc(&mut self) {
		let keys: Vec<_> = self.data.keys().map(|i| i.clone()).collect();
		let mut saved = 0;
		for k in keys {
			let exists = {
				// block here so the upgraded Arc gets dropped
				match self.data[&k].upgrade() {
					Some(_) => true,
					None => false,
    		}
			};
			if !exists {
				saved += 1;
				self.data.remove(&k);
			}
		}
		log::debug!("StrMap gc() removed {} string(s)", saved);
	}
}
