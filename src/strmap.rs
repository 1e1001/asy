//! string interning map

use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct MappedStr(Arc<String>);

impl MappedStr {
	pub fn new<T: Into<String>>(data: T, map: &mut StrMap) -> Self {
		map.intern(data)
	}
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
	data: HashMap<String, MappedStr>,
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
	pub fn intern<T: Into<String>>(&mut self, v: T) -> MappedStr {
		let v = v.into();
		self.data.entry(v.clone()).or_insert(MappedStr(Arc::new(v))).clone()
	}
}
