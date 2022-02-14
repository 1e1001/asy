use std::collections::{BTreeMap, HashMap};
use std::fmt;
use std::sync::Arc;

use tokio::sync::RwLock;

use crate::strmap::{MappedStr, StrMap};

use super::expr::AsylExpr;

pub struct AsylEnv {
	pub time: usize,
	pub map: StrMap,
}

#[derive(Debug, Clone)]
pub enum AsylScopeEntry {
	/// once you've just defined something
	Unevaluated(AsylExpr),
	/// while you're using it
	Evaluating,
	/// once you've used it
	Evaluated(AsylExpr),
}

pub trait AsylScope: Sync + Send + fmt::Debug {
	fn get_ring(&self, name: &MappedStr) -> Option<&BTreeMap<usize, AsylScopeEntry>>;
	fn get_var(&self, name: &MappedStr, max_index: usize) -> Option<(&AsylScopeEntry, usize)> {
		match self.get_ring(name) {
			Some(ring) => {
				if max_index == 0 {
					match ring.iter().rev().next() {
						Some(v) => Some((v.1, *v.0)),
						None => None
					}
				} else {
					let mut best_index = 0;
					for k in ring.keys() {
						if *k <= max_index {
							best_index = *k;
						}
					}
					if best_index > 0 {
						Some((&ring[&best_index], best_index))
					} else {
						None
					}
				}
			},
			None => None
		}
	}
	fn set_var(&mut self, name: MappedStr, index: usize, value: AsylScopeEntry);
	fn get_parent(&self) -> Option<&Arc<RwLock<dyn AsylScope>>>;
}
#[derive(Debug)]
pub struct AsylLambdaScope {
	pub data: HashMap<MappedStr, BTreeMap<usize, AsylScopeEntry>>,
	pub parent: Arc<RwLock<dyn AsylScope>>,
}
impl AsylScope for AsylLambdaScope {
	fn get_ring(&self, name: &MappedStr) -> Option<&BTreeMap<usize, AsylScopeEntry>> {
		self.data.get(name)
	}
	fn set_var(&mut self, name: MappedStr, index: usize, value: AsylScopeEntry) {
		let ent = self.data.entry(name).or_insert_with(|| BTreeMap::new());
		ent.insert(index, value);
	}
	fn get_parent(&self) -> Option<&Arc<RwLock<dyn AsylScope>>> {
		Some(&self.parent)
	}
}
