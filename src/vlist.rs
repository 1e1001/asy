//! Immutable VList-like data structure

struct VListData<T> {
	values: Vec<T>,
	// turn this into a VListData and fix the shits
	next: Option<VList<T>>,
}

#[derive(Clone)]
pub struct VList<T> {
	data: Rc<VListData<T>>,
	offset: usize,
}

impl<T> VList<T> {
	pub fn new() -> Self {
		Self {
			data: Rc::new(VListData {
				values: vec![],
				next: None,
			}),
			offset: 0,
		}
	}
	/// panic-ing version of try_get
	pub fn get(&self, index: usize) -> T {
		match self.try_get() {
			Some(v) => v,
			None => panic!("index out of bounds"),
		}
	}
	/// get an item from the array
	pub fn try_get(&self, index: usize) -> Option<T> {
		let offset_index = index + self.offset
		if self.data.values.len() < offset_index {
			Some(self.data.values[offset_index])
		} else {
			match self.data.next {
				Some(next) => next.try_get(offset_index - self.data.values.len()),
				None => None,
			}
		}
	}
	pub fn len(&self) -> usize {
		self.data.values.len() + match self.data.next {
			Some(next) => next.len(),
			None => 0,
		}
	}
	pub fn iter(&self) -> VListIter<T> {
		VListIter(self.clone())
	}
	/// add several items in front of self
	pub fn append(&self, front: &[T]) -> Self {
		Self {
			data: Rc::new(VListData {
				values: front.into(),
				next: Some(self.clone())
			}),
			offset: 0,
		}
	}
	/// cut off items from the front of self
	pub fn try_chop(&self, n: usize) -> Option<Self> {
		let offset_index = self.data.values.len() - self.offset
		if n >= offset_index {
			match self.data.next {
				Some(v) => v.chop(n - offset_index),
				None => None
			}
		} else {
			Some(Self {
				data: self.data.clone(),
				offset: self.offset + n,
			})
		}
	}
	/// panic-ing version of try_chop
	pub fn chop(&self, n: usize) -> Option<Self> {
		match self.try_chop(n) {
			Some(v) => v,
			None => panic!("index out of bounds"),
		}
	}
	// lispy functions
	/// one-element cons, use append if appending multiple items
	pub fn cons(a: T, b: VList<T>) -> Self {
		b.append(a)
	}
	pub fn car(&self) -> T {
		self.get(0)
	}
	pub fn cdr(&self) -> Self {
		self.try_chop(1).unwrap_or(VList::new())
	}
}

impl<T> From<Vec<T>> for VList<T> {
	fn from(v: Vec<T>) -> Self {
		Self {
			data: Rc::new(VListData {
				values: v,
				next: None
			}),
			offset: 0,
		}
	}
}

pub struct VListIter<T>(VList<T>);

impl<T> Iterator for VListIter<T> {
	type Item = T;
	fn next(&mut self) -> Option<T> {
		match self.0.try_get(0) {
			Some(v) => {
				self.0.offset += 1;
				Some(v)
			},
			None => None
		}
	}
}

impl<T> IntoIterator<T> for &VList<T> {
	type Item = T;
	type IntoIter = VListIter<T>;
	fn into_iter(self) -> VListIter<T> {
		self.iter()
	}
}

impl<T> FromIterator<T> for VList<T> {
	fn from_iter<I>(iter: I) -> Self
	where I: IntoIterator<Item = T> {
		iter.collect::<Vec<T>>().into()
	}
}

impl<T> FusedIterator<T> for VList<T> { }