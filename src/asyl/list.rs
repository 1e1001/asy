use std::rc::Rc;
use std::cell::RefCell;

struct ListVec<T> {
	data: Vec<T>,
	/// used indexes in order (smallest index first)
	/// reference count is decremented by one here
	used: Vec<(usize, usize)>,
}
enum RefInsertResult {
	Insert(usize),
	Replace(usize),
	Add
}

struct ListData<T> {
	/// items stored in reverse order
	data: RefCell<ListVec<T>>,
	/// next list in the chain
	next: Option<List<T>>,
}

struct List<T> {
	/// referenced data
	data: Rc<ListData<T>>,
	/// starting index of the data + 1
	/// you can imagine this list using 0..offset
	offset: usize,
}

impl<T: std::fmt::Debug> std::fmt::Debug for ListVec<T> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("ListVec").field("data", &self.data).field("capacity", &self.data.capacity()).field("used", &self.used).finish()
	}
}

impl<T: std::fmt::Debug> std::fmt::Debug for ListData<T> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("ListData").field("data", &self.data).field("next", &self.next.as_ref().map(|v| v.debug())).finish()
	}
}

impl<T> ListVec<T> {
	fn new(capacity: usize) -> Self {
		Self {
			data: Vec::with_capacity(capacity),
			used: vec![],
		}
	}
	fn set(&mut self, index: usize, value: T) {
		if self.can_set(index) {
			if self.data.len() == index {
				self.data.push(value);
			} else {
				self.data[index] = value;
			}
		} else {
			panic!("can't set!");
		}
	}
	fn can_set(&self, index: usize) -> bool {
		if self.used.len() > 0 && self.used[self.used.len() - 1].0 >= index {
			false // value is being used
		} else if index < self.data.len() {
			true // value already allocated
		} else if self.data.len() < self.data.capacity() {
			true // space left in vector
		} else {
			false // no space left
		}
	}
	fn get_ref_index(&self, index: usize) -> RefInsertResult {
		for (i, (v, _)) in self.used.iter().enumerate() {
			if *v > index {
				return RefInsertResult::Insert(i)
			} else if *v == index {
				return RefInsertResult::Replace(i)
			}
		}
		RefInsertResult::Add
	}
	fn ref_index(&mut self, index: usize) {
		match self.get_ref_index(index) {
			RefInsertResult::Insert(v) => self.used.insert(v, (index, 0)),
			RefInsertResult::Replace(v) => self.used[v] = (index, self.used[v].1 + 1),
			RefInsertResult::Add => self.used.push((index, 0)),
		}
	}
	fn get_deref_index(&self, index: usize) -> usize {
		for (i, (v, _)) in self.used.iter().enumerate() {
			if *v == index {
				return i
			}
		}
		panic!("deref on non-referenced index");
	}
	fn deref_index(&mut self, index: usize) {
		let v = self.get_deref_index(index);
		match self.used[v].1 {
			0 => { self.used.remove(v); },
			o => self.used[v] = (index, o - 1),
		}
	}
}

impl<T> ListData<T> {
	fn new(len: usize) -> Self {
		Self {
			data: RefCell::new(ListVec::new(len)),
			next: None
		}
	}
	fn with_tail(len: usize, tail: List<T>) -> Self {
		Self {
			data: RefCell::new(ListVec::new(len)),
			next: Some(tail),
		}
	}
}

impl<T> List<T> {
	fn new() -> Self {
		Self {
			data: Rc::new(ListData::new(2)),
			offset: 0,
		}
	}
	fn cons(&self, value: T) -> Self {
		let can_set = {
			let borrow = self.data.data.borrow();
			borrow.can_set(self.offset)
		};
		if can_set {
			{
				let mut borrow = self.data.data.borrow_mut();
				borrow.set(self.offset, value);
				borrow.ref_index(self.offset);
			}
			Self {
				data: self.data.clone(),
				offset: self.offset + 1,
			}
		} else {
			// allocate a new ListData
			let block_len = {
				let borrow = self.data.data.borrow();
				borrow.data.capacity()
			};
			let data = ListData::with_tail(block_len * 2, self.clone());
			{
				let mut borrow = data.data.borrow_mut();
				borrow.set(0, value);
				borrow.ref_index(0);
			}
			Self {
				data: Rc::new(data),
				offset: 1,
			}
		}
	}
	fn list_ref<'a>(&'a self, idx: usize) -> &'a T {
		// eprint!("<list-ref {}>", idx);
		if idx >= self.offset {
			match &self.data.next {
				Some(v) => v.list_ref(idx - self.offset),
				None => panic!("index out of bounds"),
			}
		} else {
			// SAFETY: this is fine as long as:
			// - the list's index is makred as allocated
			//     it's ref'd when a List is cloned
			//     and deref'd when a List is dropped
			// - the vec never reallocates, invariant:
			//     ListVec will panic if it needs to reallocate
			//     the lifetime 'a ensures the reference is available until the List gets dropped (at which point the inner data could deallocate)
			//     TODO: maybe consider pinning the vector?
			unsafe {
				match self.data.data.try_borrow_unguarded() {
					Ok(v) => &v.data[self.offset - 1 - idx],
					Err(e) => panic!("data is currently being mutated ({})", e),
				}
			}
		}
	}
	fn car<'a>(&'a self) -> &'a T {
		self.list_ref(0)
		// &self.claim[self.claim.len() - 1]
		// &self.data.data.borrow().data[self.offset]
	}
	/// panics if self.len() == 0
	fn cdr(&self) -> Self {
		if self.offset == 0 {
			panic!("can't cdr at end of list");
		} else if self.offset == 1 && self.data.next.is_some() {
			// extract the next one now to hide away the 0-length item
			self.data.next.as_ref().unwrap().clone()
		} else {
			if self.offset >= 2 {
				{
					let mut borrow = self.data.data.borrow_mut();
					borrow.ref_index(self.offset - 2);
				}
			}
			Self {
				data: self.data.clone(),
				offset: self.offset - 1,
			}
		}
	}
	fn len(&self) -> usize {
		match &self.data.next {
			Some(v) => self.offset + v.len(),
			None => self.offset,
		}
	}
	fn iter<'a>(&'a self) -> ListIter<'a, T> {
		ListIter {
			_start: self,
			index: self.clone()
		}
	}
	fn debug(&self) -> (&Rc<ListData<T>>, usize) {
		(&self.data, self.offset)
	}
}

impl<T> Clone for List<T> {
	fn clone(&self) -> Self {
		let data = self.data.clone();
		if self.offset > 0 {
			{
				let mut borrow = data.data.borrow_mut();
				borrow.ref_index(self.offset - 1);
			}
		}
		Self {
			data, offset: self.offset,
		}
	}
}

impl<T> Drop for List<T> {
	fn drop(&mut self) {
		if self.offset > 0 {
			{
				let mut borrow = self.data.data.borrow_mut();
				borrow.deref_index(self.offset - 1);
			}
		}
	}
}

impl<T: std::fmt::Debug> std::fmt::Debug for List<T> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_list().entries(self.iter()).finish()
	}
}

struct ListIter<'a, T> {
	index: List<T>,
	_start: &'a List<T>,
}

impl<'a, T: std::fmt::Debug> Iterator for ListIter<'a, T> {
	type Item = &'a T;
	fn next(&mut self) -> Option<&'a T> {
		// eprint!("{:?}", self.index.debug());
		if self.index.len() == 0 {
			None
		} else {
			let car = self.index.car() as *const T;
			// SAFETY: the existence of self._start ensures that this reference remains valid until the end of 'a
			let res = unsafe {
				car.as_ref().unwrap()
				// technically we could just return .as_ref() without unwrapping but a failed cast is still a failure condition so \shrug
			};
			self.index = self.index.cdr();
			Some(res)
		}
	}
}

mod tests {
	use super::List;
	#[test]
	fn test() {
		let test = List::new();
		dbg!(&test);
		let test = test.cons(1);
		dbg!(&test);
		let test = test.cons(2);
		dbg!(&test);
		let test2 = test.clone();
		let test = test.cons(3);
		let test = test.cons(4);
		// dbg!(test.debug());
		dbg!(&test);
		dbg!(&test2);
		let test2 = test2.cons(1);
		let test2 = test2.cons(0);
		// dbg!(test2.debug());
		dbg!(&test2);
	}
}
