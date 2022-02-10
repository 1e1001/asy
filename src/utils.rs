use std::iter;
use std::ops::{RangeBounds, Bound};

pub fn safe_string(t: &str) -> String {
  let mut out = String::new();
  for ch in t.chars() {
	match ch {
	  '\\'|'*'|'_'|'`'|'>'|'<'|'~'|'|' => {
		out.push('\\');
	  },
	  _ => {}
	}
	out.push(ch);
  }
  out
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


mod tests {
	use crate::utils::{format_arg_range, range_to_dual};

	#[test]
	fn arg_range_tests() {
		assert_eq!(format_arg_range(range_to_dual(0..)), "at least 0 arguments");
		assert_eq!(format_arg_range(range_to_dual(1..)), "at least 1 argument");
		assert_eq!(format_arg_range(range_to_dual(2..)), "at least 2 arguments");
		assert_eq!(format_arg_range(range_to_dual(..1)), "0 arguments");
		assert_eq!(format_arg_range(range_to_dual(..2)), "0 – 1 argument");
		assert_eq!(format_arg_range(range_to_dual(..3)), "0 – 2 arguments");
		assert_eq!(format_arg_range(range_to_dual(..=1)), "0 – 1 argument");
		assert_eq!(format_arg_range(range_to_dual(..=2)), "0 – 2 arguments");
		assert_eq!(format_arg_range(range_to_dual(..=3)), "0 – 3 arguments");
		assert_eq!(format_arg_range(range_to_dual(0..=0)), "0 arguments");
		assert_eq!(format_arg_range(range_to_dual(0..=1)), "0 – 1 arguments");
		assert_eq!(format_arg_range(range_to_dual(0..=2)), "0 – 2 arguments");
		assert_eq!(format_arg_range(range_to_dual(1..=1)), "1 argument");
		assert_eq!(format_arg_range(range_to_dual(1..=2)), "1 – 2 arguments");
		assert_eq!(format_arg_range(range_to_dual(1..=3)), "1 – 3 arguments");
		assert_eq!(format_arg_range(range_to_dual(2..=2)), "2 arguments");
		assert_eq!(format_arg_range(range_to_dual(0..1)), "0 arguments");
		assert_eq!(format_arg_range(range_to_dual(0..2)), "0 – 1 arguments");
		assert_eq!(format_arg_range(range_to_dual(0..3)), "0 – 2 arguments");
		assert_eq!(format_arg_range(range_to_dual(1..2)), "1 argument");
		assert_eq!(format_arg_range(range_to_dual(1..3)), "1 – 2 arguments");
		assert_eq!(format_arg_range(range_to_dual(1..4)), "1 – 3 arguments");
		assert_eq!(format_arg_range(range_to_dual(2..3)), "2 arguments");
	}
}
