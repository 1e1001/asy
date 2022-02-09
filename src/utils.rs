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

pub fn print_arg_range(r: impl RangeBounds<usize>) -> String {
    let (start, end) = match r.start_bound() {
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
    };
    match start {
        Some(start) => match end {
            Some(end) => if start == end {
                format!("{} argument{}", start, if start == 1 {""} else {"s"})
            } else {
                format!("{} to {} arguments", start, end)
            },
            None => format!("at least {} argument{}", start, if start == 1 {""} else {"s"}),
        },
        None => match end {
            Some(end) => format!("at most {} argument{}", end, if end == 1 {""} else {"s"}),
            None => "any number of arguments".to_string(),
        },
    }
}