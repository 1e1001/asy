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
    let (start, end, end_exclude) = match r.start_bound() {
        Bound::Excluded(_) => panic!("invalid range"),
        Bound::Included(start) => match r.end_bound() {
            Bound::Excluded(end) => (Some(*start), Some(*end), true),
            Bound::Included(end) => (Some(*start), Some(end + 1), false),
            Bound::Unbounded => (Some(*start), None, false),
        },
        Bound::Unbounded => match r.end_bound() {
            Bound::Excluded(end) => (None, Some(*end), true),
            Bound::Included(end) => (None, Some(end + 1), false),
            Bound::Unbounded => (None, None, false),
        },
    };
    match start {
        Some(start) => match end {
            Some(end) => if start == end {
                format!("{} argument{}", start, if start == 1 {""} else {"s"})
            } else {
                format!("{} to {} arguments", start, end - 1)
            },
            None => format!("at least {} argument{}", start, if start == 1 {""} else {"s"}),
        },
        None => match end {
            Some(end) => if end_exclude {
				if end == 1 {
					format!("0 arguments")
				} else {
					format!("less than {} argument{}", end, if end == 1 {""} else {"s"})
				}
			} else {
				format!("at most {} argument{}", end - 1, if end == 2 {""} else {"s"})
			},
            None => "any number of arguments".to_string(),
        },
    }
}