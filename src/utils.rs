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
