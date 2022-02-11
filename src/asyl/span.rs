
// char, line, col, length, name, content
#[derive(Debug, Clone)]
pub struct AsylInSpan(usize, usize, usize, usize, MappedStr, MappedStr);

impl AsylInSpan {
	pub fn print(&self, span: &AsylError) -> Result<String, fmt::Error> {
		let (source_name, source) = (&self.0.3, &self.0.4);
		let mut out = String::new();
		let mut cols_until_start = self.0.2 - 1;
		let mut line = self.0.1;
		let mut current_col = 1;
		writeln!(out, "\n       │")?;
		write!(out, "{:>6} │ ", line)?;
		let mut count = self.1;
		for (_, ch) in source.get_ref().chars().chain(iter::once('\n')).skip(self.0.0 + 1 - self.0.2).enumerate() {
			match ch {
				'\n' => {
					line += 1;
					write!(out, "\n       │ ")?;
					let p_cus = cols_until_start;
					for _ in 0..current_col.min(count + cols_until_start) {
						out.push(if cols_until_start > 0 {
							cols_until_start -= 1;
							' '
						} else {
							'~'
						});
					}
					if current_col > count { break }
					count -= current_col - p_cus;
					current_col = 1;
					write!(out, "\n{:>6} │ ", line)?;
				},
				ch => {
					current_col += 1;
					out.push(ch);
				},
			}
		}
		let mut prefixed = format!("```\n{}\n       ╭─[ {}:{} ]", span, source_name.get_ref(), self.0.1);
		out.push_str("\n       │");
		out.push_str("```");
		prefixed.push_str(&out);
		Ok(prefixed)
	}
}

pub type AsylSpan = Option<AsylInSpan>;
