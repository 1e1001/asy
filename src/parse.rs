use std::{collections::HashMap, io, str::Chars};

#[derive(Debug, Clone)]
enum AsylExpr {
	Symbol(String),
	String(String),
	Float(f64),
	Int(i64),
	UInt(u64),
	Bool(bool),
	List(Vec<AsylExpr>),
}

#[derive(Debug)]
enum AsylError {
	Other(String),
	UnexpectedEOF,
	InvalidEscape,
}

type AsylResult<T> = Result<T, AsylError>;

#[derive(Clone)]
struct AsylEnv {
	data: HashMap<String, AsylExpr>,
}

enum AsylToken {
	Symbol(String),
	String(String),
	Float(f64),
	Int(i64),
	UInt(u64),
	Paren(bool),
}

enum StartQuoteType {
	Quote,
	SlantQuote,
	LeftBracket,
	RightBracket,
}

impl StartQuoteType {
	fn other_side(&self) -> char {
		match self {
			Self::Quote        => '"',
			Self::SlantQuote   => '”',
			Self::LeftBracket  => '»',
			Self::RightBracket => '«',
		}
	}
}

enum CharType {
	StartQuote(StartQuoteType), // "" “” «» »«
	Whitespace,  // ' '
	Paren(bool), // true: (  false: )
	Comment,     // ;
	Other(char),
}

fn char_type(c: char) -> CharType {
	match c {
		// we MIGHT add [] or similar as a form of block comment
		'"' => CharType::StartQuote(StartQuoteType::Quote),
		'“' => CharType::StartQuote(StartQuoteType::SlantQuote),
		'«' => CharType::StartQuote(StartQuoteType::LeftBracket),
		'»' => CharType::StartQuote(StartQuoteType::RightBracket),
		' '|'\t'|'\r'|'\n' => CharType::Whitespace,
		'('|'['|'{' => CharType::Paren(true),
		')'|']'|'}' => CharType::Paren(false),
		';' => CharType::Comment,
		c => CharType::Other(c)
	}
}

// haven't tested this, good luck future me
fn tokenize(data: String) -> AsylResult<Vec<AsylToken>> {
	let iter = data.chars().peekable();
	let out = vec![];
	loop {
		let ch = match iter.next() {
			Some(v) => char_type(v),
			None => break,
		};
		match ch {
			CharType::Paren(v) => out.push(AsylToken::Paren(v)),
			CharType::Comment => {
				// discord until eol or eof
				while match iter.next() {
					None => true,
					Some('\n') => false,
					Some(_) => true,
				} {}
			},
			CharType::StartQuote(t) => {
				let end = t.other_side();
				// string parsing
				let res = String::new();
				loop {
					match iter.next() {
						None => return Err(AsylError::UnexpectedEOF),
						Some(ch) => match ch {
							'\\' => {
								// escapes
								// shorthands: \n \t \r \a \e \0
								// codes:      \xHH \{code}
								// literals:   \#
								let ty = match iter.next() {
									None => return Err(AsylError::UnexpectedEOF),
									Some('n') => res.push('\n'),
									Some('t') => res.push('\t'),
									Some('r') => res.push('\r'),
									Some('a') => res.push('\x07'),
									Some('e') => res.push('\x1b'),
									Some('0') => res.push('\x00'),
									Some('x') => {
										let t = [
											iter.next().ok_or(AsylError::UnexpectedEOF)?,
											iter.next().ok_or(AsylError::UnexpectedEOF)?,
										];
										let buf = [0u8; 1];
										hex::decode_to_slice(t, &mut buf).or(AsylError::InvalidEscape)?;
										res.push(char::from(buf[0]));
									},
									Some('{') => {
										// unicode parsing :(
										let esc = String::new();
										loop { // 3rd level nested loop :)
											if esc.len() > 8 { return Err(AsylError::InvalidEscape); }
											match iter.next() {
												None => return Err(AsylError::UnexpectedEOF),
												Some('}') => break,
												Some(ch) => esc.push(ch),
											}
										}
										// append 0's so it's 8 byte in size
										while esc.len() < 8 {
											esc.insert(0, '0');
										}
										// “`char` is always four bytes in size.”
										let buf = [0u8; 4];
										hex::decode_to_slice(t, &mut buf).or(AsylError::InvalidEscape)?;
										res.push(char::from_u32(u32::from_be_bytes(buf)).ok_or(AsylError::InvalidEscape));
									},
									Some(tc) => res.push(tc),
								}
							},
							ch => {
								if ch == end {
									break
								} else {
									res.push(ch);
								}
							},
						}
					}
				}
			},
			CharType::Other(ch) => {
				// consume until it's not other
				let res = String::from(ch);
				loop {
					match iter.peek() {
						None => break,
						Some(v) => match char_type(v) {
							CharType::Other(ch) => res.push(ch),
							_ => break,
						}
					}
					// we know this value so we discard it
					iter.next();
				}
				out.push(AsylToken::Symbol(ch));
			},
			CharType::Whitespace => {},
		}
	}
	Ok(out)
}
