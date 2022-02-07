//! lisp :)
//!
//! a lot of this is taken from risp (https://stopa.io/post/222)
//!
//! there are a few notable differences though, the main ones
//! being that it's styled like racket and lazily evaluates

use std::{collections::HashMap, io, str::Chars};

#[derive(Debug, Clone)]
enum AsylExpr {
	Symbol(String),
	String(String),
	Float(f64),
	Int(i64),
	Uint(u64),
	Bool(bool),
	List(Vec<AsylExpr>),
	ExtFn(fn(&[AsylExpr]) -> Result<AsylExpr, AsylError>),
}

#[derive(Debug)]
enum AsylError {
	Other(String),
	UnexpectedEOF,
	InvalidEscape,
	UnexpectedCloseParen,
	InvalidNumber,
}

type AsylResult<T> = Result<T, AsylError>;

enum AsylEnvEntry {
	// probably also some scope information here
	Lazy(AsylExpr),
	Evaluated(AsylExpr),
}

#[derive(Clone)]
struct AsylEnv {
	data: HashMap<String, AsynEnvEntry>,
}

enum AsylToken {
	Symbol(String),
	String(String),
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

fn parse<'a>(tokens: &'a [AsylToken]) -> AsylResult<(AsylExpr, &'a [AsylToken])> {
	let (token, rest) = tokens.split_first().ok_or(AsylError::UnexpectedEOF)?;
	match token {
		AsylToken::Paren(true) => read_seq(rest),
		AsylToken::Paren(false) => {
			Err(AsylError::UnexpectedCloseParen)
		},
		token => Ok((parse_atom(token)?, rest)),
	}
}

fn read_seq<'a>(tokens: &'a [AsylToken]) -> AsylResult<(AsylExpr, &'a [AsylToken])> {
	let mut res = vec![];
	let mut xs = tokens;
	loop {
		let (token, rest) = xs.split_first().ok_or(AsylError::UnexpectedEOF)?;
		match token {
			AsylToken::Paren(false) => return Ok((AsylExpr::List(res), rest)),
			_ => {
				let (exp, new_xs) = parse(&rest)?;
				res.push(exp);
				xs = new_xs;
			}
		}
	}
}

fn is_number(data: &str) -> bool {
	match data[0] {
		'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' => true,
		'-'|'+' => {
			if data.len() > 0 {
				is_number(data[1..])
			} else {
				false
			}
		}, _ => false,
	}
}

fn char_val(c: char, radix: u8) -> AsylResult<u8> {
	let val = match c {
		'0' => 0,
		'1' => 1,
		'2' => 2,
		'3' => 3,
		'4' => 4,
		'5' => 5,
		'6' => 6,
		'7' => 7,
		'8' => 8,
		'9' => 9,
		'a'|'A' => 10,
		'b'|'B' => 11,
		'c'|'C' => 12,
		'd'|'D' => 13,
		'e'|'E' => 14,
		'f'|'F' => 15,
		_ => return Err(AsylError::InvalidNumber)
	};
	if val < radix {Ok(val)} else {Err(AsylError::InvalidNumber)}
}

fn parse_number(data: &str) -> AsylResult<AsylExpr> {
	// i hope we can put it here
	enum ParseState {
		IntSign, IntBase, Int, Frac, ExpSign, Exp,
	}
	let mut state = ParseState::IntSign;
	let mut base = 10;
	let mut base_max = 20;
	let mut int = 0u64;
	let mut int_len = 0u8;
	let mut int_neg = false;
	let mut frac = 0u64;
	let mut frac_len = 0u8;
	let mut exp = 0u64;
	let mut exp_len = 0u8;
	let mut exp_neg = false;
	// i could rewrite this as a consume-y thingy like the tokenizer but i dont feel like it
	for ch in data.chars() {
		match ParseState {
			ParseState::IntSign => match ch {
				'+' => {},
				'-' => int_neg = !int_neg;
				'0' => state = ParseState::IntBase,
				o => {
					state = ParseState::Int;
					int = char_val(o, base)? as u64;
					int_len = 1;
				},
			},
			ParseState::IntBase => match ch {
				'x' => {
					state = ParseState::Int;
					base = 16;
					base_max = 16;
				},
				'b' => {
					state = ParseState::Int;
					base = 2;
					base_max = 64;
				},
				o => {
					state = ParseState::Int;
					int = char_val(o, base)? as u64;
					int_len = 1;
				},
			},
			ParseState::Int => match ch {
				'_'|',' => {},
				'.' if base == 10 => state = ParseState::Frac,
				'e'|'E' if base == 10 => state = ParseState::ExpSign,
				o => {
					if int_len >= base_max {
						return Err(AsylError::InvalidNumber);
					}
					int <<= base;
					int |= char_val(o, base)? as u64;
					int_len += 1;
				},
			},
			ParseState::Frac => match ch {
				'_'|',' => {},
				'e' => state = ParseState::ExpSign,
				o => {
					if frac_len >= 20 {
						return Err(AsylError::InvalidNumber);
					}
					frac <<= 10;
					frac |= char_val(o, 10)? as u64;
					frac_len += 1;
				},
			},
			ParseState::ExpSign => match ch {
				'+' => {},
				'-' => exp_neg = !exp_neg,
				'_'|',' => state = ParseState::Exp,
				o => {
					state = ParseState::Exp;
					exp = char_val(o, 10)? as u64;
					exp_len = 1;
				}
			},
			ParseState::Exp => match ch {
				'_'|',' => {},
				o => {
					if exp_len >= 4 {
						return Err(AsylError::InvalidNumber);
					}
					exp <<= 10;
					exp |= char_val(o, 10)? as u64;
					exp_len += 1;
				},
			},
		}
	}
	// and now, every sanity check in existance
	match state {
		ParseState::IntSign => Err(AsylError::InvalidNumber),
		ParseState::IntBase => Ok(AsylExpr::Uint(0)),
		ParseState::Int => if int_len > 0 {
			if int_neg {
				if int <= 0x1000000000000000u64 {
					AsylExpr::Int(-int as i64)
				} else {
					Err(AsylError::InvalidNumber)
				}
			} else {
				AsylExpr::Uint(int)
			}
		} else {
			Err(AsylError::InvalidNumber)
		},
		ParseState::Frac => if int_len > 0{
			Ok(AsylExpr::Float((int as f64) + (frac as f64) / (10.0.powi(frac_len))))
		} else {
			Err(AsylError::InvalidNumber)
		},
		ParseState::ExpSign => Err(AsylError::InvalidNumber),
		ParseState::Exp => if int_len > 0 && exp_len > 0 {
			let frac = (int as f64) + (frac as f64) / (10.0.powi(frac_len));
			Ok(AsylExpr::Float(frac * 10.0.powi(exp)))
		}
	}
}

fn parse_atom(token: AsylToken) -> AsylResult<AsylExpr> {
	match token {
		AsylToken::String(data) => AsylExpr::String(data),
		AsylToken::Symbol(data) => if is_number(&data) {
			parse_number(&data)
		} else {
			Ok(AsylExpr::Symbol(data))
		}
	}
}