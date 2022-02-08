//! lisp :)
//!
//! a lot of this is taken from risp (https://stopa.io/post/222)
//!
//! there are a few notable differences though, the main ones
//! being that it's styled like racket and (hopefully) lazily evaluates

use std::collections::HashMap;
use std::{error, fmt};

use crate::utils::safe_string;

#[derive(Clone)]
pub enum AsylExpr {
	Symbol(String),
	String(String),
	Float(f64),
	Int(i64),
	Uint(u64),
	Bool(bool),
	List(Vec<AsylExpr>),
	ExtFn(fn(&[AsylExpr]) -> Result<AsylExpr, AsylError>),
}

impl fmt::Display for AsylExpr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_str(&match self {
			AsylExpr::Symbol(v) => v.clone(),
			AsylExpr::String(v) => v.clone(),
			AsylExpr::Float(v) => v.to_string(),
			AsylExpr::Int(v) => v.to_string(),
			AsylExpr::Uint(v) => v.to_string(),
			AsylExpr::Bool(v) => if *v {"true"} else {"false"}.to_string(),
			AsylExpr::List(v) => format!("({})", v.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(" ")),
			AsylExpr::ExtFn(v) => "<ExtFn>".to_string(),
		})
	}
}

#[derive(Debug)]
pub enum AsylError {
	UnexpectedEOF,
	InvalidEscape(String),
	UnexpectedCloseParen(usize),
	InvalidNumber(String),
	ExpectedNumber,
	ConvertFailure,
	ArgMismatch(String),
}

impl fmt::Display for AsylError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_str(&match self {
			AsylError::UnexpectedEOF => "Unexpected End-of-file in input".to_string(),
			AsylError::InvalidEscape(text) => format!("Invalid escape sequence `{}`", safe_string(text)),
			AsylError::UnexpectedCloseParen(index) => format!("Unexpected `)` at position {}", index),
			AsylError::InvalidNumber(text) => format!("Invalid number `{}`", safe_string(text)),
			AsylError::ExpectedNumber => "Expected number".to_string(),
			AsylError::ConvertFailure => "Conversion Failure".to_string(),
			AsylError::ArgMismatch(text) => format!("Argument mismatch, expected {}", text),
		})
	}
}

impl error::Error for AsylError {}

type AsylResult<T> = Result<T, AsylError>;

#[derive(Clone)]
pub enum AsylEnvEntry {
	// probably also some scope information here
	Lazy(AsylExpr),
	Evaluated(AsylExpr),
}

#[derive(Clone)]
pub struct AsylEnv {
	data: HashMap<String, AsylEnvEntry>,
}

enum UniformNumberList {
	Float(Vec<f64>),
	Int(Vec<i64>),
	Uint(Vec<u64>),
}

fn to_uniform_number_list(list: &[AsylExpr]) -> AsylResult<UniformNumberList> {
	enum NumberType {
		Float, Int, Uint
	}
	fn number_type(v: &AsylExpr) -> AsylResult<NumberType> {
		Ok(match v {
			AsylExpr::Float(_) => NumberType::Float,
			AsylExpr::Int(_) => NumberType::Int,
			AsylExpr::Uint(_) => NumberType::Uint,
			_ => return Err(AsylError::ExpectedNumber),
		})
	}
	macro_rules! res {
		(dir, $v:expr, $typ:ty) => {$v};
		(cas, $v:expr, $typ:ty) => {$v as $typ};
	}
	macro_rules! collect {
		($typ:ty, $list:expr, $float:tt $int:tt $uint:tt) => {{
			let mut out = vec![];
			for i in &$list[1..] {
				out.push(match i {
					AsylExpr::Float(v) => res!($float, *v, $typ),
					AsylExpr::Int(v)   => res!($int,   *v, $typ),
					AsylExpr::Uint(v)  => res!($uint,  *v, $typ),
					_ => return Err(AsylError::ExpectedNumber)
				});
			}
			Ok(out)
		}}
	}
	if list.len() == 0 {
		return Ok(UniformNumberList::Uint(vec![]))
	} else if list.len() == 1 {
		return match list[0] {
			AsylExpr::Float(v) => Ok(UniformNumberList::Float(vec![v])),
			AsylExpr::Int(v)   => Ok(UniformNumberList::Int(vec![v])),
			AsylExpr::Uint(v)  => Ok(UniformNumberList::Uint(vec![v])),
			_ => Err(AsylError::ExpectedNumber),
		}
	}
	Ok(match list[0] {
		AsylExpr::Float(_) => UniformNumberList::Float(collect!(f64, list, dir cas cas)?),
		AsylExpr::Int(_) => UniformNumberList::Int(collect!(i64, list,   cas dir cas)?),
		AsylExpr::Uint(_) => UniformNumberList::Uint(collect!(u64, list,  cas cas dir)?),
		_ => return Err(AsylError::ExpectedNumber),
	})
}

pub fn default_env() -> AsylEnv {
	let mut data = HashMap::new();
	data.insert("+".to_string(), AsylEnvEntry::Evaluated(AsylExpr::ExtFn(|args| {
		Ok(match to_uniform_number_list(args)? {
			UniformNumberList::Float(v) => AsylExpr::Float(v.iter().fold(0.0, |a, b| a + b)),
			UniformNumberList::Int(v)   => AsylExpr::Int(  v.iter().fold(0, |a, b| a + b)),
			UniformNumberList::Uint(v)  => AsylExpr::Uint( v.iter().fold(0, |a, b| a + b)),
		})
	})));
	data.insert("-".to_string(), AsylEnvEntry::Evaluated(AsylExpr::ExtFn(|args| {
		if args.len() < 2 {
			return Err(AsylError::ArgMismatch("at least two arguments".to_string()))
		}
		Ok(match to_uniform_number_list(args)? {
			UniformNumberList::Float(v) => AsylExpr::Float(v[0] - v[1..].iter().fold(0.0, |a, b| a + b)),
			UniformNumberList::Int(v)   => AsylExpr::Int(  v[0] - v[1..].iter().fold(0, |a, b| a + b)),
			UniformNumberList::Uint(v)  => AsylExpr::Uint( v[0] - v[1..].iter().fold(0, |a, b| a + b)),
		})
	})));
	AsylEnv { data }
}

#[derive(Debug)]
pub enum AsylToken {
	Symbol(String),
	String(String),
	Paren(bool),
}

#[derive(Debug)]
pub struct AsylTraceToken(AsylToken, usize);

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
pub fn tokenize(data: &str) -> AsylResult<Vec<AsylTraceToken>> {
	let mut iter = data.chars().enumerate().peekable();
	let mut out = vec![];
	loop {
		let (chi, ch) = match iter.next() {
			Some(v) => (v.0, char_type(v.1)),
			None => break,
		};
		match ch {
			CharType::Paren(v) => out.push(AsylTraceToken(AsylToken::Paren(v), chi)),
			CharType::Comment => {
				// discord until eol or eof
				while match iter.next() {
					None => true,
					Some((_, '\n')) => false,
					Some(_) => true,
				} {}
			},
			CharType::StartQuote(t) => {
				let end = t.other_side();
				// string parsing
				let mut res = String::new();
				loop {
					match iter.next() {
						None => return Err(AsylError::UnexpectedEOF),
						Some((_, ch)) => match ch {
							// escapes
							// shorthands: \n \t \r \a \e \0
							// codes:      \xHH \{code}
							// literals:   \#
							'\\' => match iter.next().and_then(|v| Some(v.1)) {
								None => return Err(AsylError::UnexpectedEOF),
								Some('n') => res.push('\n'),
								Some('t') => res.push('\t'),
								Some('r') => res.push('\r'),
								Some('a') => res.push('\x07'),
								Some('e') => res.push('\x1b'),
								Some('0') => res.push('\x00'),
								Some('x') => {
									let mut t = String::new();
									t.push(iter.next().ok_or(AsylError::UnexpectedEOF)?.1);
									t.push(iter.next().ok_or(AsylError::UnexpectedEOF)?.1);
									let mut buf = [0u8; 1];
									hex::decode_to_slice(&t, &mut buf).or_else(|_| Err(AsylError::InvalidEscape(format!("\\x{}", t))))?;
									res.push(char::from(buf[0]));
								},
								Some('{') => {
									// unicode parsing :(
									let mut esc = String::new();
									loop { // 3rd level nested loop :)
										if esc.len() > 8 { return Err(AsylError::InvalidEscape(format!("\\{{{}", esc))); }
										match iter.next() {
											None => return Err(AsylError::UnexpectedEOF),
											Some((_, '}')) => break,
											Some((_, ch)) => esc.push(ch),
										}
									}
									// append 0's so it's 8 byte in size
									while esc.len() < 8 {
										esc.insert(0, '0');
									}
									// “`char` is always four bytes in size.”
									let mut buf = [0u8; 4];
									hex::decode_to_slice(&esc, &mut buf).or_else(|_| Err(AsylError::InvalidEscape(format!("\\{{{}", esc))))?;
									res.push(char::from_u32(u32::from_be_bytes(buf)).ok_or_else(|| AsylError::InvalidEscape(format!("\\{{{}", esc)))?);
								},
								Some(tc) => res.push(tc),
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
				out.push(AsylTraceToken(AsylToken::String(res), chi));
			},
			CharType::Other(ch) => {
				// consume until it's not other
				let mut res = String::from(ch);
				loop {
					match iter.peek() {
						None => break,
						Some(v) => match char_type(v.1) {
							CharType::Other(ch) => res.push(ch),
							_ => break,
						}
					}
					// we know this value so we discard it
					iter.next();
				}
				out.push(AsylTraceToken(AsylToken::Symbol(res), chi));
			},
			CharType::Whitespace => {},
		}
	}
	Ok(out)
}

pub fn parse<'a>(tokens: &'a [AsylTraceToken]) -> AsylResult<(AsylExpr, &'a [AsylTraceToken])> {
	let (token, rest) = tokens.split_first().ok_or(AsylError::UnexpectedEOF)?;
	match &token.0 {
		AsylToken::Paren(true) => read_seq(rest),
		AsylToken::Paren(false) => {
			Err(AsylError::UnexpectedCloseParen(token.1))
		},
		AsylToken::String(data) => Ok((AsylExpr::String(data.clone()), rest)),
		AsylToken::Symbol(data) => if is_number(data.as_bytes()) {
			Ok((parse_number(&data)?, rest))
		} else {
			Ok((AsylExpr::Symbol(data.clone()), rest))
		},
	}
}

fn read_seq<'a>(tokens: &'a [AsylTraceToken]) -> AsylResult<(AsylExpr, &'a [AsylTraceToken])> {
	let mut res = vec![];
	let mut xs = tokens;
	loop {
		let (token, rest) = xs.split_first().ok_or(AsylError::UnexpectedEOF)?;
		match token.0 {
			AsylToken::Paren(false) => return Ok((AsylExpr::List(res), rest)),
			_ => {
				let (exp, new_xs) = parse(&xs)?;
				res.push(exp);
				xs = new_xs;
			}
		}
	}
}

fn is_number(data: &[u8]) -> bool {
	match data[0] {
		b'0'|b'1'|b'2'|b'3'|b'4'|b'5'|b'6'|b'7'|b'8'|b'9' => true,
		b'-'|b'+' => {
			if data.len() > 1 {
				is_number(&data[1..])
			} else {
				false
			}
		},
		_ => false,
	}
}

fn char_val(c: char, radix: u8) -> Option<u8> {
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
		_ => return None
	};
	if val < radix {Some(val)} else {None}
}

fn parse_number(data: &str) -> AsylResult<AsylExpr> {
	let asyl_error = || AsylError::InvalidNumber(data.to_string());
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
		match state {
			ParseState::IntSign => match ch {
				'+' => {},
				'-' => int_neg = !int_neg,
				'0' => state = ParseState::IntBase,
				o => {
					state = ParseState::Int;
					int = char_val(o, base).ok_or_else(asyl_error)? as u64;
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
					int = char_val(o, base).ok_or_else(asyl_error)? as u64;
					int_len = 1;
				},
			},
			ParseState::Int => match ch {
				'_'|',' => {},
				'.' if base == 10 => state = ParseState::Frac,
				'e'|'E' if base == 10 => state = ParseState::ExpSign,
				o => {
					if int_len >= base_max {
						return Err(AsylError::InvalidNumber(data.to_string()));
					}
					int *= base as u64;
					int |= char_val(o, base).ok_or_else(asyl_error)? as u64;
					int_len += 1;
				},
			},
			ParseState::Frac => match ch {
				'_'|',' => {},
				'e' => state = ParseState::ExpSign,
				o => {
					if frac_len >= 20 {
						return Err(AsylError::InvalidNumber(data.to_string()));
					}
					frac *= 10;
					frac |= char_val(o, 10).ok_or_else(asyl_error)? as u64;
					frac_len += 1;
				},
			},
			ParseState::ExpSign => match ch {
				'+' => {},
				'-' => exp_neg = !exp_neg,
				'_'|',' => state = ParseState::Exp,
				o => {
					state = ParseState::Exp;
					exp = char_val(o, 10).ok_or_else(asyl_error)? as u64;
					exp_len = 1;
				}
			},
			ParseState::Exp => match ch {
				'_'|',' => {},
				o => {
					if exp_len >= 4 {
						return Err(AsylError::InvalidNumber(data.to_string()));
					}
					exp *= 10;
					exp |= char_val(o, 10).ok_or_else(asyl_error)? as u64;
					exp_len += 1;
				},
			},
		}
	}
	// and now, every sanity check in existance
	match state {
		ParseState::IntSign => Err(AsylError::InvalidNumber(data.to_string())),
		ParseState::IntBase => Ok(AsylExpr::Uint(0)),
		ParseState::Int => if int_len > 0 {
			if int_neg {
				if int <= 0x1000000000000000u64 {
					Ok(AsylExpr::Int(-(int as i64)))
				} else {
					Err(AsylError::InvalidNumber(data.to_string()))
				}
			} else {
				Ok(AsylExpr::Uint(int))
			}
		} else {
			Err(AsylError::InvalidNumber(data.to_string()))
		},
		ParseState::Frac => if int_len > 0{
			Ok(AsylExpr::Float((int as f64) + (frac as f64) / (10.0f64.powi(frac_len as i32))))
		} else {
			Err(AsylError::InvalidNumber(data.to_string()))
		},
		ParseState::ExpSign => Err(AsylError::InvalidNumber(data.to_string())),
		ParseState::Exp => if int_len > 0 && exp_len > 0 {
			let frac = (int as f64) + (frac as f64) / (10.0f64.powi(frac_len as i32));
			Ok(AsylExpr::Float(frac * 10.0f64.powi(exp as i32)))
		} else {
			Err(AsylError::InvalidNumber(data.to_string()))
		}
	}
}
