use crate::strmap::MappedStr;

use super::error::{AsylResult, AsylInSpan, AsylSpan, AsylError, AsylErrorType, merge_span};
use super::expr::{AsylExpr, AsylExprValue};
use super::scope::AsylEnv;



#[derive(Debug)]
pub enum AsylTokenValue {
	Symbol(MappedStr),
	String(MappedStr),
	Paren(bool),
}

pub struct AsylToken(AsylTokenValue, AsylSpan);

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

pub fn tokenize(env: &mut AsylEnv, file_name: MappedStr, data: MappedStr) -> AsylResult<Vec<AsylToken>> {
	let mut line = 1;
	let mut col = 1;
	let mut iter = data.get_ref().chars().enumerate().map(|(i, v)| {
		let res = (AsylInSpan(i, line, col, 1, file_name.clone(), data.clone()), v);
		match v {
			'\n' => { col = 1; line += 1; },
			_ => { col += 1; }
		}
		res
	}).peekable();
	let mut out = vec![];
	loop {
		let (chi, ch) = match iter.next() {
			Some(v) => (v.0, char_type(v.1)),
			None => break,
		};
		match ch {
			CharType::Paren(v) => out.push(AsylToken(AsylTokenValue::Paren(v), Some(chi))),
			CharType::Comment => {
				// discord until eol or eof
				while match iter.next() {
					None => false,
					Some((_, '\n')) => false,
					Some(_) => true,
				} {}
			},
			CharType::StartQuote(t) => {
				let end = t.other_side();
				// string parsing
				let mut res = String::new();
				let mut len = 1;
				loop {
					len += 1;
					match iter.next() {
						None => return Err(AsylError(AsylErrorType::UnexpectedEOF, None)),
						Some((chi, ch)) => match ch {
							// escapes
							// shorthands: \n \t \r \a \e \0
							// codes:      \xHH \{code}
							// literals:   \#
							'\\' => {
								len += 1;
								match iter.next().and_then(|v| Some(v.1)) {
									None => return Err(AsylError(AsylErrorType::UnexpectedEOF, None)),
									Some('n') => res.push('\n'),
									Some('t') => res.push('\t'),
									Some('r') => res.push('\r'),
									Some('a') => res.push('\x07'),
									Some('e') => res.push('\x1b'),
									Some('0') => res.push('\x00'),
									Some('x') => {
										let mut t = String::new();
										len += 2;
										t.push(iter.next().ok_or(AsylError(AsylErrorType::UnexpectedEOF, None))?.1);
										t.push(iter.next().ok_or(AsylError(AsylErrorType::UnexpectedEOF, None))?.1);
										let mut buf = [0u8; 1];
										hex::decode_to_slice(&t, &mut buf).or_else(|_| Err(AsylError(AsylErrorType::InvalidEscape(format!("\\x{}", t)), Some(chi.with_len(4)))))?;
										res.push(char::from(buf[0]));
									},
									Some('{') => {
										// unicode parsing :(
										let mut esc = String::new();
										loop { // 3rd level nested loop :)
											if esc.len() > 8 { return Err(AsylError(AsylErrorType::InvalidEscape(format!("\\{{{}…", esc)), Some(chi.with_len(2 + esc.len())))); }
											len += 1;
											match iter.next() {
												// 12 levels of indent oh no
												None => return Err(AsylError(AsylErrorType::UnexpectedEOF, None)),
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
										hex::decode_to_slice(&esc, &mut buf).or_else(|_| Err(AsylError(AsylErrorType::InvalidEscape(format!("\\{{{}}}", esc)), Some(chi.clone().with_len(2 + esc.len())))))?;
										res.push(char::from_u32(u32::from_be_bytes(buf)).ok_or_else(|| AsylError(AsylErrorType::InvalidEscape(format!("\\{{{}}}", esc)), Some(chi.clone().with_len(2 + esc.len()))))?);
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
				out.push(AsylToken(AsylTokenValue::String(env.map.add(res)), Some(chi.with_len(len))));
			},
			CharType::Other(ch) => {
				// consume until it's not other
				let mut res = String::from(ch);
				let mut len = 1;
				loop {
					match iter.peek() {
						None => break,
						Some(v) => match char_type(v.1) {
							CharType::Other(ch) => res.push(ch),
							_ => break,
						}
					}
					len += 1;
					// we know this value so we discard it
					iter.next();
				}
				out.push(AsylToken(AsylTokenValue::Symbol(env.map.add(res)), Some(chi.with_len(len))));
			},
			CharType::Whitespace => {},
		}
	}
	Ok(out)
}

pub fn parse_all(tokens: &[AsylToken], env: &mut AsylEnv) -> AsylResult<Vec<AsylExpr>> {
	let mut out = vec![];
	let mut rest = tokens;
	while rest.len() > 0 {
		let (token, new_rest) = parse(rest, env)?;
		out.push(token);
		rest = new_rest;
	}
	Ok(out)
}

fn parse<'a>(tokens: &'a [AsylToken], env: &mut AsylEnv) -> AsylResult<(AsylExpr, &'a [AsylToken])> {
	let (token, rest) = tokens.split_first().ok_or(AsylError(AsylErrorType::UnexpectedEOF, None))?;
	match &token.0 {
		AsylTokenValue::Paren(true) => read_seq(token.1.clone(), rest, env),
		AsylTokenValue::Paren(false) => {
			Err(AsylError(AsylErrorType::UnexpectedCloseParen, token.1.clone()))
		},
		AsylTokenValue::String(data) => Ok((AsylExpr(AsylExprValue::String(data.clone()), token.1.clone(), None), rest)),
		AsylTokenValue::Symbol(data) => if is_number(data.get_ref().as_bytes()) {
			Ok((parse_number(data.get_ref(), token.1.clone())?, rest))
		} else {
			Ok((AsylExpr(AsylExprValue::Symbol(data.clone()), token.1.clone(), None), rest))
		},
	}
}

fn read_seq<'a>(start: AsylSpan, tokens: &'a [AsylToken], env: &mut AsylEnv) -> AsylResult<(AsylExpr, &'a [AsylToken])> {
	let mut res = vec![];
	let mut xs = tokens;
	loop {
		let (token, rest) = xs.split_first().ok_or(AsylError(AsylErrorType::UnexpectedEOF, None))?;
		match token.0 {
			AsylTokenValue::Paren(false) => return Ok((AsylExpr(AsylExprValue::List(res), merge_span(start, token.1.clone()), None), rest)),
			_ => {
				let (exp, new_xs) = parse(&xs, env)?;
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

fn parse_number(data: &str, span: AsylSpan) -> AsylResult<AsylExpr> {
	let asyl_error = || AsylError(AsylErrorType::InvalidNumber(data.to_string()), span.clone());
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
						return Err(asyl_error());
					}
					int *= base as u64;
					int += char_val(o, base).ok_or_else(asyl_error)? as u64;
					int_len += 1;
				},
			},
			ParseState::Frac => match ch {
				'_'|',' => {},
				'e' => state = ParseState::ExpSign,
				o => {
					if frac_len >= 20 {
						return Err(asyl_error());
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
						return Err(asyl_error());
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
		ParseState::IntSign => Err(asyl_error()),
		ParseState::IntBase => Ok(AsylExpr(AsylExprValue::Int(0), span, None)),
		ParseState::Int => if int_len > 0 {
			if int_neg {
				if int <= 0x1000000000000000u64 {
					Ok(AsylExpr(AsylExprValue::Int(-(int as i64)), span, None))
				} else {
					Err(asyl_error())
				}
			} else if int < 0x1000000000000000u64 {
				Ok(AsylExpr(AsylExprValue::Int(int as i64), span, None))
			} else {
				Err(asyl_error())
			}
		} else {
			Err(asyl_error())
		},
		ParseState::Frac => if int_len > 0{
			Ok(AsylExpr(AsylExprValue::Float((int as f64) + (frac as f64) / (10.0f64.powi(frac_len as i32))), span, None))
		} else {
			Err(asyl_error())
		},
		ParseState::ExpSign => Err(asyl_error()),
		ParseState::Exp => if int_len > 0 && exp_len > 0 {
			let frac = (int as f64) + (frac as f64) / (10.0f64.powi(frac_len as i32));
			Ok(AsylExpr(AsylExprValue::Float(frac * 10.0f64.powi(exp as i32)), span, None))
		} else {
			Err(asyl_error())
		}
	}
}
