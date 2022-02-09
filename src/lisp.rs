//! lisp :)
//!
//! a lot of this is taken from risp (https://stopa.io/post/222)
//!
//! there are a few notable differences though, the main ones
//! being that it's styled like racket and (hopefully) lazily evaluates

use std::collections::HashMap;
use std::fmt::Write;
use std::{error, fmt, iter};

use crate::utils::safe_string;

#[derive(Clone)]
pub enum AsylType {
	Symbol, String, Float, Int, Bool, Type, List, Fn
}
pub struct AsylLambda {
	// todo
	// i dont think these need to be rc'd but suuure
	// i can most likely get away with boxing these
	// i'll try that when i get home
	args: Rc<Vec<String>>,
	body: Rc<Spanned<AsylExpr>>,
}
// string interning could be nice here, probably implemented as an Rc<String>?
#[derive(Clone)]
pub enum AsylExpr {
	Symbol(String),
	String(String),
	Float(f64),
	Int(i64),
	Bool(bool),
	Type(AsylType),
	// todo
	// list structures for consideration:
	// - just a vec :(
	// - snap's hybrid linked/unlinked lists
	//   - maybe have contiguous spans of memory be represented as vecs
	//     a linked list of lists
	// - VList
	//   - probably a slightly better implementation of that snap hybrid list
	List(Vec<Spanned<AsylExpr>>),
	ExtFn(fn(AsylSpan, &mut AsylEnv, &[Spanned<AsylExpr>]) -> Result<Spanned<AsylExpr>, AsylError>),
	Lambda(AsylLambda),
}

impl AsylExpr {
	fn get_type(&self) -> AsylType {
		match self {
			AsylExpr::Symbol(_) => AsylType::Symbol,
			AsylExpr::String(_) => AsylType::String,
			AsylExpr::Float(_)  => AsylType::Float,
			AsylExpr::Int(_)    => AsylType::Int,
			AsylExpr::Bool(_)   => AsylType::Bool,
			AsylExpr::Type(_)   => AsylType::Type,
			AsylExpr::List(_)   => AsylType::List,
			AsylExpr::ExtFn(_)  => AsylType::Fn,
			AsylExpr::Lambda(_) => AsylType::Fn,
		}
	}
}

// char, line, col
#[derive(Debug, Copy, Clone)]
pub struct AsylPos(usize, usize, usize);
// start, len
#[derive(Debug, Copy, Clone)]
pub struct AsylInSpan(AsylPos, usize);
type AsylSpan = Option<AsylInSpan>;

impl AsylInSpan {
	pub fn print(&self, source_name: &str, source: &str) -> Result<String, fmt::Error> {
		let mut out = String::from("```");
		let mut cols_until_start = self.0.2 - 1;
		let mut line = self.0.1;
		let mut current_col = 1;
		writeln!(out, "{}:{}", source_name, self.0.1)?;
		write!(out, "{:>6} | ", line)?;
		let mut count = self.1;
		for (_, ch) in source.chars().chain(iter::once('\n')).skip(self.0.0 + 1 - self.0.2).enumerate() {
			match ch {
				'\n' => {
					line += 1;
					write!(out, "\n       | ")?;
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
					write!(out, "\n{:>6} | ", line)?;
				},
				ch => {
					current_col += 1;
					out.push(ch);
				},
			}
		}
		out.push_str("```");
		Ok(out)
	}
}

impl fmt::Display for AsylExpr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_str(&match self {
			AsylExpr::Symbol(v) => v.clone(),
			AsylExpr::String(v) => format!("{:?}", v),
			AsylExpr::Float(v) => v.to_string(),
			AsylExpr::Int(v) => v.to_string(),
			// technically 't and 'f are just variables but i don't care
			AsylExpr::Bool(v) => if *v {"'t"} else {"'f"}.to_string(),
			AsylExpr::List(v) => format!("({})", v.iter().map(|v| v.0.to_string()).collect::<Vec<_>>().join(" ")),
			AsylExpr::ExtFn(_) => "<proc>".to_string(),
			AsylExpr::Lambda(_) => "<proc>".to_string(),
		})
	}
}

impl fmt::Debug for AsylExpr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_fmt(format_args!("{}", self))
	}
}

// todo: replace the strings with more specific details (a range, asyltype, etc)
#[derive(Debug)]
pub enum AsylError {
	UnexpectedEOF,
	InvalidEscape(AsylSpan, String),
	UnexpectedCloseParen(AsylSpan),
	InvalidNumber(AsylSpan, String),
	TypeMismatch(AsylSpan, String),
	ArgMismatch(AsylSpan, String),
	NotDefined(AsylSpan, String),
	InvalidCall(AsylSpan),
	Shit,
}

impl fmt::Display for AsylError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_str(&match self {
			AsylError::UnexpectedEOF => "Unexpected end of input".to_string(),
			AsylError::InvalidEscape(_, text) => format!("Invalid escape sequence \\`{}\\`", safe_string(text)),
			AsylError::UnexpectedCloseParen(_) => "Unexpected `)`".to_string(),
			AsylError::InvalidNumber(_, text) => format!("Invalid number \\`{}\\`", safe_string(text)),
			AsylError::ArgMismatch(_, text) => format!("Argument mismatch, expected {}", text),
			AsylError::NotDefined(_, text) => format!("\\`{}\\` isn't defined", safe_string(text)),
			AsylError::InvalidCall(_) => "Invalid call".to_string(),
			AsylError::TypeMismatch(_, text) => format!("Type mismatch, expected {}", text),
			AsylError::Shit() => "not implemented".to_string(),
		})
	}
}

impl AsylError {
	pub fn span(&self) -> AsylSpan {
		*match self {
			AsylError::UnexpectedEOF => &None,
			AsylError::InvalidEscape(s, _) => s,
			AsylError::UnexpectedCloseParen(s) => s,
			AsylError::InvalidNumber(s, _) => s,
			AsylError::TypeMismatch(s, _) => s,
			AsylError::ArgMismatch(s, _) => s,
			AsylError::NotDefined(s, _) => s,
			AsylError::InvalidCall(s) => s,
			AsylError::Shit => None,
		}
	}
	pub fn print_span(&self, source_name: &str, source: &str) -> String {
		match self.span() {
			Some(v) => v.print(source_name, source).unwrap_or("no trace".to_string()),
			None => "no trace".to_string()
		}
	}
}

impl error::Error for AsylError {}

type AsylResult<T> = Result<T, AsylError>;
#[derive(Clone)]
pub enum AsylEnvEntry {
	// once you've just defined something
	Unevaluated(Spanned<AsylExpr>),
	// if you use a value
	Evaluated(Spanned<AsylExpr>),
}
#[derive(Clone)]
pub struct AsylEnv<'a> {
	data: HashMap<String, AsylEnvEntry>,
	parent: Option<&'a mut AsylEnv<'a>>,
}

#[derive(Debug)]
enum UniformNumberList {
	Float(Vec<f64>),
	Int(Vec<i64>),
}

fn to_uniform_number_list(list: &[Spanned<AsylExpr>]) -> AsylResult<UniformNumberList> {
	#[derive(PartialEq, Eq, PartialOrd, Ord)]
	enum NumType {
		Float, Int
	}
	fn num_type(v: &Spanned<AsylExpr>) -> AsylResult<NumType> {
		match v.0 {
			AsylExpr::Float(_) => Ok(NumType::Float),
			AsylExpr::Int(_)   => Ok(NumType::Int),
			_ => Err(AsylError::TypeMismatch(v.1, "'float or 'int".to_string()))
		}
	}
	let mut best = NumType::Int;
	for i in list {
		let new_type = num_type(i)?;
		if new_type < best {
			best = new_type;
		}
	}
	macro_rules! parse_list {
		($typ:ty, $res:tt, $a:tt, $int:expr$(, $float:expr)?) => {{
			let mut res = vec![];
			for i in list {
				res.push(match i.0 {
					AsylExpr::Int($a)   => $int,
					$(AsylExpr::Float($a) => $float,
					)? _ => unreachable!(),
				});
			}
			Ok(UniformNumberList::$res(res))
		}}
	}
	match best {
		NumType::Float => parse_list!(f64, Float, a, a as f64, a),
		NumType::Int   => parse_list!(i64, Int,   a, a),
	}
}

macro_rules! tonicity_internal {
	($ty:ty, $name:tt, $check_fn:expr) => {
		fn $name(p: &$ty, n: &[$ty]) -> bool {
			match n.first() {
				Some(v) => $check_fn(p, v) && $name(v, &n[1..]),
				None => true,
			}
		}
	};
}

// this is clever, so i'm stealing it :)
macro_rules! ensure_tonicity {
	($check_fn:expr) => {
		|this, env, args| {
			let args = eval_args(args, env)?;
			asal!(this, args, 1.., "at least one argument");
			tonicity_internal!(f64, f_f64, $check_fn);
			tonicity_internal!(i64, f_i64, $check_fn);
			Ok(Spanned(AsylExpr::Bool(match to_uniform_number_list(&args)? {
				UniformNumberList::Float(v) => f_f64(&v[0], &v[1..]),
				UniformNumberList::Int(v)   => f_i64(&v[0], &v[1..]),
			}), this))
		}
	};
}

macro_rules! ast {
	($this:expr, $val:expr, $ty:tt, $ty_str:expr) => {
		if let AsylExpr::$ty(v) = $val.0 {
			Ok(v)
		} else {
			Err(AsylError::TypeMismatch($this, $ty_str.to_string()))
		}
	}
}

macro_rules! asal {
	($this:expr, $args:expr, $range:expr, $msg:expr) => {
		if !$range.contains($args.len()) {
			return Err(AsylError::ArgMismatch($this, $msg))
		}
	}
}

macro_rules! envi {
	($env:expr, $name:expr, $value:expr) => {
		$env.insert($name.to_string(), Spanned($value, None))
	}
}

pub fn default_env() -> AsylEnv {
	let mut data = HashMap::new();
	// maths
	envi!(data, "+", AsylExpr::ExtFn(|this, env, args| {
		let args = eval_args(args, env)?;
		asal!(this, args, 1.., "at least one argument");
		Ok(Spanned(match to_uniform_number_list(&args)? {
			UniformNumberList::Float(v) => AsylExpr::Float(v.iter().fold(0.0, |a, b| a + b)),
			UniformNumberList::Int(v)   => AsylExpr::Int(  v.iter().fold(0, |a, b| a + b)),
		}, this))
	}));
	envi!(data, "*", AsylExpr::ExtFn(|this, env, args| {
		let args = eval_args(args, env)?;
		asal!(this, args, 1.., "at least one argument");
		Ok(Spanned(match to_uniform_number_list(&args)? {
			UniformNumberList::Float(v) => AsylExpr::Float(v.iter().fold(1.0, |a, b| a * b)),
			UniformNumberList::Int(v)   => AsylExpr::Int(  v.iter().fold(1, |a, b| a * b)),
		}, this))
	}));
	envi!(data, "-", AsylExpr::ExtFn(|this, env, args| {
		let args = eval_args(args, env)?;
		asal!(this, args, 2.., "at least two arguments");
		Ok(Spanned(match to_uniform_number_list(&args)? {
			UniformNumberList::Float(v) => AsylExpr::Float(v[0] - v[1..].iter().fold(0.0, |a, b| a + b)),
			UniformNumberList::Int(v)   => AsylExpr::Int(  v[0] - v[1..].iter().fold(0, |a, b| a + b)),
		}, this))
	}));
	// ' isn't quoting in this lang and # is used by discord
	envi!(data, "'f", AsylExpr::Bool(false));
	envi!(data, "'t", AsylExpr::Bool(true));
	envi!(data, "'n", AsylExpr::List(vec![]));
	// types
	envi!(data, "'symbol", AsylExpr::Type(AsylType::Symbol));
	envi!(data, "'string", AsylExpr::Type(AsylType::String));
	envi!(data, "'float",  AsylExpr::Type(AsylType::Float));
	envi!(data, "'int",    AsylExpr::Type(AsylType::Int));
	envi!(data, "'bool",   AsylExpr::Type(AsylType::Bool));
	envi!(data, "'type",   AsylExpr::Type(AsylType::Type));
	envi!(data, "'list",   AsylExpr::Type(AsylType::List));
	envi!(data, "'fn",     AsylExpr::Type(AsylType::Fn));
	// comparisons
	envi!(data, ">",  AsylExpr::ExtFn(ensure_tonicity!(|a, b| a > b)));
	envi!(data, "=",  AsylExpr::ExtFn(ensure_tonicity!(|a, b| a == b)));
	envi!(data, ">=", AsylExpr::ExtFn(ensure_tonicity!(|a, b| a >= b)));
	envi!(data, "<",  AsylExpr::ExtFn(ensure_tonicity!(|a, b| a < b)));
	envi!(data, "!=", AsylExpr::ExtFn(ensure_tonicity!(|a, b| a != b)));
	envi!(data, "<=", AsylExpr::ExtFn(ensure_tonicity!(|a, b| a <= b)));
	// logical joiners
	envi!(data, "&&", AsylExpr::ExtFn(|this, env, args| {
		for arg in args {
			let res = eval(arg, env)?;
			if ast!(this, res, Bool, "'bool")? == false {
				return Ok(Spanned(AsylExpr::Bool(false), this))
			}
		}
		Ok(Spanned(AsylExpr::Bool(true), this))
	}));
	envi!(data, "||", AsylExpr::ExtFn(|this, env, args| {
		for arg in args {
			let res = eval(arg, env)?;
			if ast!(this, res, Bool, "'bool")? == true {
				return Ok(Spanned(AsylExpr::Bool(true), this))
			}
		}
		Ok(Spanned(AsylExpr::Bool(false), this))
	}));
	envi!(data, "^^", AsylExpr::ExtFn(|this, env, args| {
		let args = eval_args(args, env)?;
		Ok(Spanned(AsylExpr::Bool(
			args
			.iter()
			.map(|v| ast!(this, v, Bool, "'bool"))
			.collect::<AsylResult<Vec<_>>>()?
			.iter()
			.fold(false, |a, b| if *b {!a} else {a})
		), this))
	}));
	envi!(data, "!", AsylExpr::ExtFn(|this, env, args| {
		let args = eval_args(args, env)?;
		asal!(this, args, 1..=1, "one argument");
		Ok(Spanned(AsylExpr::Bool(!ast!(this, args[0], Bool, "'bool")?), this))
	}));
	// typeof operator
	envi!(data, "'", AsylExpr::ExtFn(|this, env, args| {
		asal!(this, args, 1..=1, "one argument");
		return Ok(AsylExpr::Type(args[0].get_type()))
	}));
	envi!(data, "if", AsylExpr::ExtFn(|this, env, args| {
		asal!(this, args, 3..=3, "three arguments");
		if ast!(this, eval(&args[0], env)?, Bool, "'bool")? {
			eval(&args[1], env)
		} else {
			eval(&args[2], env)
		}
	}));
	// i like def and fn over define and lambda, so i'm using those
	envi!(data, "def", AsylExpr::ExtFn(|this, env, args| {
		asal!(this, args, 2..=2, "two arguments");
		let name = ast!(this, args[0], Symbol, "'symbol")?;
		env.data.insert(name, AsylEnvEntry::Unevaluated(args[1].clone()));
		Ok(args[0].clone())
	}));
	envi!(data, "fn", AsylExpr::ExtFn(|this, env, args| {
		// todo: add a begin op and support more than 2 args here
		// asal!(this, args, 2.., "at least two arguments");
		asal!(this, args, 2..=2, "two arguments");
		let args_exprs = ast!(this, args[0], List, "'list")?;
		let lambda_args = vec![];
		for arg in args_exprs {
			lambda_args.push(ast!(this, arg, Symbol, "'symbol")?);
		}
		Ok(Spanned(AsylExpr::Lambda(AsylLambda {
			args: Rc::new(lambda_args),
			body: Rc::new(args[1].clone()),
		}), this))
	}));
	AsylEnv { data, parent: None }
}

#[derive(Debug)]
pub enum AsylToken {
	Symbol(String),
	String(String),
	Paren(bool),
}

#[derive(Debug, Clone)]
pub struct Spanned<T>(pub T, pub AsylSpan);

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

pub fn tokenize(data: &str) -> AsylResult<Vec<Spanned<AsylToken>>> {
	let mut line = 1;
	let mut col = 1;
	let mut iter = data.chars().enumerate().map(|(i, v)| {
		let res = (AsylPos(i, line, col), v);
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
			CharType::Paren(v) => out.push(Spanned(AsylToken::Paren(v), Some(AsylInSpan(chi, 1)))),
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
						None => return Err(AsylError::UnexpectedEOF),
						Some((chi, ch)) => match ch {
							// escapes
							// shorthands: \n \t \r \a \e \0
							// codes:      \xHH \{code}
							// literals:   \#
							'\\' => {
								len += 1;
								match iter.next().and_then(|v| Some(v.1)) {
									None => return Err(AsylError::UnexpectedEOF),
									Some('n') => res.push('\n'),
									Some('t') => res.push('\t'),
									Some('r') => res.push('\r'),
									Some('a') => res.push('\x07'),
									Some('e') => res.push('\x1b'),
									Some('0') => res.push('\x00'),
									Some('x') => {
										let mut t = String::new();
										len += 2;
										t.push(iter.next().ok_or(AsylError::UnexpectedEOF)?.1);
										t.push(iter.next().ok_or(AsylError::UnexpectedEOF)?.1);
										let mut buf = [0u8; 1];
										hex::decode_to_slice(&t, &mut buf).or_else(|_| Err(AsylError::InvalidEscape(Some(AsylInSpan(chi, 4)), format!("\\x{}", t))))?;
										res.push(char::from(buf[0]));
									},
									Some('{') => {
										// unicode parsing :(
										let mut esc = String::new();
										loop { // 3rd level nested loop :)
											if esc.len() > 8 { return Err(AsylError::InvalidEscape(Some(AsylInSpan(chi, 2 + esc.len())), format!("\\{{{}", esc))); }
											len += 1;
											match iter.next() {
												// 12 levels of indent oh no
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
										hex::decode_to_slice(&esc, &mut buf).or_else(|_| Err(AsylError::InvalidEscape(Some(AsylInSpan(chi, 2 + esc.len())), format!("\\{{{}", esc))))?;
										res.push(char::from_u32(u32::from_be_bytes(buf)).ok_or_else(|| AsylError::InvalidEscape(Some(AsylInSpan(chi, 2 + esc.len())), format!("\\{{{}", esc)))?);
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
				out.push(Spanned(AsylToken::String(res), Some(AsylInSpan(chi, len))));
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
				out.push(Spanned(AsylToken::Symbol(res), Some(AsylInSpan(chi, len))));
			},
			CharType::Whitespace => {},
		}
	}
	Ok(out)
}

fn merge_span(start: AsylSpan, end: AsylSpan) -> AsylSpan {
	match start {
		Some(a) => match end {
			Some(b) => Some(AsylInSpan(a.0, b.1 + b.0.0 - a.0.0)),
			None => Some(a),
		},
		None => match end {
			Some(b) => Some(b),
			None => None,
		}
	}
}

pub fn parse_all(tokens: &[Spanned<AsylToken>]) -> AsylResult<Vec<Spanned<AsylExpr>>> {
	let mut out = vec![];
	let mut rest = tokens;
	while rest.len() > 0 {
		let (token, new_rest) = parse(rest)?;
		out.push(token);
		rest = new_rest;
	}
	Ok(out)
}

fn parse<'a>(tokens: &'a [Spanned<AsylToken>]) -> AsylResult<(Spanned<AsylExpr>, &'a [Spanned<AsylToken>])> {
	let (token, rest) = tokens.split_first().ok_or(AsylError::UnexpectedEOF)?;
	match &token.0 {
		AsylToken::Paren(true) => read_seq(token.1, rest),
		AsylToken::Paren(false) => {
			Err(AsylError::UnexpectedCloseParen(token.1))
		},
		AsylToken::String(data) => Ok((Spanned(AsylExpr::String(data.clone()), token.1), rest)),
		AsylToken::Symbol(data) => if is_number(data.as_bytes()) {
			Ok((parse_number(&data, token.1)?, rest))
		} else {
			Ok((Spanned(AsylExpr::Symbol(data.clone()), token.1), rest))
		},
	}
}

fn read_seq<'a>(start: AsylSpan, tokens: &'a [Spanned<AsylToken>]) -> AsylResult<(Spanned<AsylExpr>, &'a [Spanned<AsylToken>])> {
	let mut res = vec![];
	let mut xs = tokens;
	loop {
		let (token, rest) = xs.split_first().ok_or(AsylError::UnexpectedEOF)?;
		match token.0 {
			AsylToken::Paren(false) => return Ok((Spanned(AsylExpr::List(res), merge_span(start, token.1)), rest)),
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

fn parse_number(data: &str, span: AsylSpan) -> AsylResult<Spanned<AsylExpr>> {
	let asyl_error = || AsylError::InvalidNumber(span, data.to_string());
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
						return Err(AsylError::InvalidNumber(span, data.to_string()));
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
						return Err(AsylError::InvalidNumber(span, data.to_string()));
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
						return Err(AsylError::InvalidNumber(span, data.to_string()));
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
		ParseState::IntSign => Err(AsylError::InvalidNumber(span, data.to_string())),
		ParseState::IntBase => Ok(Spanned(AsylExpr::Int(0), span)),
		ParseState::Int => if int_len > 0 {
			if int_neg {
				if int <= 0x1000000000000000u64 {
					Ok(Spanned(AsylExpr::Int(-(int as i64)), span))
				} else {
					Err(AsylError::InvalidNumber(span, data.to_string()))
				}
			} else if int < 0x1000000000000000u64 {
				Ok(Spanned(AsylExpr::Int(int as i64), span))
			} else {
				Err(AsylError::InvalidNumber(span, data.to_string()))
			}
		} else {
			Err(AsylError::InvalidNumber(span, data.to_string()))
		},
		ParseState::Frac => if int_len > 0{
			Ok(Spanned(AsylExpr::Float((int as f64) + (frac as f64) / (10.0f64.powi(frac_len as i32))), span))
		} else {
			Err(AsylError::InvalidNumber(span, data.to_string()))
		},
		ParseState::ExpSign => Err(AsylError::InvalidNumber(span, data.to_string())),
		ParseState::Exp => if int_len > 0 && exp_len > 0 {
			let frac = (int as f64) + (frac as f64) / (10.0f64.powi(frac_len as i32));
			Ok(Spanned(AsylExpr::Float(frac * 10.0f64.powi(exp as i32)), span))
		} else {
			Err(AsylError::InvalidNumber(span, data.to_string()))
		}
	}
}

fn eval_args(args: &[Spanned<AsylExpr>], env: &mut AsylEnv) -> AsylResult<Vec<Spanned<AsylExpr>>> {
	args.iter().map(|x| eval(x, env)).collect()
}

fn lookup_var(span: AsylSpan, name: &str, env: &mut AsylEnv) -> AsylResult<Spanned<AsylExpr>> {
	match env.data.get(name) {
		Some(v) => match v {
			AsylEnvEntry::Unevaluated(exp) => {
				let exp_eval = eval(&exp, env)?;
				env.insert(name.clone(), AsylEnvEntry::Evaluated(exp_eval));
				Ok(exp_eval)
			},
			AsylEnvEntry::Evaluated(exp) => Ok(exp),
		},
		None => match env.parent {
			Some(parent) => lookup_var(name, parent),
			None => Err(AsylError::NotDefined(span, name.clone())),
		}
	}
}

// this feels like the kind of thing that'd just desugar into like (lambda-exec lambda . args)
fn lambda_env(span: AsylSpan, args: &[String], parent: &mut AsylEnv, arg_vals: &[AsylExpr]) -> AsylResult<AsylEnv> {
	if arg_vals.len() != args.len() {
		Err(AsylError::ArgMismatch(span, format!("{} argument(s)", arg_vals.len())))
	}
	let mut data = HashMap::new();
	for (k, v) in args.iter().zip(arg_vals.iter()) {
		data.insert(k.clone(), AsylEnvEntry::Unevaluated(v.clone()));
	}
	Ok(AsylEnv { data, parent })
}

// todo: tail recursion optimization
pub fn eval(exp: &Spanned<AsylExpr>, env: &mut AsylEnv) -> AsylResult<Spanned<AsylExpr>> {
	match &exp.0 {
		AsylExpr::Symbol(v) => lookup_var(exp.1, v, env),
		AsylExpr::List(list) => {
			let first = list.first().ok_or(AsylError::InvalidCall(exp.1))?;
			let args = &list[1..];
			let res = eval(first, env)?;
			match res.0 {
				AsylExpr::ExtFn(f) => {
					Ok(f(exp.1, env, args)?)
				},
				AsylExpr::Lambda(f) => {
					let new_env = lambda_env(exp.1, f.args, env, args)?;
					eval(&f.body, &mut new_env)
				},
				// shorthand for an identity / force function
				// for instance (define var 12) returns var
				// but ((define var 12)) returns 12
				other => eval(other, env),
			}
		},
		other => Ok(Spanned(other.clone(), exp.1))
	}
}
