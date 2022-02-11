//! lisp :)
//!
//! a lot of this is taken from risp (https://stopa.io/post/222)
//!
//! there are a few notable differences though, the main ones
//! being that it's styled like racket and (hopefully) lazily evaluates

use std::collections::{HashMap, BTreeMap};
use std::fmt::Write;
use std::sync::{Arc, RwLock};
use std::{error, fmt, iter};

use crate::strmap::{MappedStr, StrMap};
use crate::utils::{range_to_dual, format_arg_range, format_list, Dual};

#[derive(Debug, Clone)]
pub enum AsylType {
	Symbol, String, Float, Int, Bool, Type, List, Fn, Null
}
impl fmt::Display for AsylType {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_str(match self {
			AsylType::Symbol => "'symbol",
			AsylType::String => "'string",
			AsylType::Float  => "'float",
			AsylType::Int    => "'int",
			AsylType::Bool   => "'bool",
			AsylType::Type   => "'type",
			AsylType::List   => "'list",
			AsylType::Fn     => "'fn",
			AsylType::Null   => "'null",
		})
	}
}

#[derive(Debug, Clone)]
pub struct AsylLambda {
	args: Vec<MappedStr>,
	body: AsylExpr,
	scope: Arc<RwLock<dyn AsylScope>>,
}
#[derive(Clone)]
pub enum AsylExprValue {
	Symbol(MappedStr),
	String(MappedStr),
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
	List(Vec<AsylExpr>),
	ExtFn(fn(AsylSpan, &Arc<RwLock<dyn AsylScope>>, &[AsylExpr], &mut AsylEnv, usize) -> Result<AsylExpr, AsylError>),
	Lambda(Box<AsylLambda>),
	Null,
}

impl fmt::Debug for AsylExprValue {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Symbol(a) => f.debug_tuple("Symbol").field(a).finish(),
			Self::String(a) => f.debug_tuple("String").field(a).finish(),
			Self::Float(a) => f.debug_tuple("Float").field(a).finish(),
			Self::Int(a) => f.debug_tuple("Int").field(a).finish(),
			Self::Bool(a) => f.debug_tuple("Bool").field(a).finish(),
			Self::Type(a) => f.debug_tuple("Type").field(a).finish(),
			Self::List(a) => f.debug_tuple("List").field(a).finish(),
			Self::ExtFn(_) => f.debug_tuple("ExtFn").finish(),
			Self::Lambda(a) => f.debug_tuple("Lambda").field(a).finish(),
			Self::Null => write!(f, "Null"),
		}
	}
}

impl AsylExprValue {
	fn get_type(&self) -> AsylType {
		match self {
			AsylExprValue::Symbol(_) => AsylType::Symbol,
			AsylExprValue::String(_) => AsylType::String,
			AsylExprValue::Float(_)  => AsylType::Float,
			AsylExprValue::Int(_)    => AsylType::Int,
			AsylExprValue::Bool(_)   => AsylType::Bool,
			AsylExprValue::Type(_)   => AsylType::Type,
			AsylExprValue::List(_)   => AsylType::List,
			AsylExprValue::ExtFn(_)  => AsylType::Fn,
			AsylExprValue::Lambda(_) => AsylType::Fn,
    	AsylExprValue::Null      => AsylType::Null,
		}
	}
}

// char, line, col, name, content
#[derive(Debug, Clone)]
pub struct AsylPos(usize, usize, usize, MappedStr, MappedStr);
// start, len
#[derive(Debug, Clone)]
pub struct AsylInSpan(AsylPos, usize);
type AsylSpan = Option<AsylInSpan>;

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

impl fmt::Display for AsylExpr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_str(&match &self.0 {
			AsylExprValue::Symbol(v) => v.to_string(),
			AsylExprValue::String(v) => format!("{:?}", v.to_string()),
			AsylExprValue::Float(v) => v.to_string(),
			AsylExprValue::Int(v) => v.to_string(),
			// technically 't and 'f are just variables but i don't care
			AsylExprValue::Bool(v) => if *v {"'t"} else {"'f"}.to_string(),
			AsylExprValue::List(v) => format!("({})", v.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(" ")),
			AsylExprValue::ExtFn(_) => "<proc>".to_string(),
			AsylExprValue::Lambda(data) => format!("<lambda ({})>", data.args.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(" ")),
			AsylExprValue::Type(v) => v.to_string(),
    	AsylExprValue::Null => "null".to_string(),
		})
	}
}

impl fmt::Debug for AsylExpr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.debug_tuple("AsylExpr").field(&self.0).finish()
	}
}

// todo: replace the strings with more specific details (a range, asyltype, etc)
#[derive(Debug)]
#[allow(dead_code)]
pub enum AsylErrorType {
	UnexpectedEOF,

	// user data
	InvalidEscape(String),
	InvalidNumber(String),
	NotDefined(String),
	UseDuringEval(String),

	UnexpectedCloseParen,
	TypeMismatch(Vec<AsylType>),
	ArgMismatch(Dual, usize),
	InvalidCall,
	Internal,
	Shit,
}

#[derive(Debug)]
pub struct AsylError(AsylErrorType, AsylSpan);

impl fmt::Display for AsylError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_str(&match &self.0 {
			AsylErrorType::UnexpectedEOF => "Unexpected end of input".to_string(),
			AsylErrorType::InvalidEscape(text) => format!("Invalid escape sequence \\`{}\\`", text),
			AsylErrorType::InvalidNumber(text) => format!("Invalid number \\`{}\\`", text),
			AsylErrorType::NotDefined(text) => format!("\\`{}\\` isn't defined", text),
			AsylErrorType::UseDuringEval(text) => format!("Can't get the value of \\`{}\\` because it is currently being evaluated", text),
			AsylErrorType::UnexpectedCloseParen => "Unexpected `)`".to_string(),
			AsylErrorType::TypeMismatch(types) => format!("Type mismatch, expected {}", format_list(&types.iter().map(|v| v.to_string()).collect::<Vec<_>>(), false)),
			AsylErrorType::ArgMismatch(dual, got) => format!("Argument count mismatch: expected {}, got {}", format_arg_range(*dual), got),
			AsylErrorType::InvalidCall => "Invalid call".to_string(),
			AsylErrorType::Internal => "Internal error".to_string(),
			AsylErrorType::Shit => "Not implemented".to_string(),
		})
	}
}

impl AsylError {
	pub fn print(&self) -> String {
		match &self.1 {
			Some(v) => v.print(self).unwrap_or("no trace :(".to_string()),
			None => "no trace :(".to_string()
		}
	}
}

impl error::Error for AsylError {}

type AsylResult<T> = Result<T, AsylError>;
#[derive(Debug, Clone)]
pub enum AsylScopeEntry {
	/// once you've just defined something
	Unevaluated(AsylExpr),
	/// while you're using it
	Evaluating,
	/// once you've used it
	Evaluated(AsylExpr),
}

pub trait AsylScope: Sync + Send + fmt::Debug {
	fn get_ring(&self, name: &MappedStr) -> Option<&BTreeMap<usize, AsylScopeEntry>>;
	fn get_var(&self, name: &MappedStr, max_index: usize) -> Option<(&AsylScopeEntry, usize)> {
		match self.get_ring(name) {
			Some(ring) => {
				if max_index == 0 {
					match ring.iter().rev().next() {
						Some(v) => Some((v.1, *v.0)),
						None => None
					}
				} else {
					let mut best_index = 0;
					for k in ring.keys() {
						if *k <= max_index {
							best_index = *k;
						}
					}
					if best_index > 0 {
						Some((&ring[&best_index], best_index))
					} else {
						None
					}
				}
			},
			None => None
		}
	}
	fn set_var(&mut self, name: MappedStr, index: usize, value: AsylScopeEntry);
	fn get_parent(&self) -> Option<&Arc<RwLock<dyn AsylScope>>>;
}
#[derive(Debug)]
struct AsylLambdaScope {
	data: HashMap<MappedStr, BTreeMap<usize, AsylScopeEntry>>,
	parent: Arc<RwLock<dyn AsylScope>>,
}
impl AsylScope for AsylLambdaScope {
	fn get_ring(&self, name: &MappedStr) -> Option<&BTreeMap<usize, AsylScopeEntry>> {
		self.data.get(name)
	}
	fn set_var(&mut self, name: MappedStr, index: usize, value: AsylScopeEntry) {
		let ent = self.data.entry(name).or_insert_with(|| BTreeMap::new());
		ent.insert(index, value);
	}
	fn get_parent(&self) -> Option<&Arc<RwLock<dyn AsylScope>>> {
		Some(&self.parent)
	}
}
#[derive(Debug)]
pub struct AsylRootScope {
	data: HashMap<MappedStr, BTreeMap<usize, AsylScopeEntry>>,
}
impl AsylScope for AsylRootScope {
	fn get_ring(&self, name: &MappedStr) -> Option<&BTreeMap<usize, AsylScopeEntry>> {
		self.data.get(name)
	}
	fn set_var(&mut self, name: MappedStr, index: usize, value: AsylScopeEntry) {
		let ent = self.data.entry(name).or_insert_with(|| BTreeMap::new());
		ent.insert(index, value);
	}
	fn get_parent(&self) -> Option<&Arc<RwLock<dyn AsylScope>>> {
		None
	}
}

pub struct AsylEnv {
	pub time: usize,
	pub map: StrMap,
}

#[derive(Debug)]
enum UniformNumberList {
	Float(Vec<f64>),
	Int(Vec<i64>),
}

fn to_uniform_number_list(list: &[AsylExpr]) -> AsylResult<UniformNumberList> {
	#[derive(PartialEq, Eq, PartialOrd, Ord)]
	enum NumType {
		Float, Int
	}
	fn num_type(v: &AsylExpr) -> AsylResult<NumType> {
		match v.0 {
			AsylExprValue::Float(_) => Ok(NumType::Float),
			AsylExprValue::Int(_)   => Ok(NumType::Int),
			_ => Err(AsylError(AsylErrorType::TypeMismatch(vec![AsylType::Float, AsylType::Int]), v.1.clone()))
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
					AsylExprValue::Int($a)   => $int,
					$(AsylExprValue::Float($a) => $float,
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
		|this, scope, args, env, time| {
			let args = eval_args(args, scope, env, time)?;
			assert_arg_length!(this, args, 1..);
			tonicity_internal!(f64, f_f64, $check_fn);
			tonicity_internal!(i64, f_i64, $check_fn);
			Ok(AsylExpr(AsylExprValue::Bool(match to_uniform_number_list(&args)? {
				UniformNumberList::Float(v) => f_f64(&v[0], &v[1..]),
				UniformNumberList::Int(v)   => f_i64(&v[0], &v[1..]),
			}), this, None))
		}
	};
}

macro_rules! assert_type {
	($this:expr, $val:expr, $ty:tt) => {
		if let AsylExprValue::$ty(v) = &$val.0 {
			Ok(v)
		} else {
			Err(AsylError(AsylErrorType::TypeMismatch(vec![AsylType::$ty]), $this))
		}
	}
}

macro_rules! check_type {
	($this:expr, $val:expr, $ty:tt) => {
		if let AsylExprValue::$ty(v) = &$val.0 { Some(v) } else { None }
	};
}

macro_rules! assert_arg_length {
	($this:expr, $args:expr, $range:expr) => {
		if !$range.contains(&$args.len()) {
			return Err(AsylError(AsylErrorType::ArgMismatch(range_to_dual($range), $args.len()), $this))
		}
	}
}

macro_rules! scope_insert {
	($env:expr, $scope:expr, $name:expr, $value:expr) => {
		$scope.insert($env.map.add($name), BTreeMap::from([(1, AsylScopeEntry::Evaluated(AsylExpr($value, None, None)))]))
	}
}

pub fn default_scope(env: &mut AsylEnv) -> Arc<RwLock<dyn AsylScope>> {
	let mut data = HashMap::new();
	// maths
	scope_insert!(env, data, "+", AsylExprValue::ExtFn(|this, scope, args, env, time| {
		let args = eval_args(args, scope, env, time)?;
		assert_arg_length!(this, args, 1..);
		Ok(AsylExpr(match to_uniform_number_list(&args)? {
			UniformNumberList::Float(v) => AsylExprValue::Float(v.iter().fold(0.0, |a, b| a + b)),
			UniformNumberList::Int(v)   => AsylExprValue::Int(  v.iter().fold(0, |a, b| a + b)),
		}, this, None))
	}));
	scope_insert!(env, data, "*", AsylExprValue::ExtFn(|this, scope, args, env, time| {
		let args = eval_args(args, scope, env, time)?;
		assert_arg_length!(this, args, 1..);
		Ok(AsylExpr(match to_uniform_number_list(&args)? {
			UniformNumberList::Float(v) => AsylExprValue::Float(v.iter().fold(1.0, |a, b| a * b)),
			UniformNumberList::Int(v)   => AsylExprValue::Int(  v.iter().fold(1, |a, b| a * b)),
		}, this, None))
	}));
	scope_insert!(env, data, "-", AsylExprValue::ExtFn(|this, scope, args, env, time| {
		let args = eval_args(args, scope, env, time)?;
		assert_arg_length!(this, args, 2..);
		Ok(AsylExpr(match to_uniform_number_list(&args)? {
			UniformNumberList::Float(v) => AsylExprValue::Float(v[0] - v[1..].iter().fold(0.0, |a, b| a + b)),
			UniformNumberList::Int(v)   => AsylExprValue::Int(  v[0] - v[1..].iter().fold(0, |a, b| a + b)),
		}, this, None))
	}));
	// ' isn't quoting in this lang and # is used by discord
	scope_insert!(env, data, "'f", AsylExprValue::Bool(false));
	scope_insert!(env, data, "'t", AsylExprValue::Bool(true));
	scope_insert!(env, data, "'n", AsylExprValue::List(vec![]));
	// types
	scope_insert!(env, data, "'symbol", AsylExprValue::Type(AsylType::Symbol));
	scope_insert!(env, data, "'string", AsylExprValue::Type(AsylType::String));
	scope_insert!(env, data, "'float",  AsylExprValue::Type(AsylType::Float));
	scope_insert!(env, data, "'int",    AsylExprValue::Type(AsylType::Int));
	scope_insert!(env, data, "'bool",   AsylExprValue::Type(AsylType::Bool));
	scope_insert!(env, data, "'type",   AsylExprValue::Type(AsylType::Type));
	scope_insert!(env, data, "'list",   AsylExprValue::Type(AsylType::List));
	scope_insert!(env, data, "'fn",     AsylExprValue::Type(AsylType::Fn));
	// comparisons
	scope_insert!(env, data, ">",  AsylExprValue::ExtFn(ensure_tonicity!(|a, b| a > b)));
	scope_insert!(env, data, "=",  AsylExprValue::ExtFn(ensure_tonicity!(|a, b| a == b)));
	scope_insert!(env, data, ">=", AsylExprValue::ExtFn(ensure_tonicity!(|a, b| a >= b)));
	scope_insert!(env, data, "<",  AsylExprValue::ExtFn(ensure_tonicity!(|a, b| a < b)));
	scope_insert!(env, data, "!=", AsylExprValue::ExtFn(ensure_tonicity!(|a, b| a != b)));
	scope_insert!(env, data, "<=", AsylExprValue::ExtFn(ensure_tonicity!(|a, b| a <= b)));
	// logical joiners
	scope_insert!(env, data, "&&", AsylExprValue::ExtFn(|this, scope, args, env, time| {
		for arg in args {
			let res = eval(arg, scope, env, time)?;
			if *assert_type!(this.clone(), res, Bool)? == false {
				return Ok(AsylExpr(AsylExprValue::Bool(false), this, None))
			}
		}
		Ok(AsylExpr(AsylExprValue::Bool(true), this, None))
	}));
	scope_insert!(env, data, "||", AsylExprValue::ExtFn(|this, scope, args, env, time| {
		for arg in args {
			let res = eval(arg, scope, env, time)?;
			if *assert_type!(this.clone(), res, Bool)? == true {
				return Ok(AsylExpr(AsylExprValue::Bool(true), this, None))
			}
		}
		Ok(AsylExpr(AsylExprValue::Bool(false), this, None))
	}));
	scope_insert!(env, data, "^^", AsylExprValue::ExtFn(|this, scope, args, env, time| {
		let args = eval_args(args, scope, env, time)?;
		Ok(AsylExpr(AsylExprValue::Bool(
			args
			.iter()
			.map(|v| assert_type!(this.clone(), v, Bool))
			.collect::<AsylResult<Vec<_>>>()?
			.iter()
			.fold(false, |a, b| if **b {!a} else {a})
		), this, None))
	}));
	scope_insert!(env, data, "!", AsylExprValue::ExtFn(|this, scope, args, env, time| {
		let args = eval_args(args, scope, env, time)?;
		assert_arg_length!(this, args, 1..=1);
		Ok(AsylExpr(AsylExprValue::Bool(!*assert_type!(this.clone(), args[0], Bool)?), this, None))
	}));
	// typeof operator
	scope_insert!(env, data, "'", AsylExprValue::ExtFn(|this, scope, args, env, time| {
		assert_arg_length!(this, args, 1..=1);
		return Ok(AsylExpr(AsylExprValue::Type(eval(&args[0], scope, env, time)?.0.get_type()), this, None))
	}));
	scope_insert!(env, data, "if", AsylExprValue::ExtFn(|this, scope, args, env, time| {
		assert_arg_length!(this, args, 3..=3);
		if *assert_type!(this, eval(&args[0], scope, env, time)?, Bool)? {
			eval(&args[1], scope, env, time)
		} else {
			eval(&args[2], scope, env, time)
		}
	}));
	// i like def and fn over define and lambda, so i'm using those
	scope_insert!(env, data, "def", AsylExprValue::ExtFn(|this, scope, args, env, time| {
		assert_arg_length!(this, args, 2..=2);
		if let Some(v) = check_type!(this.clone(), args[0], List) {
			return eval(&AsylExpr(AsylExprValue::List(vec![
				AsylExpr(AsylExprValue::Symbol(env.map.add("def")), None, None),
				v[0].clone(),
				AsylExpr(AsylExprValue::List(vec![
					AsylExpr(AsylExprValue::Symbol(env.map.add("fn")), None, None),
					AsylExpr(AsylExprValue::List(v[1..].into()), None, None),
					args[1].clone()
				]), None, None)
			]), None, None), scope, env, time)
		}
		let name = assert_type!(this.clone(), args[0], Symbol)?;
		{
			let mut lock = scope.write().map_err(|_| AsylError(AsylErrorType::Internal, this))?;
			if lock.get_var(&name, 0).is_some() {
				env.time += 1;
				log::debug!("env.time + 1 (now {}) (def)", env.time);
			}
			lock.set_var(name.clone(), env.time, AsylScopeEntry::Unevaluated(args[1].clone()));
		}
		Ok(args[0].clone())
	}));
	scope_insert!(env, data, "fn", AsylExprValue::ExtFn(|this, scope, args, _env, _time| {
		// todo: add a begin op and support more than 2 args here
		// asal!(this, args, 2.., "at least two arguments");
		assert_arg_length!(this, args, 2..=2);
		let args_exprs = assert_type!(this.clone(), args[0], List)?;
		let mut lambda_args = vec![];
		for arg in args_exprs {
			lambda_args.push(assert_type!(this.clone(), arg, Symbol)?.clone());
		}
		Ok(AsylExpr(AsylExprValue::Lambda(box AsylLambda {
			args: lambda_args,
			body: args[1].clone(),
			scope: scope.clone(),
		}), this, None))
	}));
	Arc::new(RwLock::new(AsylRootScope { data }))
}

#[derive(Debug)]
pub enum AsylTokenValue {
	Symbol(MappedStr),
	String(MappedStr),
	Paren(bool),
}

pub struct AsylToken(AsylTokenValue, AsylSpan);

#[derive(Clone)]
pub struct AsylExpr(pub AsylExprValue, pub AsylSpan, pub Option<Arc<RwLock<dyn AsylScope>>>);

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
		let res = (AsylPos(i, line, col, file_name.clone(), data.clone()), v);
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
			CharType::Paren(v) => out.push(AsylToken(AsylTokenValue::Paren(v), Some(AsylInSpan(chi, 1)))),
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
										hex::decode_to_slice(&t, &mut buf).or_else(|_| Err(AsylError(AsylErrorType::InvalidEscape(format!("\\x{}", t)), Some(AsylInSpan(chi, 4)))))?;
										res.push(char::from(buf[0]));
									},
									Some('{') => {
										// unicode parsing :(
										let mut esc = String::new();
										loop { // 3rd level nested loop :)
											if esc.len() > 8 { return Err(AsylError(AsylErrorType::InvalidEscape(format!("\\{{{}…", esc)), Some(AsylInSpan(chi, 2 + esc.len())))); }
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
										hex::decode_to_slice(&esc, &mut buf).or_else(|_| Err(AsylError(AsylErrorType::InvalidEscape(format!("\\{{{}}}", esc)), Some(AsylInSpan(chi.clone(), 2 + esc.len())))))?;
										res.push(char::from_u32(u32::from_be_bytes(buf)).ok_or_else(|| AsylError(AsylErrorType::InvalidEscape(format!("\\{{{}}}", esc)), Some(AsylInSpan(chi.clone(), 2 + esc.len()))))?);
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
				out.push(AsylToken(AsylTokenValue::String(env.map.add(res)), Some(AsylInSpan(chi, len))));
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
				out.push(AsylToken(AsylTokenValue::Symbol(env.map.add(res)), Some(AsylInSpan(chi, len))));
			},
			CharType::Whitespace => {},
		}
	}
	Ok(out)
}

fn merge_span(start: AsylSpan, end: AsylSpan) -> AsylSpan {
	match start {
		Some(a) => match end {
			Some(b) => Some(AsylInSpan(a.clone().0, b.1 + b.0.0 - a.0.0)),
			None => Some(a),
		},
		None => match end {
			Some(b) => Some(b),
			None => None,
		}
	}
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

fn eval_args(args: &[AsylExpr], scope: &Arc<RwLock<dyn AsylScope>>, env: &mut AsylEnv, time: usize) -> AsylResult<Vec<AsylExpr>> {
	args.iter().map(|x| eval(x, scope, env, time)).collect()
}

fn lookup_var(span: AsylSpan, name: &MappedStr, time: usize, scope: &Arc<RwLock<dyn AsylScope>>, env: &mut AsylEnv) -> AsylResult<AsylExpr> {
	enum LookupResult {
		Ok(AsylExpr),
		Err(AsylError),
		Recurse(Arc<RwLock<dyn AsylScope>>),
		Insert(AsylExpr, usize),
	}
	let res = {
		let lock = scope.read().map_err(|_| AsylError(AsylErrorType::Internal, span.clone()))?;
		match lock.get_var(&name, time) {
			Some(v) => match v.0 {
				AsylScopeEntry::Unevaluated(exp) => {
					// lock.data.insert(name.to_string(), AsylEnvEntry::Evaluated(exp_eval.clone()));
					LookupResult::Insert(exp.clone(), v.1)
				},
				AsylScopeEntry::Evaluated(exp) => LookupResult::Ok(exp.clone()),
				AsylScopeEntry::Evaluating => LookupResult::Err(AsylError(AsylErrorType::UseDuringEval(name.to_string()), span.clone())),
			},
			None => match &lock.get_parent() {
				Some(parent) => LookupResult::Recurse((*parent).clone()),
				None => LookupResult::Err(AsylError(AsylErrorType::NotDefined(name.to_string()), span.clone())),
			}
		}
	};
	match res {
		LookupResult::Ok(v) => Ok(v),
		LookupResult::Err(v) => Err(v),
		LookupResult::Recurse(parent) => lookup_var(span, name, time, &parent, env),
		LookupResult::Insert(exp, time) => {
			{
				let mut write_lock = scope.write().map_err(|_| AsylError(AsylErrorType::Internal, span.clone()))?;
				write_lock.set_var(name.clone(), time, AsylScopeEntry::Evaluating);
			}
			// free the lock so that further lookups in eval work
			// time travel happens here :)
			let value = eval(&exp, scope, env, time)?;
			{
				let mut write_lock = scope.write().map_err(|_| AsylError(AsylErrorType::Internal, span))?;
				write_lock.set_var(name.clone(), time, AsylScopeEntry::Evaluated(value.clone()));
			}
			Ok(value)
		}
	}
}

// this feels like the kind of thing that'd just desugar into like (lambda-exec lambda . args)
fn lambda_scope(span: AsylSpan, args: &[MappedStr], parent: &Arc<RwLock<dyn AsylScope>>, arg_vals: &[AsylExpr], env: &mut AsylEnv) -> AsylResult<Arc<RwLock<dyn AsylScope>>> {
	if arg_vals.len() != args.len() {
		return Err(AsylError(AsylErrorType::ArgMismatch((Some(args.len()), Some(args.len())), arg_vals.len()), span))
	}
	env.time += 1;
	log::debug!("env.time + 1 (now {}) (lambda)", env.time);
	let mut data = HashMap::new();
	for (k, v) in args.iter().zip(arg_vals.iter()) {
		data.insert(k.clone(), BTreeMap::from([(env.time, AsylScopeEntry::Unevaluated(v.clone()))]));
	}
	Ok(Arc::new(RwLock::new(AsylLambdaScope { data, parent: parent.clone() })))
}

// todo: tail recursion optimization
pub fn eval(exp: &AsylExpr, scope: &Arc<RwLock<dyn AsylScope>>, env: &mut AsylEnv, time: usize) -> AsylResult<AsylExpr> {
	log::debug!("eval {} {:?}", time, exp);
	let scope = match &exp.2 {
		Some(v) => v,
		None => scope,
	};
	let res = match &exp.0 {
		AsylExprValue::Symbol(v) => lookup_var(exp.1.clone(), v, time, scope, env),
		AsylExprValue::List(list) => {
			let first = list.first().ok_or(AsylError(AsylErrorType::InvalidCall, exp.1.clone()))?;
			let args = &list[1..];
			let res = eval(first, scope, env, time)?;
			match res.0 {
				AsylExprValue::ExtFn(f) => {
					Ok(f(exp.1.clone(), scope, args, env, time)?)
				},
				AsylExprValue::Lambda(f) => {
					let new_scope = lambda_scope(
						exp.1.clone(), &f.args, &f.scope,
						&args
							.iter()
							.map(|AsylExpr(a, b, _)| AsylExpr(a.clone(), b.clone(), Some(scope.clone())))
							.collect::<Vec<_>>(),
						env,
					)?;
					eval(&f.body, &new_scope, env, time)
				},
				// shorthand for an identity / force function
				// for instance (define var 12) returns var
				// but ((define var 12)) returns 12
				_ => Ok(res),
			}
		},
		other => Ok(AsylExpr(other.clone(), exp.1.clone(), exp.2.as_ref().map(|v| v.clone())))
	};
	log::debug!("eval result: {:?}", res);
	res
}
