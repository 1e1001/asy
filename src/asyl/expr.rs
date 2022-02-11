use std::fmt;
use std::sync::Arc;
use tokio::sync::RwLock;

use crate::strmap::MappedStr;

use super::error::{AsylResult, AsylSpan};
use super::scope::{AsylScope, AsylEnv};


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
/// lambda data
#[derive(Debug, Clone)]
pub struct AsylLambda {
	pub args: Vec<MappedStr>,
	pub body: AsylExpr,
	pub scope: Arc<RwLock<dyn AsylScope>>,
}
/// a value inside an expression
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
	ExtFn(fn(AsylSpan, &Arc<RwLock<dyn AsylScope>>, &[AsylExpr], &mut AsylEnv, usize) -> AsylResult<AsylExpr>),
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

/// an expression
#[derive(Clone)]
pub struct AsylExpr(pub AsylExprValue, pub AsylSpan, pub Option<Arc<RwLock<dyn AsylScope>>>);

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
