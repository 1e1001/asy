use std::{fmt, error};

use crate::strmap::MappedStr;
use crate::utils::{Dual, format_list, format_arg_range};

use super::expr::AsylType;


// char, line, col, length, name, content
#[derive(Debug, Clone)]
pub struct AsylInSpan(pub usize, pub usize, pub usize, pub usize, pub MappedStr, pub MappedStr);

impl AsylInSpan {
	pub fn with_len(&self, len: usize) -> Self {
		AsylInSpan(self.0, self.1, self.2, len, self.4.clone(), self.5.clone())
	}
}

pub fn merge_span(start: AsylSpan, end: AsylSpan) -> AsylSpan {
	match start {
		Some(a) => match end {
			Some(b) => Some(a.with_len(b.1 + b.0 - a.0)),
			None => Some(a),
		},
		None => match end {
			Some(b) => Some(b),
			None => None,
		}
	}
}

pub type AsylSpan = Option<AsylInSpan>;


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
pub struct AsylError(pub AsylErrorType, pub AsylSpan);

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
		// match &self.1 {
		// 	Some(v) => v.print(self).unwrap_or("no trace :(".to_string()),
		// 	None => "no trace :(".to_string()
		// }
		"trace implementation pending :(".to_string()
	}
}

impl error::Error for AsylError {}
pub type AsylResult<T> = Result<T, AsylError>;
