use crate::utils::Dual;

use super::expr::AsylType;
use super::span::AsylSpan;



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
