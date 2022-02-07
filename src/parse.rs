use std::{collections::HashMap, io, str::Chars};

#[derive(Debug, Clone)]
enum AsylExpr {
	Symbol(String),
	String(String),
	Float(f64),
	Int(i64),
	UInt(u64),
	List(Vec<AsylExpr>),
}

#[derive(Debug)]
enum AsylError {
	Other(String),
}

#[derive(Clone)]
struct AsylEnv {
	data: HashMap<String, AsylExpr>,
}

fn parse(data: String) -> Vec<AsylExpr> {
	let iter = data.chars();
	let out = vec![];
	out
}
