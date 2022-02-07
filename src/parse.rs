#[derive(Debug, Clone)]
enum ALExpr {
	Symbol(String),
	String(String),
	Float(f64),
	Int(i64),
	UInt(u64),
	List(Vec<Expr>),
}

#[derive(Debug)]
enum ALError {
	Other(String),
}

#[derive(Clone)]
struct ALEnv {
	data: HashMap<String, AlExpr>,
}
