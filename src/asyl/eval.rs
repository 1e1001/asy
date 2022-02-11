use std::collections::{HashMap, BTreeMap};
use std::sync::Arc;

use tokio::sync::RwLock;

use crate::asyl::error::{AsylError, AsylErrorType};
use crate::asyl::expr::AsylExprValue;
use crate::asyl::scope::{AsylScopeEntry, AsylLambdaScope};
use crate::strmap::MappedStr;

use super::error::{AsylResult, AsylSpan};
use super::expr::AsylExpr;
use super::scope::{AsylScope, AsylEnv};


async fn eval_args(args: &[AsylExpr], scope: &Arc<RwLock<dyn AsylScope>>, env: &mut AsylEnv, time: usize) -> AsylResult<Vec<AsylExpr>> {
	let mut res = Vec::with_capacity(args.len());
	for x in args {
		res.push(eval(x, scope, env, time).await?);
	}
	Ok(res)
}

#[async_recursion::async_recursion]
async fn lookup_var(span: AsylSpan, name: &MappedStr, time: usize, scope: &Arc<RwLock<dyn AsylScope>>, env: &mut AsylEnv) -> AsylResult<AsylExpr> {
	enum LookupResult {
		Ok(AsylExpr),
		Err(AsylError),
		Recurse(Arc<RwLock<dyn AsylScope>>),
		Insert(AsylExpr, usize),
	}
	let res = {
		let lock = scope.read().await;
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
		LookupResult::Recurse(parent) => lookup_var(span, name, time, &parent, env).await,
		LookupResult::Insert(exp, time) => {
			{
				let mut write_lock = scope.write().await;
				write_lock.set_var(name.clone(), time, AsylScopeEntry::Evaluating);
			}
			// free the lock so that further lookups in eval work
			// time travel happens here :)
			let value = eval(&exp, scope, env, time).await?;
			{
				let mut write_lock = scope.write().await;
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
#[async_recursion::async_recursion]
pub async fn eval(exp: &AsylExpr, scope: &Arc<RwLock<dyn AsylScope>>, env: &mut AsylEnv, time: usize) -> AsylResult<AsylExpr> {
	log::debug!("eval {} {:?}", time, exp);
	let scope = match &exp.2 {
		Some(v) => v,
		None => scope,
	};
	let res = match &exp.0 {
		AsylExprValue::Symbol(v) => lookup_var(exp.1.clone(), v, time, scope, env).await,
		AsylExprValue::List(list) => {
			let first = list.first().ok_or(AsylError(AsylErrorType::InvalidCall, exp.1.clone()))?;
			let args = &list[1..];
			let res = eval(first, scope, env, time).await?;
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
					eval(&f.body, &new_scope, env, time).await
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
