//! big numbers
//!
//! stores numbers as either an f64 or a big integer fractions
//! on a 32-bit system you can store a u2³⁷
//! on a 64-bit system you “can” store a u2⁷⁰
//!
//! fun docs on things
//!
//! primitives to implement lol:
//! -  +   addition        Add     AddAssign
//! -  -   negation        Neg     NegAssign
//! -  -   subtract        Sub     SubAssign
//! -  *   multiplication  Mul     MulAssign
//! -  /   division        Div     DivAssign
//! -  %   modulo          Rem     RemAssign
//! -  &   bin. and        BitAnd  BitAndAssign
//! -  |   bin. or         BitOr   BitOrAssign
//! -  ^   bin. xor        BitXor  BitXorAssign
//! -  !   bin. negation   Not     NotAssign (technically ~ but i don't care)
//! -  <<  left-shift      Shl     ShlAssign
//! -  >>  right-shift     Shr     ShrAssign
//! - number->string
//! - exact->inexact, inexact->exact
//! - log_a(b), log₂(n), ln(n), log₁₀(n)
//! - a^b, 2^n, e^n, 10^n
//! - e^n - 1, ln(1 + n), 1/n
//! - abs, cbrt, ceil, clamp, floor, max, min, root, round, sign, sqrt
//! - acos(h), asin(h), atan(h), atan2, cos(h), sin(h), tan(h)
//! - fract, trunc, hypot, muladd, add1, sub1, gcd, lcm, numerator, denominator
//! - is-finite, is-infinite, is-nan, is-normal, is-positive,
//! - is-negative, is-subnormal, is-exact, is-inexact
//! - eq, ord traits

use std::borrow::Cow;

enum Sign {
	Positive, Negative
}

impl Sign {
	fn to_val(&self) -> usize {
		match self {
			Sign::Positive => usize::MIN,
			Sign::Negative => usize::MAX,
		}
	}
}

/// number type 1: i2^size*size
/// MSB last
#[derive(Clone)]
struct IHuge(Vec<usize>);

impl IHuge {
	fn trim_vec(v: Vec<usize>) -> Self {
		while v.len() > 1 {
			let val = v[v.len() - 1];
			if val > usize::MIN && val < usize::MAX {
				break
			}
			// trim 0×00… and 0×FF…
			v.pop();
		}
		Self(v)
	}
	fn from_usize(v: usize) -> Self {
		Self(vec![v])
	}
	fn from_isize(v: isize) -> Self {
		// best solution i could think of
		Self(vec![usize::from_ne_bytes(v.to_ne_bytes())])
	}
	fn sign(&self) -> Sign {
		if self.0[self.0.len() - 1] & (1 << (usize::BITS - 1)) != 0 {
			Sign::Negative
		} else {
			Sign::Positive
		}
	}
	fn match_len(&self, other: &Self, extra: usize) -> (Self, Self) {
		let self_len = self.0.len();
		let other_len = other.0.len();
		let self_val = self.0.clone();
		let other_val = other.0.clone();
		let self_push = self.sign().to_val();
		let other_push = other.sign().to_val();
		let target_len = self_len.max(other_len) + extra;
		for i in self_len..target_len {
			self_val.push(self_push);
		}
		for i in other_len..target_len {
			other_val.push(other_push);
		}
		(Self(self_val), Self(other_val))
	}
	fn match_len_consume(self, other: Self, extra: usize) -> (Self, Self) {
		let self_len = self.0.len();
		let other_len = other.0.len();
		let self_push = self.sign().to_val();
		let other_push = other.sign().to_val();
		let target_len = self_len.max(other_len) + extra;
		for i in self_len..target_len {
			self.0.push(self_push);
		}
		for i in other_len..target_len {
			other.0.push(other_push);
		}
		(self, other)
	}
	fn add(&self, other: Self) -> Self {
		// length: maximum is current length + 1
		// according to my dad, just expand the integer and add
		let (self_val, other_val) = self.match_len(&other, 1);
	}
}

/// number type 2: (-)uhuge/uhuge
struct ExactNumber {
	numerator: IHuge,
	denominator: IHuge,
}
/// number type 3: (-)uhuge/uhuge or f64
pub enum Number {
	Inexact(f64),
	Exact(ExactNumber),
}
