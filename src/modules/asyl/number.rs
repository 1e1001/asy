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

enum Sign {
	Positive, Negative
}

/// number type 1: u2^size*size
struct UHuge(Vec<usize>);

/// number type 2: (-)uhuge/uhuge
struct ExactNumber {
	sign: Sign,
	numerator: UHuge,
	denominator: UHuge,
}
/// number type 3: (-)uhuge/uhuge or f64
pub enum Number {
	Inexact(f64),
	Exact(ExactNumber),
}
