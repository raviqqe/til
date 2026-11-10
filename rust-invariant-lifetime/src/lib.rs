use core::{cell::UnsafeCell, marker::PhantomData};

/// Covariant in its lifetime, so a long lifetime shrinks to a shorter one.
///
/// ```
/// use rust_invariant_lifetime::Covariant;
///
/// fn shorten<'long: 'short, 'short>(x: Covariant<'long>) -> Covariant<'short> {
///     x
/// }
/// ```
pub struct Covariant<'a>(PhantomData<&'a ()>);

/// Invariant in its lifetime because `UnsafeCell<T>` is invariant in `T`.
///
/// ```compile_fail,E0308
/// use rust_invariant_lifetime::Invariant;
///
/// fn shorten<'long: 'short, 'short>(x: Invariant<'long>) -> Invariant<'short> {
///     x
/// }
/// ```
pub struct Invariant<'a>(PhantomData<UnsafeCell<&'a ()>>);
