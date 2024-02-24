use std::cell::{Ref, RefCell, RefMut};
use std::fmt::Debug;
use std::ops::{Deref, DerefMut};
use std::rc::{Rc, Weak};

/// A uniquely owned [`Rc`] with interior mutability. Interior mutability is abstracted away with [`WeakRef`].
///
/// This represents an [`Rc`] that is known to be uniquely owned -- that is, have exactly one strong
/// reference.
///
/// **TODO:** This is actually going to be a standard library feature. Use [`alloc::rc::UniqueRc`] when it is stabilized.
#[derive(Debug)]
pub struct UniqueRc<T: ?Sized>(Rc<RefCell<T>>);
impl<T: ?Sized> UniqueRc<T> {
    /// Get a reference to the inner [`Rc`] of [`UniqueRc`].
    ///
    /// # Safety
    ///
    /// This function is unsafe because it allows direct access to the [`Rc`].
    /// If cloned then the guarantee that there is only ever one strong reference is no longer satisfied.
    const unsafe fn get_rc(&self) -> &Rc<RefCell<T>> {
        &self.0
    }
    pub(crate) fn borrow_inner(&self) -> &RefCell<T> {
        &self.0
    }
    /// Create a new weak pointer to this [`UniqueRc`].
    pub fn borrow(&self) -> WeakRef<T> {
        WeakRef::new(self)
    }
}
impl<T> UniqueRc<T> {
    /// Create a new [`UniqueRc`].
    pub fn new(value: T) -> UniqueRc<T> {
        UniqueRc(Rc::new(RefCell::new(value)))
    }
    /// Get the inner value (`T`) of this [`UniqueRc<T>`].
    pub fn into_inner(self) -> T {
        Rc::try_unwrap(self.0)
            .unwrap_or_else(|rc| {
                panic!(
                    "There are {} strong pointers to a UniqueRc!",
                    Rc::strong_count(&rc)
                )
            })
            .into_inner()
    }
}
impl<T: ?Sized + Clone> Clone for UniqueRc<T> {
    fn clone(&self) -> Self {
        let t = self.borrow_inner().clone().into_inner();

        Self::new(t)
    }
}

impl<T> Deref for UniqueRc<T> {
    type Target = RefCell<T>;
    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}
impl<T> DerefMut for UniqueRc<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        Rc::get_mut(&mut self.0).unwrap()
    }
}

/// A weak reference to a [`UniqueRc`] that may or may not exist.
#[derive(Debug)]
pub struct WeakRef<T: ?Sized> {
    reference: Weak<RefCell<T>>,
}
impl<T: ?Sized> Clone for WeakRef<T> {
    fn clone(&self) -> Self {
        Self {
            reference: self.reference.clone(),
        }
    }
}
impl<T: ?Sized> WeakRef<T> {
    fn new(unique_rc: &UniqueRc<T>) -> Self {
        // SAFETY: We are not cloning the `Rc`, so this is fine.
        let rc = unsafe { unique_rc.get_rc() };
        Self {
            reference: Rc::downgrade(rc),
        }
    }
    /// Converts this [`WeakRef`] into a [`StrongRef`] (may be unsafe, see [`StrongRef`]'s documentation).
    pub fn upgrade(&self) -> Option<StrongRef<T>> {
        Some(StrongRef(self.reference.upgrade()?))
    }
}

/// A strong reference to value `T`.
///
/// This value is *technically* unsafe due to [`UniqueRc`] expecting only one strong reference to its inner value.
/// However in practice the only way you could obtain it is by having it passed into a custom function.
/// In which case it is safe (probably).
///
/// ```
/// use bevy_dev_console::builtin_parser::{Value, StrongRef};
///
/// fn add_to_reference(my_reference: StrongRef<Value>, add: String) {
///     // currently you can only do it with `Value` (TODO)
///     if let Value::String(string) = &mut *my_reference.borrow_mut() {
///         *string += &add;
///     } else {
///         todo!();
///     }
/// }
/// ```
#[derive(Debug)]
pub struct StrongRef<T: ?Sized>(Rc<RefCell<T>>);
impl<T: ?Sized> StrongRef<T> {
    /// Immutably borrows the wrapped value.
    pub fn borrow(&self) -> Ref<T> {
        self.0.borrow()
    }
    /// Mutably borrows the wrapped value.
    pub fn borrow_mut(&self) -> RefMut<T> {
        self.0.borrow_mut()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[should_panic]
    fn strong_ref_panic() {
        let rc = UniqueRc::new(0);

        let weak = rc.borrow();

        let strong = weak.upgrade().unwrap();

        println!("{}", rc.into_inner()); // Panic!

        println!("{}", strong.borrow());
    }
}
