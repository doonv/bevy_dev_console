use std::{
    cell::{Cell, Ref, RefCell, RefMut},
    fmt::Debug,
    ops::{Deref, DerefMut},
    rc::{Rc, Weak},
};

/// A uniquely owned [`Rc`] with interior mutability. Interior mutability is abstracted away with [`WeakRef`].
///
/// This represents an [`Rc`] that is known to be uniquely owned -- that is, have exactly one strong
/// reference.
///
/// **TODO:** This is actually going to be a standard library feature. Use [`alloc::rc::UniqueRc`] when it is stabilized.
pub struct UniqueRc<T: ?Sized>(Rc<RefCell<T>>);
impl<T: ?Sized> UniqueRc<T> {
    /// Get a reference to the inner [`Rc`] of [`UniqueRc`].
    ///
    /// # Safety
    ///
    /// This function is unsafe because it allows direct access to the [`Rc`].
    /// If cloned then the gurantee that there is only ever one strong reference is no longer satisfied.
    unsafe fn get_rc(&self) -> &Rc<RefCell<T>> {
        &self.0
    }
    pub fn borrow_inner(&self) -> &RefCell<T> {
        &self.0
    }
    pub fn borrow(&self) -> WeakRef<T> {
        WeakRef::new(self)
    }
}
impl<T> UniqueRc<T> {
    /// Create a new [`UniqueRc`].
    pub fn new(value: T) -> UniqueRc<T> {
        UniqueRc(Rc::new(RefCell::new(value)))
    }
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

#[derive(Debug)]
pub struct WeakRef<T: ?Sized> {
    reference: Weak<RefCell<T>>,
    upgraded: Cell<bool>,
}
impl<T: ?Sized> WeakRef<T> {
    fn new(unique_rc: &UniqueRc<T>) -> Self {
        // SAFETY: We are not cloning the `Rc`, so this is fine.
        let rc = unsafe { unique_rc.get_rc() };
        Self {
            reference: Rc::downgrade(rc),
            upgraded: Cell::new(false),
        }
    }
    pub fn upgrade(&self) -> Option<StrongRef<T>> {
        if !self.upgraded.get() {
            self.upgraded.set(true);
            unsafe { self.upgrade_unchecked() }
        } else {
            None
        }
    }
    unsafe fn upgrade_unchecked(&self) -> Option<StrongRef<T>> {
        Some(StrongRef(self.reference.upgrade()?))
    }
}

/// A reference to value `T`.
///
/// This value is *technically* unsafe, but in practice the only way
/// you could obtain it is by having it passed into a custom function.
///
/// ```
/// use bevy_dev_console::builtin_parser::{Value, StrongRef};
///
/// fn add_to_reference(my_reference: StrongRef<Value>, add: String) {
///     // currently you can only do it with `Value`
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
    fn weak_ref_upgrade_once() {
        let rc = UniqueRc::new(0);

        let weak = rc.borrow();

        assert!(weak.upgrade().is_some());
        assert!(weak.upgrade().is_none());
        assert!(weak.upgrade().is_none());
    }

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
