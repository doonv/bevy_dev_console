use std::{rc::Rc, ops::{DerefMut, Deref}, cell::RefCell};

struct UniqueRc<T: ?Sized>(Rc<T>);
impl<T: ?Sized> UniqueRc<T> {
  pub fn into_rc(self) -> Rc<T> {
    self.0
  }
}
impl<T> UniqueRc<T> {
  pub fn into_inner(self) -> T {
    // SAFETY: There is only ever one owner of Rc
    unsafe { Rc::try_unwrap(self.0).unwrap_unchecked() }
  }
}

impl<T> Deref for UniqueRc<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}
impl<T> DerefMut for UniqueRc<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // SAFETY: There is only ever one owner of Rc
        unsafe { Rc::get_mut(&mut self.0).unwrap_unchecked() }
    }
}