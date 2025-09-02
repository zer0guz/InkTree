use std::{slice::Iter, vec::IntoIter};

use derivative::Derivative;

use crate::error::Errors;
pub trait IteratorExt: Iterator {
    fn collect_either<T, E>(mut self) -> Result<Vec<T>, Errors<E>>
    where
        Self: Sized + Iterator<Item = Result<T, E>>,
        E: snafu::Error,
    {
        self.by_ref()
            .try_fold(Vec::new(), |mut xs: Vec<T>, xy: Result<T, E>| {
                xs.push(xy.map_err(|y| Vec::from([y]))?);
                Ok(xs)
            })
            .map_err(|mut ys: Vec<E>| {
                ys.extend(self.filter_map(Result::err));
                ys.into()
            })
    }
    fn collect_either_flatten<T, E>(mut self) -> Result<Vec<T>, Errors<E>>
    where
        Self: Sized + Iterator<Item = Result<T, Errors<E>>>,
        E: snafu::Error,
    {
        self.by_ref()
            .try_fold(Vec::new(), |mut xs: Vec<T>, xy: Result<T, Errors<E>>| {
                xs.push(xy.map_err(|y| Vec::from(y.0))?);
                Ok(xs)
            })
            .map_err(|mut ys: Vec<E>| {
                ys.extend(
                    self.filter_map(Result::err)
                        .into_iter()
                        .flat_map(|errs| errs.0),
                );
                ys.into()
            })
    }
    fn collect_either_flatten_into<T, E, U>(mut self) -> Result<Vec<T>, Errors<U>>
    where
        Self: Sized + Iterator<Item = Result<T, Errors<E>>>,
        E: snafu::Error,
        U: From<E> + snafu::Error,
    {
        self.by_ref()
            .try_fold(Vec::new(), |mut xs: Vec<T>, xy: Result<T, Errors<E>>| {
                xs.push(xy.map_err(|y| Vec::from(y.0))?);
                Ok(xs)
            })
            .map_err(|mut ys: Vec<E>| {
                ys.extend(
                    self.filter_map(Result::err)
                        .into_iter()
                        .flat_map(|errs| errs.0),
                );
                ys.into_iter().map(|err| U::from(err)).collect()
            })
    }
}

impl<A> IteratorExt for A where A: Iterator {}

pub fn _transpose_errors<I, E>(res: Result<I, E>) -> impl Iterator<Item = Result<I::Item, E>>
where
    I: IntoIterator,
{
    enum Either<L, R> {
        Left(L),
        Right(R),
    }
    let mut either = match res {
        Ok(iterable) => Either::Left(iterable.into_iter()),
        Err(e) => Either::Right(::core::iter::once(e)),
    };
    ::core::iter::from_fn(move || match &mut either {
        Either::Left(i) => i.next().map(Ok),
        Either::Right(e) => e.next().map(Err),
    })
}

pub struct Pool<T>(Vec<T>);

impl<T> Pool<T> {
    pub fn with_capacity(cap: u32) -> Self {
        Self(Vec::with_capacity(cap as usize))
    }

    #[inline(always)]
    pub fn push(&mut self, entry: T) -> Handle<T> {
        let entry_handle = self.0.len() as u32;
        self.0.push(entry);
        Handle::<T>::new(entry_handle)
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn iter(&self) -> PoolIter<'_, T> {
        PoolIter {
            inner: self.0.iter().enumerate(),
        }
    }
    pub fn len(&self) -> u32 {
        self.0.len() as u32
    }

    pub fn next_handle(&self) -> Handle<T> {
        Handle::new(self.len())
    }
}

#[derive(Derivative)]
#[derivative(Debug, Default, Clone, Copy, Hash, Eq, PartialEq)]
pub struct Handle<T> {
    idx: u32,
    phantom: std::marker::PhantomData<T>,
}

impl<T> Handle<T> {
    #[inline(always)]
    fn new(idx: u32) -> Self {
        Self {
            idx,
            phantom: std::marker::PhantomData::<T> {},
        }
    }
}

impl<T> std::ops::Index<Handle<T>> for Pool<T> {
    type Output = T;

    #[inline(always)]
    fn index(&self, handle: Handle<T>) -> &Self::Output {
        self.0.get(handle.idx as usize).expect("pool error")
    }
}

impl<T> std::ops::IndexMut<Handle<T>> for Pool<T> {
    #[inline(always)]
    fn index_mut(&mut self, handle: Handle<T>) -> &mut Self::Output {
        self.0.get_mut(handle.idx as usize).expect("pool error")
    }
}

pub struct PoolIntoIter<T> {
    inner: std::iter::Enumerate<IntoIter<T>>,
}

impl<T> Iterator for PoolIntoIter<T> {
    type Item = (Handle<T>, T);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .next()
            .map(|(idx, item)| (Handle::new(idx as u32), item))
    }
}

impl<T> IntoIterator for Pool<T> {
    type Item = (Handle<T>, T);
    type IntoIter = PoolIntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        PoolIntoIter {
            inner: self.0.into_iter().enumerate(),
        }
    }
}

pub struct PoolIter<'a, T> {
    inner: std::iter::Enumerate<Iter<'a, T>>,
}

impl<'a, T> Iterator for PoolIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(_, t)| t)
    }
}

impl<'a, T> IntoIterator for &'a Pool<T> {
    type Item = &'a T;
    type IntoIter = PoolIter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        PoolIter {
            inner: self.0.iter().enumerate(),
        }
    }
}
