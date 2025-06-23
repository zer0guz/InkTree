use crate::error::Errors;

pub struct ErrorsOnly<E, I> {
    first: Option<E>,
    iter: I,
}

impl<T, E, I: Iterator<Item = Result<T, E>>> Iterator for ErrorsOnly<E, I> {
    type Item = E;

    fn next(&mut self) -> Option<Self::Item> {
        self.first
            .take()
            .or_else(|| self.iter.find_map(Result::err))
    }
}

impl<T, E, I: Iterator<Item = Result<T, E>>> From<ErrorsOnly<E, I>> for Vec<E> {
    fn from(errors_only: ErrorsOnly<E, I>) -> Self {
        errors_only.collect()
    }
}

pub struct AccumulateErros<I>(Option<I>);

impl<T, E, I: Iterator<Item = Result<T, E>>> Iterator for AccumulateErros<I> {
    type Item = Result<T, ErrorsOnly<E, I>>;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.0.as_mut()?.next()?.map_err(|e| ErrorsOnly {
            first: Some(e),
            iter: self.0.take().unwrap(),
        }))
    }
}

pub trait IntoErrorAccumulatingIterator: Iterator + Sized {
    fn accumulate_errors(self) -> AccumulateErros<Self>;
}

impl<T, E, I: Iterator<Item = Result<T, E>>> IntoErrorAccumulatingIterator for I {
    fn accumulate_errors(self) -> AccumulateErros<Self> {
        AccumulateErros(Some(self))
    }
}

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
}

impl<A> IteratorExt for A where A: Iterator {}

pub fn transpose_errors<I, E>(res: Result<I, E>) -> impl Iterator<Item = Result<I::Item, E>>
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
