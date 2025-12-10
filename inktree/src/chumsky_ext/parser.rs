use std::{marker::PhantomData, ops::RangeBounds};

use chumsky::{
    Parser,
    extension::v1::{Ext, ExtParser},
    extra::Full,
    input::InputRef,
    inspector::Inspector,
    prelude::any,
};

use crate::{
    Syntax,
    chumsky_ext::{
        Builder,
        extra::{GreenExtra, GreenState},
    },
};

type Input<'src> = &'src str;

pub trait BuilderParser<'src, 'cache, 'interner, 'borrow, O, Err, Sy>:
    Parser<'src, Input<'src>, O, GreenExtra<'cache, 'interner, 'borrow, Err, Sy>>
where
    Err: chumsky::error::Error<'src, &'src str>,
    Builder<'cache, 'interner, 'borrow, Sy>:
        Inspector<'src, &'src str, Checkpoint = cstree::build::Checkpoint>,
    'interner: 'cache,
    'borrow: 'interner,
    Sy: Syntax,
{
    type State;
    fn as_token(
        self,
        kind: Sy,
    ) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, Sy> + Clone;
    fn as_static_token(
        self,
        kind: Sy,
    ) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, Sy> + Clone;
    fn as_node(
        self,
        kind: Sy,
    ) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, Sy> + Clone;

    fn wrap_cp<'b>(
        self,
        kind: Sy,
    ) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, Sy> + Clone
    where
        Self: BuilderParser<'src, 'cache, 'interner, 'borrow, ::cstree::build::Checkpoint, Err, Sy>;

    fn with_cp(
        self,
    ) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, cstree::build::Checkpoint, Err, Sy> + Clone;

    fn always(self) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, Sy> + Clone;
}

impl<'src, 'cache, 'interner, 'borrow, O, P, Err, Sy>
    BuilderParser<'src, 'cache, 'interner, 'borrow, O, Err, Sy> for P
where
    P: chumsky::Parser<'src, &'src str, O, GreenExtra<'cache, 'interner, 'borrow, Err, Sy>> + Clone,
    Err: chumsky::error::Error<'src, &'src str>,
    Sy: Syntax,
    'interner: 'cache,
    'borrow: 'interner,
{
    type State = Builder<'cache, 'interner, 'borrow, Sy>;

    fn as_token(
        self,
        kind: Sy,
    ) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, Sy> + Clone {
        as_token(self.to_slice(), kind)
    }

    fn as_static_token(
        self,
        kind: Sy,
    ) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, Sy> + Clone {
        self.ignored().then(as_static_token_ext(kind)).ignored()
    }

    fn as_node(
        self,
        kind: Sy,
    ) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, Sy> + Clone {
        with_cp_ext::<Sy>()
            .then(self)
            .map(|(checkpoint, _)| checkpoint)
            .wrap_cp(kind)
    }

    fn wrap_cp<'b>(
        self,
        kind: Sy,
    ) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, Sy> + Clone
    where
        Self: BuilderParser<'src, 'cache, 'interner, 'borrow, cstree::build::Checkpoint, Err, Sy>
            + Clone,
    {
        wrap_cp(self, kind)
    }

    fn with_cp(
        self,
    ) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, cstree::build::Checkpoint, Err, Sy> + Clone
    {
        with_cp_ext::<Sy>()
            .then(self)
            .map(|(checkpoint, _)| checkpoint)
    }

    fn always(self) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, Sy> + Clone {
        always(self)
    }
}

pub fn ranges<'src, 'cache, 'interner, 'borrow, Err, Sy, R>(
    ranges: &[R],
) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, char, Err, Sy> + Clone
where
    Err: chumsky::error::Error<'src, &'src str>,
    Sy: Syntax,
    R: RangeBounds<char>,
    'interner: 'cache,
    'borrow: 'cache,
    'borrow: 'interner,
{
    any().filter(move |c| ranges.iter().any(move |range| range.contains(c)))
}

#[derive(Debug, Copy, Clone)]
pub struct WithCp_<Sy>(PhantomData<Sy>);

type WithCp<Sy> = Ext<WithCp_<Sy>>;

pub fn with_cp_ext<Sy>() -> WithCp<Sy> {
    Ext(WithCp_(PhantomData::<Sy>))
}

impl<'src, 'cache, 'interner, 'borrow, Err, Sy>
    ExtParser<
        'src,
        Input<'src>,
        cstree::build::Checkpoint,
        Full<Err, Builder<'cache, 'interner, 'borrow, Sy>, ()>,
    > for WithCp_<Sy>
where
    Err: chumsky::error::Error<'src, &'src str>,
    Sy: Syntax,
{
    fn parse<'parse>(
        &self,
        input: &mut InputRef<
            'src,
            'parse,
            Input<'src>,
            GreenExtra<'cache, 'interner, 'borrow, Err, Sy>,
        >,
    ) -> Result<cstree::build::Checkpoint, Err> {
        Ok(*input.save().inspector())
    }
}

#[derive(Debug, Copy, Clone)]
pub struct AsStaticToken_<Sy>(Sy);

pub type AsStaticToken<Sy> = Ext<AsStaticToken_<Sy>>;

pub fn as_static_token_ext<Sy>(kind: Sy) -> AsStaticToken<Sy> {
    Ext(AsStaticToken_(kind))
}

impl<'src, 'cache, 'interner, 'borrow, Err, Sy>
    ExtParser<'src, Input<'src>, (), GreenExtra<'cache, 'interner, 'borrow, Err, Sy>>
    for AsStaticToken_<Sy>
where
    Err: chumsky::error::Error<'src, &'src str>,
    Sy: Syntax,
{
    fn parse(
        &self,
        input: &mut InputRef<
            'src,
            '_,
            Input<'src>,
            GreenExtra<'cache, 'interner, 'borrow, Err, Sy>,
        >,
    ) -> Result<(), Err> {
        let extra = input.state();
        Builder::<'cache, 'interner, 'borrow, Sy>::static_token(extra, self.0);
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct AsToken_<P, Sy> {
    inner: P,
    kind: Sy,
}

pub type AsToken<P, Sy> = Ext<AsToken_<P, Sy>>;

// constructor: take an inner parser and a kind
pub fn as_token<P, Sy>(inner: P, kind: Sy) -> AsToken<P, Sy> {
    Ext(AsToken_ { inner, kind })
}

impl<'src, 'cache, 'interner, 'borrow, Err, Sy, P>
    ExtParser<'src, Input<'src>, (), GreenExtra<'cache, 'interner, 'borrow, Err, Sy>>
    for AsToken_<P, Sy>
where
    Err: chumsky::error::Error<'src, &'src str>,
    P: BuilderParser<'src, 'cache, 'interner, 'borrow, &'src str, Err, Sy>,
    Sy: Syntax,
{
    fn parse(
        &self,
        input: &mut InputRef<
            'src,
            '_,
            Input<'src>,
            GreenExtra<'cache, 'interner, 'borrow, Err, Sy>,
        >,
    ) -> Result<(), Err> {
        input.parse(&self.inner).and_then(|slice| {
            let extra = input.state();
            Builder::<'cache, 'interner, 'borrow, Sy>::token(extra, self.kind, slice);
            Ok(())
        })
    }
}

#[derive(Debug, Clone)]
pub struct WrapCp_<P, Sy> {
    inner: P,
    kind: Sy,
}

pub type WrapCp<P, Sy> = Ext<WrapCp_<P, Sy>>;

// constructor: take an inner parser and a kind
pub fn wrap_cp<P, Sy>(inner: P, kind: Sy) -> WrapCp<P, Sy> {
    Ext(WrapCp_ { inner, kind })
}

impl<'src, 'cache, 'interner, 'borrow, Err, Sy, P>
    ExtParser<'src, Input<'src>, (), GreenExtra<'cache, 'interner, 'borrow, Err, Sy>>
    for WrapCp_<P, Sy>
where
    Err: chumsky::error::Error<'src, &'src str>,
    P: BuilderParser<'src, 'cache, 'interner, 'borrow, cstree::build::Checkpoint, Err, Sy>,
    Sy: Syntax,
{
    fn parse(
        &self,
        input: &mut InputRef<
            'src,
            '_,
            Input<'src>,
            GreenExtra<'cache, 'interner, 'borrow, Err, Sy>,
        >,
    ) -> Result<(), Err> {
        input.parse(&self.inner).and_then(|checkpoint| {
            let extra = input.state();
            Builder::start_node_at(extra, checkpoint, self.kind);
            Builder::finish_node(extra);
            Ok(())
        })
    }
}

#[derive(Debug)]
pub struct Always_<P, O> {
    inner: P,
    _output: PhantomData<O>,
}

impl<P, O> Clone for Always_<P, O>
where
    P: Clone,
{
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            _output: self._output.clone(),
        }
    }
}

pub type Always<P, Sy> = Ext<Always_<P, Sy>>;

pub fn always<P, O>(inner: P) -> Always<P, O> {
    Ext(Always_ {
        inner,
        _output: PhantomData::<O>,
    })
}

impl<'src, 'cache, 'interner, 'borrow, Err, Sy, P, O>
    ExtParser<'src, Input<'src>, (), Full<Err, Builder<'cache, 'interner, 'borrow, Sy>, ()>>
    for Always_<P, O>
where
    P: BuilderParser<'src, 'cache, 'interner, 'borrow, O, Err, Sy>,
    Err: chumsky::error::Error<'src, &'src str>,
    Sy: Syntax,
{
    fn parse(
        &self,
        input: &mut InputRef<
            'src,
            '_,
            Input<'src>,
            GreenExtra<'cache, 'interner, 'borrow, Err, Sy>,
        >,
    ) -> Result<(), Err> {
        match input.parse(&self.inner) {
            Ok(_) => Ok(()),
            Err(err) => Err(err),
        }
    }
}
