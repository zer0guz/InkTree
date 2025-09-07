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
    chumsky_ext::{
        Input,
        extra::{GreenExtra, GreenState},
    },
    engine::Builder,
    language::Syntax,
};

pub trait BuilderParser<'src, 'cache, 'interner, 'borrow, O, Err, Sy>:
    Parser<'src, Input<'src>, O, Full<Err, Builder<'cache, 'interner, 'borrow, Sy>, ()>>
where
    Builder<'cache, 'interner, 'borrow, Sy>:
        Inspector<'src, &'src str> + GreenState<'src, Sy> + 'src,
    Err: chumsky::error::Error<'src, &'src str> + 'src,
    Sy: Syntax,
    'interner: 'cache,
    'borrow: 'interner,
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

    fn wrap_cp(
        self,
        kind: Sy,
    ) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, Sy> + Clone
    where
        Self: Parser<
                'src,
                Input<'src>,
                cstree::build::Checkpoint,
                GreenExtra<'cache, 'interner, 'borrow, Err, Sy>,
            > + Clone,
        <Self as BuilderParser<'src, 'cache, 'interner, 'borrow, O, Err, Sy>>::State:
            Inspector<'src, &'src str>;

    fn with_cp(
        self,
    ) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, cstree::build::Checkpoint, Err, Sy>;

    fn always(self) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, Sy> + Clone;
}

impl<'src, 'cache, 'interner, 'borrow, O, P, Err, Sy>
    BuilderParser<'src, 'cache, 'interner, 'borrow, O, Err, Sy> for P
where
    P: chumsky::Parser<'src, &'src str, O, GreenExtra<'cache, 'interner, 'borrow, Err, Sy>> + Clone,
    Builder<'cache, 'interner, 'borrow, Sy>: GreenState<'src, Sy> + 'src,
    Builder<'cache, 'interner, 'borrow, Sy>:
        Inspector<'src, Input<'src>, Checkpoint = cstree::build::Checkpoint> + 'src,
    Err: chumsky::error::Error<'src, &'src str> + 'src,
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
        //self.with_cp().wrap_cp(kind)
    }

    fn wrap_cp(
        self,
        kind: Sy,
    ) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, Sy> + Clone
    where
        Self: BuilderParser<'src, 'cache, 'interner, 'borrow, cstree::build::Checkpoint, Err, Sy>
            + Clone,
    {
        self.map_with(move |checkpoint, extra| {
            let builder: &mut Builder<'_, '_, '_, Sy> = extra.state();
            builder.start_node_at(checkpoint, kind);
            builder.finish_node();
        })
    }

    fn with_cp(
        self,
    ) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, cstree::build::Checkpoint, Err, Sy>
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
    Err: chumsky::error::Error<'src, &'src str> + 'src,
    Sy: Syntax,
    R: RangeBounds<char>,
    'interner: 'cache,
    'borrow: 'interner,
    'cache: 'src,
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
    Err: chumsky::error::Error<'src, &'src str> + 'src,
    Builder<'cache, 'interner, 'borrow, Sy>: GreenState<'src, Sy> + 'src,
    Builder<'cache, 'interner, 'borrow, Sy>:
        Inspector<'src, Input<'src>, Checkpoint = cstree::build::Checkpoint> + 'src,
    Sy: Syntax,
    'interner: 'cache,
    'borrow: 'cache,
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
    Err: chumsky::error::Error<'src, &'src str> + 'src,
    'cache: 'src,
    Sy: Syntax + 'src,
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
    // our inner parser must itself be a valid BuilderParser
    P: BuilderParser<'src, 'cache, 'interner, 'borrow, &'src str, Err, Sy>,
    P: Clone,
    Err: chumsky::error::Error<'src, &'src str> + 'src,
    'cache: 'src,
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
    P: Clone,
    Err: chumsky::error::Error<'src, &'src str> + 'src,
    'cache: 'src,
    Sy: Syntax + 'src,
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
