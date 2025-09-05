use std::{marker::PhantomData, ops::RangeBounds};

use chumsky::{
    Parser,
    extension::v1::{Ext, ExtParser},
    input::InputRef,
    inspector::Inspector,
    prelude::any,
};

use crate::{
    chumsky_ext::extra::{GreenExtra, GreenState, Input},
    engine::Builder,
    language::Syntax,
};

pub trait BuilderParser<'src, 'cache, 'interner, O, Err, Sy>:
    Parser<'src, Input<'src>, O, GreenExtra<'cache, 'interner, Err, Sy>>
where
    Self::State: GreenState<'src, SyntaxKind = Sy>,
    Err: chumsky::error::Error<'src, &'src str> + 'src,
    Sy: Syntax,
    'interner: 'cache,
    'cache: 'src,
{
    type State;
    fn as_token(self, kind: Sy)
    -> impl BuilderParser<'src, 'cache, 'interner, (), Err, Sy> + Clone;
    fn as_static_token(
        self,
        kind: Sy,
    ) -> impl BuilderParser<'src, 'cache, 'interner, (), Err, Sy> + Clone;
    fn as_node(self, kind: Sy) -> impl BuilderParser<'src, 'cache, 'interner, (), Err, Sy> + Clone;
    fn wrap_cp(
        self,
        kind: <Self::State as GreenState<'src>>::SyntaxKind,
    ) -> impl BuilderParser<'src, 'cache, 'interner, (), Err, Sy> + Clone
    where
        Self: Parser<
                'src,
                Input<'src>,
                <Self::State as Inspector<'src, Input<'src>>>::Checkpoint,
                GreenExtra<'cache, 'interner, Err, Sy>,
            > + Clone;

    fn with_cp(
        self,
    ) -> impl BuilderParser<'src, 'cache, 'interner, cstree::build::Checkpoint, Err, Sy>;

    fn always(self) -> impl BuilderParser<'src, 'cache, 'interner, (), Err, Sy> + Clone;
}

impl<'src, 'cache, 'interner, O, P, Err, Sy> BuilderParser<'src, 'cache, 'interner, O, Err, Sy>
    for P
where
    P: chumsky::Parser<'src, &'src str, O, GreenExtra<'cache, 'interner, Err, Sy>> + Clone,
    Err: chumsky::error::Error<'src, &'src str> + 'src,
    'interner: 'cache,
    'cache: 'src,
    Sy: Syntax,
{
    type State = Builder<'cache, 'interner, Sy>;

    fn as_token(
        self,
        kind: Sy,
    ) -> impl BuilderParser<'src, 'cache, 'interner, (), Err, Sy> + Clone {
        as_token(self.to_slice(), kind)
    }

    fn as_static_token(
        self,
        kind: Sy,
    ) -> impl BuilderParser<'src, 'cache, 'interner, (), Err, Sy> + Clone {
        self.ignored().then(as_static_token_ext(kind)).ignored()
    }

    fn as_node(self, kind: Sy) -> impl BuilderParser<'src, 'cache, 'interner, (), Err, Sy> + Clone {
        with_cp_ext::<Sy>()
            .then(self)
            .map(|(checkpoint, _)| checkpoint)
            .wrap_cp(kind)
        //self.with_cp().wrap_cp(kind)
    }

    fn wrap_cp(self, kind: Sy) -> impl BuilderParser<'src, 'cache, 'interner, (), Err, Sy> + Clone
    where
        Self: Parser<
                'src,
                Input<'src>,
                cstree::build::Checkpoint,
                GreenExtra<'cache, 'interner, Err, Sy>,
            >,
    {
        self.map_with(move |checkpoint, extra| {
            let builder: &mut Builder<'_, '_, _> = extra.state();
            builder.start_node_at(checkpoint, kind);
            builder.finish_node();
        })
    }

    fn with_cp(
        self,
    ) -> impl BuilderParser<'src, 'cache, 'interner, cstree::build::Checkpoint, Err, Sy> {
        with_cp_ext::<Sy>()
            .then(self)
            .map(|(checkpoint, _)| checkpoint)
    }

    fn always(self) -> impl BuilderParser<'src, 'cache, 'interner, (), Err, Sy> + Clone {
        always(self)
    }
}

pub fn ranges<'src, 'cache, 'interner, Err, Sy, R>(
    ranges: &[R],
) -> impl BuilderParser<'src, 'cache, 'interner, char, Err, Sy> + Clone
where
    Err: chumsky::error::Error<'src, &'src str> + 'src,
    Sy: Syntax,
    R: RangeBounds<char>,
    'interner: 'cache,
    'cache: 'src,
{
    any().filter(move |c| ranges.iter().any(|range| range.contains(c)))
}

#[derive(Debug, Copy, Clone)]
pub struct WithCp_<Sy>(PhantomData<Sy>);

type WithCp<Sy> = Ext<WithCp_<Sy>>;

pub fn with_cp_ext<Sy>() -> WithCp<Sy> {
    Ext(WithCp_(PhantomData::<Sy>))
}

impl<'src, 'cache, 'interner, Err, Sy>
    ExtParser<
        'src,
        Input<'src>,
        <Builder<'cache, 'interner, Sy> as Inspector<'src, Input<'src>>>::Checkpoint,
        GreenExtra<'cache, 'interner, Err, Sy>,
    > for WithCp_<Sy>
where
    Err: chumsky::error::Error<'src, &'src str> + 'src,
    'cache: 'src,
    'interner: 'src,
    'interner: 'cache,
    Sy: Syntax + 'src,
{
    fn parse(
        &self,
        input: &mut InputRef<'src, '_, Input<'src>, GreenExtra<'cache, 'interner, Err, Sy>>,
    ) -> Result<<Builder<'cache, 'interner, Sy> as Inspector<'src, Input<'src>>>::Checkpoint, Err>
    {
        Ok(*input.save().inspector())
    }
}

#[derive(Debug, Copy, Clone)]
pub struct AsStaticToken_<Sy>(Sy);

pub type AsStaticToken<Sy> = Ext<AsStaticToken_<Sy>>;

pub fn as_static_token_ext<Sy>(kind: Sy) -> AsStaticToken<Sy> {
    Ext(AsStaticToken_(kind))
}

impl<'src, 'cache, 'interner, Err, Sy>
    ExtParser<'src, Input<'src>, (), GreenExtra<'cache, 'interner, Err, Sy>> for AsStaticToken_<Sy>
where
    Err: chumsky::error::Error<'src, &'src str> + 'src,
    'cache: 'src,
    'interner: 'src,
    Sy: Syntax + 'src,
{
    fn parse(
        &self,
        input: &mut InputRef<'src, '_, Input<'src>, GreenExtra<'cache, 'interner, Err, Sy>>,
    ) -> Result<(), Err> {
        let extra = input.state();
        Builder::<'cache, 'interner, Sy>::static_token(extra, self.0);
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

impl<'src, 'cache, 'interner, Err, Sy, P>
    ExtParser<'src, Input<'src>, (), GreenExtra<'cache, 'interner, Err, Sy>> for AsToken_<P, Sy>
where
    // our inner parser must itself be a valid BuilderParser
    P: BuilderParser<'src, 'cache, 'interner, &'src str, Err, Sy>,
    P: Clone,
    Err: chumsky::error::Error<'src, &'src str> + 'src,
    'cache: 'src,
    'interner: 'cache,
    Sy: Syntax + 'src,
{
    fn parse(
        &self,
        input: &mut InputRef<'src, '_, Input<'src>, GreenExtra<'cache, 'interner, Err, Sy>>,
    ) -> Result<(), Err> {
        input.parse(&self.inner).and_then(|slice| {
            let extra = input.state();
            Builder::<'cache, 'interner, Sy>::token(extra, self.kind, slice);
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

impl<'src, 'cache, 'interner, Err, Sy, P, O>
    ExtParser<'src, Input<'src>, (), GreenExtra<'cache, 'interner, Err, Sy>> for Always_<P, O>
where
    P: BuilderParser<'src, 'cache, 'interner, O, Err, Sy>,
    Err: chumsky::error::Error<'src, &'src str> + 'src,
    'cache: 'src,
    'interner: 'cache,
    Sy: Syntax + 'src,
{
    fn parse(
        &self,
        input: &mut InputRef<'src, '_, Input<'src>, GreenExtra<'cache, 'interner, Err, Sy>>,
    ) -> Result<(), Err> {
        match input.parse(&self.inner) {
            Ok(_) => Ok(()),
            Err(err) => Err(err),
        }
    }
}
