use std::{marker::PhantomData, ops::{Range, RangeBounds}};

use chumsky::{
    Parser,
    extension::v1::{Ext, ExtParser},
    input::InputRef,
    inspector::Inspector,
    prelude::any,
};

use crate::{
    chumksy_ext::{
        extra::{GreenExtra, GreenState}, Input
    }, engine::Builder, language::Syntax
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
    fn as_token(self, kind: Sy) -> impl BuilderParser<'src, 'cache, 'interner, (), Err, Sy>;
    fn as_static_token(self, kind: Sy) -> impl BuilderParser<'src, 'cache, 'interner, (), Err, Sy>;
    fn as_node(self, kind: Sy) -> impl BuilderParser<'src, 'cache, 'interner, (), Err, Sy>;
    fn wrap_cp(
        parser: impl Parser<
            'src,
            Input<'src>,
            <Self::State as Inspector<'src, Input<'src>>>::Checkpoint,
            GreenExtra<'cache, 'interner, Err, Sy>,
        >,
        kind: <Self::State as GreenState<'src>>::SyntaxKind,
    ) -> impl BuilderParser<'src, 'cache, 'interner, (), Err, Sy>;
    fn with_cp(
        self,
    ) -> impl BuilderParser<
        'src,
        'cache,
        'interner,
        <Self::State as Inspector<'src, Input<'src>>>::Checkpoint,
        Err,
        Sy,
    >;
}

impl<'src, 'cache, 'interner, O, P, Err, Sy> BuilderParser<'src, 'cache, 'interner, O, Err, Sy>
    for P
where
    P: chumsky::Parser<'src, &'src str, O, GreenExtra<'cache, 'interner, Err, Sy>>,
    Err: chumsky::error::Error<'src, &'src str> + 'src,
    'interner: 'cache,
    'cache: 'src,
    Sy: Syntax,
{
    type State = Builder<'cache, 'interner, Sy>;

    fn as_token(self, kind: Sy) -> impl BuilderParser<'src, 'cache, 'interner, (), Err, Sy> {
        self.to_slice().map_with(move |slice, extra| {
            Builder::<'cache, 'interner, Sy>::token(extra.state(), kind, slice);
        })
    }

    fn as_static_token(self, kind: Sy) -> impl BuilderParser<'src, 'cache, 'interner, (), Err, Sy> {
        self.ignored().map_with(move |_, extra| {
            Builder::<'cache, 'interner, Sy>::static_token(extra.state(), kind);
        })
    }

    fn as_node(self, kind: Sy) -> impl BuilderParser<'src, 'cache, 'interner, (), Err, Sy> {
        Self::wrap_cp(self.with_cp(), kind)
    }

    fn wrap_cp(
        parser: impl BuilderParser<
            'src,
            'cache,
            'interner,
            <Self::State as Inspector<'src, Input<'src>>>::Checkpoint,
            Err,
            Sy,
        >,
        kind: Sy,
    ) -> impl BuilderParser<'src, 'cache, 'interner, (), Err, Sy> {
        parser.map_with(move |checkpoint, extra| {
            let builder = extra.state();
            builder.start_node_at(checkpoint, kind);
            builder.finish_node();
        })
    }

    fn with_cp(
        self,
    ) -> impl BuilderParser<
        'src,
        'cache,
        'interner,
        <Self::State as Inspector<'src, Input<'src>>>::Checkpoint,
        Err,
        Sy,
    > {
        with_cp::<Sy>().then(self).map(|(checkpoint, _)| checkpoint)
    }
}

pub fn ranges<'src, 'cache, 'interner, Err, Sy,R>(
    ranges: &[R]
) -> impl BuilderParser<'src, 'cache, 'interner, char, Err, Sy>
where
    Err: chumsky::error::Error<'src, &'src str> + 'src,
    Sy: Syntax,
    R: RangeBounds<char>,
    'interner: 'cache,
    'cache: 'src,
{
    any().filter(move |c| {
        ranges.iter().any(|range| range.contains(c))
    })
}

#[derive(Debug, Copy, Clone)]
struct WithCp_<Sy>(PhantomData<Sy>);

type WithCp<Sy> = Ext<WithCp_<Sy>>;

fn with_cp<Sy>() -> WithCp<Sy> {
    Ext(WithCp_(PhantomData::<Sy>))
}

impl<'src, 'cache, 'interner, Err, Sy>
    ExtParser<'src, Input<'src>, cstree::build::Checkpoint, GreenExtra<'cache, 'interner, Err, Sy>>
    for WithCp_<Sy>
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
    ) -> Result<cstree::build::Checkpoint, Err> {
        Ok(*input.save().inspector())
    }
}
