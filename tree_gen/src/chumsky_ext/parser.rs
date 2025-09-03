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
        self.to_slice().map_with(move |slice, extra| {
            Builder::<'cache, 'interner, Sy>::token(extra.state(), kind, slice);
        })
    }

    fn as_static_token(
        self,
        kind: Sy,
    ) -> impl BuilderParser<'src, 'cache, 'interner, (), Err, Sy> + Clone {
        self.ignored().map_with(move |_, extra| {
            Builder::<'cache, 'interner, Sy>::static_token(extra.state(), kind);
        })
    }

    fn as_node(self, kind: Sy) -> impl BuilderParser<'src, 'cache, 'interner, (), Err, Sy> + Clone {
        with_cp::<Sy>()
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
        with_cp::<Sy>().then(self).map(|(checkpoint, _)| checkpoint)
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

pub fn with_cp<Sy>() -> WithCp<Sy> {
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
