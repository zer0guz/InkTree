use std::marker::PhantomData;

use chumsky::{
    Parser,
    extension::v1::{Ext, ExtParser},
    input::InputRef,
};

use crate::{
    Builder, Syntax,
    chumksy_ext::{
        Input,
        extra::{GreenExtra, GreenState},
    },
};

pub trait BuilderParser<'src, 'cache, 'interner, O, Err, St>:
    Parser<'src, Input<'src>, O, GreenExtra<'cache, 'interner, Err, St::SyntaxKind>>
where
    St: GreenState<'src>,
    Err: chumsky::error::Error<'src, &'src str> + 'src,
    <St as GreenState<'src>>::SyntaxKind: Syntax,
    'interner: 'cache,
    'cache: 'src,
{
    fn as_token(
        self,
        kind: St::SyntaxKind,
    ) -> impl Parser<'src, Input<'src>, (), GreenExtra<'cache, 'interner, Err, St::SyntaxKind>>;
    fn as_static_token(
        self,
        kind: St::SyntaxKind,
    ) -> impl Parser<'src, Input<'src>, (), GreenExtra<'cache, 'interner, Err, St::SyntaxKind>>;
    fn as_node(
        self,
        kind: St::SyntaxKind,
    ) -> impl Parser<'src, Input<'src>, (), GreenExtra<'cache, 'interner, Err, St::SyntaxKind>>;
    fn wrap_cp(
        parser: impl Parser<
            'src,
            Input<'src>,
            St::Checkpoint,
            GreenExtra<'cache, 'interner, Err, St::SyntaxKind>,
        >,
        kind: St::SyntaxKind,
    ) -> impl Parser<'src, Input<'src>, (), GreenExtra<'cache, 'interner, Err, St::SyntaxKind>>;
    fn with_cp(
        self,
    ) -> impl Parser<'src, Input<'src>, St::Checkpoint, GreenExtra<'cache, 'interner, Err, St::SyntaxKind>>;
}

impl<'src, 'cache, 'interner, O, P, Err, Sy>
    BuilderParser<'src, 'cache, 'interner, O, Err, Builder<'cache, 'interner, Sy>> for P
where
    P: chumsky::Parser<'src, &'src str, O, GreenExtra<'cache, 'interner, Err, Sy>>,
    Err: chumsky::error::Error<'src, &'src str> + 'src,
    'interner: 'cache,
    'cache: 'src,
    Sy: Syntax,
{
    fn as_token(
        self,
        kind: Sy,
    ) -> impl Parser<'src, Input<'src>, (), GreenExtra<'cache, 'interner, Err, Sy>> {
        self.to_slice().map_with(move |slice, extra| {
            Builder::<'cache, 'interner, Sy>::token(extra.state(), kind, slice);
        })
    }

    fn as_static_token(
        self,
        kind: Sy,
    ) -> impl Parser<'src, Input<'src>, (), GreenExtra<'cache, 'interner, Err, Sy>> {
        self.ignored().map_with(move |_, extra| {
            Builder::<'cache, 'interner, Sy>::static_token(extra.state(), kind);
        })
    }

    fn as_node(
        self,
        kind: Sy,
    ) -> impl Parser<'src, Input<'src>, (), GreenExtra<'cache, 'interner, Err, Sy>> {
        Self::wrap_cp(self.with_cp(), kind)
    }

    fn wrap_cp(
        parser: impl Parser<
            'src,
            Input<'src>,
            cstree::build::Checkpoint,
            GreenExtra<'cache, 'interner, Err, Sy>,
        >,
        kind: Sy,
    ) -> impl Parser<'src, Input<'src>, (), GreenExtra<'cache, 'interner, Err, Sy>> {
        parser.map_with(move |checkpoint, extra| {
            let builder = extra.state();
            builder.start_node_at(checkpoint, kind);
            builder.finish_node();
        })
    }

    fn with_cp(
        self,
    ) -> impl Parser<'src, Input<'src>, cstree::build::Checkpoint, GreenExtra<'cache, 'interner, Err, Sy>>
    {
        with_cp::<Sy>().then(self).map(|(checkpoint, _)| checkpoint)
    }
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
