use std::marker::PhantomData;

use chumsky::{
    Parser,
    extension::v1::{Ext, ExtParser},
    extra::ParserExtra,
    input::InputRef,
};

use crate::chumksy::{Input, extra::BuilderExtra};

pub trait BuilderParser<'src, O, E>: Parser<'src, Input<'src>, O, E>
where
    E: BuilderExtra<'src>,
{
    fn as_token(self, kind: E::SyntaxKind) -> impl Parser<'src, Input<'src>, (), E>;
    fn as_static_token(self, kind: E::SyntaxKind) -> impl Parser<'src, Input<'src>, (), E>;
    fn as_node(self, kind: E::SyntaxKind) -> impl Parser<'src, Input<'src>, (), E>;
    fn wrap_cp(
        parser: impl Parser<'src, Input<'src>, E::Checkpoint, E>,
        kind: E::SyntaxKind,
    ) -> impl Parser<'src, Input<'src>, (), E>;
    fn with_cp(self) -> impl Parser<'src, Input<'src>, E::Checkpoint, E>;
}

impl<'src, O, P, E> BuilderParser<'src, O, E> for P
where
    E: BuilderExtra<'src>,
    P: chumsky::Parser<'src, &'src str, O, E>,
{
    fn as_token(self, kind: E::SyntaxKind) -> impl Parser<'src, Input<'src>, (), E> {
        self.to_slice().map_with(move |slice, extra| {
            BuilderExtra::token(extra.state(), kind, Some(slice));
        })
    }
    fn as_static_token(self, kind: E::SyntaxKind) -> impl Parser<'src, Input<'src>, (), E> {
        self.ignored().map_with(move |_, extra| {
            BuilderExtra::token(extra.state(), kind, None);
        })
    }
    fn with_cp(self) -> impl Parser<'src, Input<'src>, E::Checkpoint, E> {
        with_cp::<E::SyntaxKind>()
            .then(self)
            .map(|(checkpoint, _)| checkpoint)
    }
    fn as_node(self, kind: E::SyntaxKind) -> impl Parser<'src, Input<'src>, (), E> {
        Self::wrap_cp(self.with_cp(), kind)
    }
    fn wrap_cp(
        parser: impl Parser<'src, Input<'src>, E::Checkpoint, E>,
        kind: E::SyntaxKind,
    ) -> impl Parser<'src, Input<'src>, (), E> {
        parser.map_with(move |checkpoint, extra| {
            let builder = extra.state();
            builder.start_node_at(checkpoint, kind);
            builder.finish_node();
        })
    }
}

#[derive(Debug, Copy, Clone)]
struct WithCp_<Sy>(PhantomData<Sy>);

type WithCp<Sy> = Ext<WithCp_<Sy>>;

fn with_cp<Sy>() -> WithCp<Sy> {
    Ext(WithCp_(PhantomData::<Sy>))
}

impl<'src, E, Sy> ExtParser<'src, Input<'src>, E::Checkpoint, E> for WithCp_<Sy>
where
    E: BuilderExtra<'src>,
{
    fn parse(
        &self,
        input: &mut InputRef<'src, '_, Input<'src>, E>,
    ) -> Result<E::Checkpoint, <E as ParserExtra<'src, Input<'src>>>::Error> {
        Ok(*input.save().inspector())
    }
}
