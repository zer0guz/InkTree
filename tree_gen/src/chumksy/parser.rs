use chumsky::{extension::v1::{Ext, ExtParser}, extra::ParserExtra, input::InputRef, inspector::Inspector, Parser};

use crate::{chumksy::{extra::BuilderExtra, state::BuilderState, Input}, Cp, Sy};


pub trait BuilderParser<'src, O, E>: Parser<'src, Input<'src>, O, E>
where
    E: BuilderExtra<'src>,
{
    fn token_parser(self, kind: Sy<'src, E>) -> impl Parser<'src, Input<'src>, (), E>;
    fn as_node(self, kind: Sy<'src, E>) -> impl Parser<'src, Input<'src>, (), E>;
    fn wrap_cp(
        parser: impl Parser<'src, Input<'src>, Cp<'src, E>, E>,
        kind: Sy<'src, E>,
    ) -> impl Parser<'src, Input<'src>, (), E>;
    fn with_cp(self) -> impl Parser<'src, Input<'src>, Cp<'src, E>, E>;
}

impl<'src, O, P, E> BuilderParser<'src, O, E> for P
where
    E: BuilderExtra<'src>,
    E::State: BuilderState<'src, Checkpoint = Cp<'src, E>, SyntaxKind = Sy<'src, E>>,
    P: chumsky::Parser<'src, &'src str, O, E>,
    Sy<'src, E>: Copy,
{
    fn token_parser(self, kind: Sy<'src, E>) -> impl Parser<'src, Input<'src>, (), E> {
        self.to_slice().map_with(move |slice, extra| {
            extra.state().token(kind, Some(slice));
            ()
        })
    }
    fn with_cp(self) -> impl Parser<'src, Input<'src>, Cp<'src, E>, E> {
        with_cp().then(self).map(|(cp, _)| cp)
    }
    fn as_node(self, kind: Sy<'src, E>) -> impl Parser<'src, Input<'src>, (), E> {
        Self::wrap_cp(self.with_cp(), kind)
    }
    fn wrap_cp(
        parser: impl Parser<'src, Input<'src>, Cp<'src, E>, E>,
        kind: Sy<'src, E>,
    ) -> impl Parser<'src, Input<'src>, (), E> {
        parser.map_with(move |cp, extra| {
            let builder = extra.state();
            builder.start_node_at(cp, kind);
            builder.finish_node();
        })
    }
}


#[derive(Debug, Copy, Clone)]
struct WithCp_();

type WithCp = Ext<WithCp_>;

fn with_cp() -> WithCp {
    Ext(WithCp_())
}

impl<'src, E> ExtParser<'src, Input<'src>, Cp<'src, E>, E> for WithCp_
where
    E: BuilderExtra<'src>,
    E::State: Inspector<'src, Input<'src>, Checkpoint = Cp<'src, E>>,
{
    fn parse(
        &self,
        input: &mut InputRef<'src, '_, Input<'src>, E>,
    ) -> Result<Cp<'src, E>, <E as ParserExtra<'src, Input<'src>>>::Error> {
        Ok(*input.save().inspector())
    }
}
