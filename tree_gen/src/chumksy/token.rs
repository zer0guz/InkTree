use chumsky::{
    Parser,
    extension::v1::{Ext, ExtParser},
    input::InputRef,
    prelude::just,
};
use cstree::Syntax;

use crate::{
    BuilderParser, Cp, Input, Sy,
    chumksy::{extra::BuilderExtra, state::BuilderState},
};

#[derive(Debug, Clone, Copy)]
pub struct Token_<Sy>(Sy);

impl<'src, 'cache, 'interner, E, Sy> ExtParser<'src, Input<'src>, (), E> for Token_<Sy>
where
    E: BuilderExtra<'src>,
    E::State: BuilderState<'src, Checkpoint = Cp<'src, E>, SyntaxKind = Sy>,
    Sy: Syntax,
{
    fn parse(
        &self,
        inp: &mut InputRef<'src, '_, Input<'src>, E>,
    ) -> Result<(), <E as chumsky::extra::ParserExtra<'src, Input<'src>>>::Error> {
        let static_text = self
            .0
            .static_text()
            .expect("tried to call static parser for non static token");
        match inp.check(just(static_text)) {
            Ok(_) => {
                let builder = inp.state();
                builder.token(self.0, None);
                Ok(())
            }
            error => error,
        }
    }
}
pub type Token<Sy> = Ext<Token_<Sy>>;

fn _just_token<'src, E>(kind: Sy<'src, E>) -> Token<Sy<'src, E>>
where
    E: BuilderExtra<'src>,
{
    Ext(Token_::<Sy<'src,E>>(kind))
}

pub fn just_token<'src, E>(static_kind: Sy<'src, E>) -> impl BuilderParser<'src, (), E> + Clone
where
    E: BuilderExtra<'src>,
    E::State: BuilderState<'src, Checkpoint = Cp<'src, E>, SyntaxKind = Sy<'src, E>>,
    Sy<'src, E>: Syntax + Copy,
{
    _just_token::<'_,E>(static_kind)
}
