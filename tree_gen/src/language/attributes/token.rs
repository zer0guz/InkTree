use std::collections::HashMap;

use proc_macro2::{Literal, TokenStream};
use quote::quote;
use regex_syntax::hir::Class;
use snafu::{ResultExt, Snafu};
use syn::{Ident, Lit};

use crate::{
    LanguageElement,
    attributes::{AttributeError, parseable_impl, properties::SyntaxPropertyKind},
    language::{
        code::struct_def,
        mir::{Mir, MirError},
    },
    parser::FromMeta,
};

#[derive(Debug, Snafu)]
pub enum TokenError {
    #[snafu(display("todo text"))]
    Mir {
        #[snafu(source(true))]
        mir: MirError,
        #[snafu(source(false))]
        source: syn::Error,
    },
}

#[derive(Debug)]
pub struct Token {
    pub expr: Mir,
}

impl Token {
    pub fn from_lit<'src>(lit: &Lit) -> Result<Self, TokenError> {
        let text = match lit {
            Lit::Str(lit_str) => lit_str.value(),
            _ => todo!("token from_lit"),
        };
        let expr = Mir::parse(text.as_str()).context(MirSnafu {
            source: syn::Error::new_spanned(lit, "todo message"),
        })?;
        Ok(Self { expr })
    }
    fn parser(expr: &Mir) -> TokenStream {
        match expr {
            Mir::Empty => quote! { just(()) },
            Mir::Literal(literal) => {
                let bytes: &[u8] = literal.0.as_ref();
                let lit = match std::str::from_utf8(bytes) {
                    Ok(s) => Literal::string(s),     // "foo\n" style
                    Err(_) => Literal::byte_string(bytes), // b"\xC8\xC8" style
                };
                quote! { just(#lit) }
            }
            Mir::CharClass(class) => match class {
                Class::Unicode(unicode) => {
                    let ranges: Vec<_> = unicode
                        .ranges()
                        .iter()
                        .map(|r| {
                            let lo = r.start() as char;
                            let hi = r.end() as char;
                            quote! { (#lo..=#hi) }
                        })
                        .collect();

                    quote! {
                      ::tree_gen::ranges(&[ #(#ranges),* ])
                    }
                }
                Class::Bytes(bytes) => {
                    let ranges: Vec<_> = bytes
                        .ranges()
                        .iter()
                        .map(|r| {
                            let lo = r.start() as char;
                            let hi = r.end() as char;
                            quote! { (#lo..=#hi) }
                        })
                        .collect();

                    quote! {
                      ::tree_gen::ranges(&[ #(#ranges),* ])
                    }
                }
            },
            Mir::Sequence(mirs) => {
                mirs.iter()
                    .map(|expr| Self::parser(expr))
                    .fold(quote! {}, |acc, p| {
                        if acc.is_empty() {
                            p
                        } else {
                            quote! { #acc.then(#p) }
                        }
                    })
            }
            Mir::Choice(mirs) => {
                let branches: Vec<_> = mirs.iter().map(Self::parser).collect();
                quote! {
                    choice(( #(#branches),* ))
                }
            }
            Mir::ZeroOrMore(mir) => {
                let parser = Self::parser(mir);
                quote! {#parser.repeated()}
            }
            Mir::Optional(mir) => {
                let parser = Self::parser(mir);
                quote! {#parser.or_not()}
            }
        }
    }
}

impl FromMeta for Token {
    fn from_list(list: &syn::MetaList) -> Result<super::SyntaxAttribute, AttributeError> {
        let lit: Lit = list.parse_args()?;
        Ok(Self::from_lit(&lit)?.into())
    }

    fn from_name_value(
        name_value: &syn::MetaNameValue,
    ) -> Result<super::SyntaxAttribute, AttributeError> {
        match &name_value.value {
            syn::Expr::Lit(expr_lit) => Ok(Self::from_lit(&expr_lit.lit)?.into()),
            _ => todo!("error"),
        }
    }
}

impl LanguageElement for Token {
    fn codegen(
        &self,
        ident: &Ident,
        lang_ident: &Ident,
        _: &HashMap<String, Ident>,
    ) -> Result<TokenStream, AttributeError> {
        let def_body = quote! {};
        let def = struct_def(def_body, &ident);
        let parser = Self::parser(&self.expr);
        let impl_code = token_impl(ident, lang_ident, parser);

        Ok(quote! {
            #def
            #impl_code
        })
    }

    fn allowed(&self) -> &'static [SyntaxPropertyKind] {
        &[]
    }
}

pub fn token_impl(ident: &Ident, lang_ident: &Ident, body: TokenStream) -> TokenStream {
    let parser = quote! {
        use ::tree_gen::chumsky::prelude::*;
        use tree_gen::BuilderParser;
        #body.as_token(#lang_ident::#ident)
    };
    parseable_impl(parser, ident, lang_ident)
}
