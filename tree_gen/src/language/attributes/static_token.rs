use std::collections::HashMap;

use proc_macro2::{TokenStream, TokenTree};
use quote::quote;
use snafu::ResultExt;
use syn::{Ident, Lit, LitStr, MetaList};

use crate::{
    LanguageElement,
    attributes::{SyntaxProperty, properties},
    language::{
        attributes::{AttributeError, SyntaxAttribute, properties::SyntaxPropertyKind},
        code::struct_def,
    },
    parser::{FromMeta, PathSnafu, ValueSnafu},
};

const ALLOWED: &[SyntaxPropertyKind] = &[];

pub struct StaticToken {
    pub text: String,
}

impl StaticToken {
    fn from_lit(lit: &Lit) -> Result<Self, AttributeError> {
        let text = match lit {
            Lit::Str(str) => str.value(),
            Lit::ByteStr(_) => todo!("support byte str"),
            _ => todo!("lit type error"),
        };
        Ok(Self { text })
    }
}

impl FromMeta for StaticToken {
    fn from_list(list: &MetaList) -> Result<SyntaxAttribute, AttributeError> {
        let lit: LitStr = list.parse_args()?;
        Ok(Self { text: lit.value() }.into())
    }

    fn from_path(path: &syn::Path) -> Result<SyntaxAttribute, AttributeError> {
        Err(syn::Error::new_spanned(path, "todo")).context(PathSnafu)?
    }

    fn from_name_value(name_value: &syn::MetaNameValue) -> Result<SyntaxAttribute, AttributeError> {
        match &name_value.value {
            syn::Expr::Lit(expr_lit) => Ok(Self::from_lit(&expr_lit.lit)?.into()),
            _ => Err(syn::Error::new_spanned(
                &name_value.value,
                "todo better text expr type error",
            ))
            .context(ValueSnafu)?,
        }
    }
}

impl LanguageElement for StaticToken {
    fn codegen(
        &self,
        ident: &Ident,
        lang_ident: &Ident,
        _: &HashMap<String, Ident>,
    ) -> Result<TokenStream, AttributeError> {
        let def_body = quote! {};
        let def = struct_def(def_body, &ident);
        let impl_code = static_token_impl("test", ident, lang_ident);
        Ok(quote! {
            #def
            #impl_code
        })
    }

    fn allowed(&self) -> &'static [SyntaxPropertyKind] {
        &[]
    }
}

// impl KwLet {
//     const fn into_syntax() -> TestLang {
//         return TestLang::KwLet;
//     }
//     pub fn parser<'src, 'cache, 'interner, Err, Tok, Sy>()
//     -> impl BuilderParser<'src, 'cache, 'interner, (), Err, Builder<'cache, 'interner, TestLang>>
//     where
//         Err: chumsky::error::Error<'src, &'src str> + 'src,
//         'cache: 'src,
//         'interner: 'src,
//         'interner: 'cache,
//     {
//         just("let").as_static_token(TestLang::KwLet)
//     }
// }

pub fn static_token_impl(text: &str, ident: &Ident, lang_ident: &Ident) -> TokenStream {
    quote! {
        impl #ident {
            const fn into_syntax() ->#lang_ident {
                return #lang_ident::#ident;
            }
            pub fn parser<'src, 'cache, 'interner, Err>()
            -> impl ::tree_gen::BuilderParser<'src, 'cache, 'interner, (), Err, ::tree_gen::Builder<'cache, 'interner, #lang_ident>>
            where
                Err: ::tree_gen::chumsky::error::Error<'src, &'src str> + 'src,
                'cache: 'src,
                'interner: 'src,
                'interner: 'cache,
            {
                use ::tree_gen::BuilderParser;
                ::tree_gen::chumsky::prelude::just(#text).as_static_token(#lang_ident::#ident)
            }
        }
    }
}
