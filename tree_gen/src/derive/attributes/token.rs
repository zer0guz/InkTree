use proc_macro2::{Literal, TokenStream};
use quote::quote;
use regex_syntax::hir::Class;
use syn::{Ident, Lit};

use crate::{
    derive::{
        attributes::allowed::ALLOWED_TOKEN,
        parser::{FromMeta, Mir},
        properties::{Property, PropertyKind, try_handle_extra},
    },
    language::{ElementError, Language, LanguageElement},
};

#[derive(Debug)]
pub struct Token {
    pub expr: Mir,
    name: Ident,
    is_extra: bool,
}

impl Token {
    pub fn from_lit<'src>(lit: &Lit, name: Ident) -> Result<Self, ElementError> {
        let text = match lit {
            Lit::Str(lit_str) => lit_str.value(),
            _ => todo!("token from_lit"),
        };
        let expr = Mir::parse(text.as_str()).map_err(|err| {
            syn::Error::new_spanned(lit, format!("todo message mir error: {}", err))
        })?;

        Ok(Self {
            expr,
            name,
            is_extra: false,
        })
    }
    fn parser(expr: &Mir) -> TokenStream {
        let parser = match expr {
            Mir::Empty => quote! { just(()) },
            Mir::Literal(literal) => {
                let bytes: &[u8] = literal.0.as_ref();
                let lit = match std::str::from_utf8(bytes) {
                    Ok(s) => Literal::string(s),           // "foo\n" style
                    Err(_) => Literal::byte_string(bytes), // b"\xC8\xC8" style
                };
                quote! {just(#lit)}
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
                      ranges(&[#(#ranges),*])
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
                      ranges(&[#(#ranges),*])
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
                            quote! {#acc.then(#p)}
                        }
                    })
            }
            Mir::Choice(mirs) => {
                let branches: Vec<_> = mirs.iter().map(Self::parser).collect();
                quote! {
                    choice((#(#branches),*))
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
        };

        quote! {#parser}
    }
}

impl FromMeta for Token {
    fn from_list(list: &syn::MetaList, name: Option<&Ident>) -> Result<Self, ElementError> {
        let lit: Lit = list.parse_args()?;
        Ok(Self::from_lit(&lit, name.expect("todod name option arg").clone())?.into())
    }

    fn from_name_value(
        name_value: &syn::MetaNameValue,
        name: Option<&Ident>,
    ) -> Result<Self, ElementError> {
        match &name_value.value {
            syn::Expr::Lit(expr_lit) => Ok(Self::from_lit(
                &expr_lit.lit,
                name.expect("todo name arg option").clone(),
            )?
            .into()),
            _ => todo!("error"),
        }
    }
}

impl LanguageElement for Token {
    fn codegen(&self, language: &Language) -> Result<TokenStream, ElementError> {
        let parser = Self::parser(&self.expr);

        let ident = &self.name;
        let lang_ident = &language.ident;
        // let impl_code = token_impl(&self.name, &language.ident, parser);
        if language.extras.is_empty() || self.is_extra {
            Ok(quote! {
               tree_gen::token!(#lang_ident::#ident,{#parser});
            })
        } else {
            Ok(quote! {
               tree_gen::token!(#lang_ident::#ident,{#parser},has_extras);
            })
        }
    }

    fn allowed(&self) -> &'static [PropertyKind] {
        ALLOWED_TOKEN
    }

    fn name(&self) -> &Ident {
        &self.name
    }

    fn build(
        &mut self,
        properties: &Vec<Property>,
        language: &mut Language,
    ) -> Result<(), ElementError> {
        properties
            .iter()
            .for_each(|prop| self.is_extra = try_handle_extra(&self.name, prop, language));
        Ok(())
    }
}
