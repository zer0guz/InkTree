use proc_macro2::Span;
use snafu::ResultExt;
use syn::{DeriveInput, Ident, spanned::Spanned};

use crate::{
    ParseError,
    error::*,
    parser::variant::{SyntaxVariant, VariantError},
};

pub struct SyntaxEnum {
    pub ident: Ident,
    pub attributes: Vec<SyntaxVariant>,
    pub errors: Vec<VariantError>,
    pub span: Span,
}

impl SyntaxEnum {
    pub fn from_input(input: DeriveInput) -> Result<Self, Vec<Error>> {
        let mut syntax_enum = Self {
            ident: input.ident.clone(),
            span: input.span(),
            errors: vec![],
            attributes: vec![],
        };

        let syn::Data::Enum(syntax) = input.data else {
            todo!("möps")
        };

        for variant in syntax.variants.into_iter() {
            syntax_enum.parse_variant(variant);
        }

        syntax_enum.into_result()
    }

    fn parse_variant(&mut self, variant: syn::Variant) {
        let mut attrs = variant.attrs;
        let Some(attr) = attrs.pop() else {
            //self.attributes.push(SyntaxVariant::None(variant.ident));
            return;
        };

        let syntax_variant = match SyntaxVariant::from_attr(attr) {
            Ok(ok) => ok,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };
    }

    fn into_result(self) -> Result<SyntaxEnum, Vec<Error>> {
        if self.errors.is_empty() {
            Ok(self)
        } else {
            Err(self
                .errors
                .into_iter()
                .map(|err| Error::from(ParseError::from(err)))
                .collect())
        }
    }
}
