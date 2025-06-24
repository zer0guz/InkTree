use quote::quote;
use snafu::ResultExt;
use strum::IntoDiscriminant;
use syn::{Ident, Lit, LitStr, MetaList, Variant};

use crate::{
    LanguageElement, SemanticError,
    attributes::{
        AttributeError, FromMetaKind, SyntaxAttribute,
        from_meta::{PathSnafu, ValueSnafu},
        properties::{SyntaxProperty, SyntaxPropertyKind},
    },
    error::Errors,
    util::IteratorExt,
};

pub struct StaticToken {
    pub text: String,
    properties: Vec<SyntaxProperty>,
    source: Option<Variant>,
}

impl StaticToken {
    fn from_lit(lit: &Lit) -> Result<Self, AttributeError> {
        let text = match lit {
            Lit::Str(lit_str) => lit_str.value(),
            Lit::ByteStr(lit_byte_str) => todo!("support byte str"),
            _ => todo!("lit type error"),
        };
        Ok(Self {
            text,
            properties: vec![],
            source: None,
        })
    }
}

impl FromMetaKind for StaticToken {
    fn from_list(list: &MetaList) -> Result<SyntaxAttribute, AttributeError> {
        let lit: LitStr = list.parse_args()?;
        Ok(Self {
            text: lit.value(),
            properties: vec![],
            source: None,

        }
        .into())
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
    fn codegen(&self, stream: &mut proc_macro2::TokenStream) {
        let ident = &self.source.as_ref().unwrap().ident;
        let tree = quote! {
           struct #ident;
        };
        stream.extend(tree);
    }

    fn allowed(&self) -> &'static [SyntaxPropertyKind] {
        &[]
    }
    
    fn build(&mut self,properties:Vec<SyntaxProperty>,source: Variant) {
        self.properties = properties;
        self.source = Some(source);
    }
}
