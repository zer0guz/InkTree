use snafu::ResultExt;
use strum::IntoDiscriminant;
use syn::{Lit, LitStr, MetaList, Variant};

use crate::{
    attributes::{
        from_meta::{PathSnafu, ValueSnafu}, properties::{SyntaxProperty, SyntaxPropertyKind}, AttributeError, FromMetaKind, SyntaxAttribute
    }, error::Errors, util::IteratorExt, LanguageElement, SemanticError, Verify
};

pub struct StaticToken {
    pub text: String,
    properties: Vec<SyntaxProperty>
}

impl StaticToken {
    fn from_lit(lit: &Lit) -> Result<Self, AttributeError> {
        let text = match lit {
            Lit::Str(lit_str) => lit_str.value(),
            Lit::ByteStr(lit_byte_str) => todo!("support byte str"),
            _ => todo!("lit type error"),
        };
        Ok(Self { text, properties: todo!() })
    }
}

impl FromMetaKind for StaticToken {
    fn from_list(list: &MetaList) -> Result<SyntaxAttribute, AttributeError> {
        let lit: LitStr = list.parse_args()?;
        Ok(Self { text: lit.value(), properties: todo!() }.into())
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

impl Verify for StaticToken {
    const ALLOWED: &[SyntaxPropertyKind] = &[];
}


impl LanguageElement for StaticToken {
    fn codegen(&self, stream: &mut proc_macro2::TokenStream) {
        // let span = self.source.span();
        // let ident = &self.source.ident;
        // let tree = quote!{ span=>
        //     struct #ident {}
        // };
        // stream.extend(tree);
    }
}