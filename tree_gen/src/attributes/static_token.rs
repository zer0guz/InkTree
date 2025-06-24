use proc_macro2::TokenStream;
use quote::quote;
use snafu::ResultExt;
use syn::{Ident, Lit, LitStr, MetaList};

use crate::{
    LanguageElement, attributes::{
        AttributeError, FromMetaKind, SyntaxAttribute,
        from_meta::{PathSnafu, ValueSnafu},
        properties::{SyntaxProperty, SyntaxPropertyKind},
    },
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
        Ok(Self {
            text,
        })
    }
    
    fn functions(&self) -> TokenStream {
        let parser = self.parser();
        quote! {
            #parser
        }
    }
    
    fn parser(&self) -> TokenStream {
        quote! {
            pub fn parser(){
                
            }
        }
    }
}

impl FromMetaKind for StaticToken {
    fn from_list(list: &MetaList) -> Result<SyntaxAttribute, AttributeError> {
        let lit: LitStr = list.parse_args()?;
        Ok(Self {
            text: lit.value(),

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
    fn allowed(&self) -> &'static [SyntaxPropertyKind] {
        ALLOWED
    }
    
    fn codegen(&self,_:&Vec<SyntaxProperty>,ident:&Ident,stream: &mut TokenStream) {
        let functions = self.functions();
        
        let code = quote! {
            struct #ident;
            impl #ident {
                #functions
            }
        };
        stream.extend(code);

    }
}
