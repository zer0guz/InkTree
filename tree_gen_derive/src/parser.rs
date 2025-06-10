use quote::ToTokens;
use syn::{Expr, Meta, MetaList, MetaNameValue};

pub enum VariantParser {
    StaticToken(String),
    Node,
    None,
}

impl VariantParser {
    fn static_token(expr: &Expr) -> Self {
        match expr {
            syn::Expr::Lit(expr_lit) => match &expr_lit.lit {
                syn::Lit::Str(lit_str) => VariantParser::StaticToken(lit_str.value()),
                _ => todo!(),
            },

            _ => todo!(),
        }
    }
    fn token(expr: &Expr) -> Self {
        match expr {
            _ => todo!(),
        }
    }
    fn node(expr: &Expr) -> Self {
        match expr {
            _ => todo!(),
        }
    }
    fn from_list(list: &MetaList) -> Self {
        match list {
            _ => todo!(),
        }
    }
    fn from_name_value(meta_name_value: &MetaNameValue) -> Self {
        let expr = &meta_name_value.value;

        match meta_name_value
            .path
            .get_ident()
            .to_token_stream()
            .to_string()
            .as_str()
        {
            "token" => VariantParser::token(expr),
            "node" => VariantParser::node(expr),
            "static_token" => VariantParser::static_token(expr),

            _ => todo!("4"),
        }
    }
}

pub struct SyntaxVariant {
    pub ident: String,
    pub parser: VariantParser,
}

pub fn parse_variant(variant: &mut syn::Variant) -> Result<SyntaxVariant, syn::Error> {
    let attrs = &mut variant.attrs;
    let mut syntax_variant = SyntaxVariant {
        ident: variant.ident.to_string(),
        parser: VariantParser::None,
    };
    let Some(attr) = attrs.pop() else {
        return Ok(syntax_variant);
    };

    let parser = if attr.path().is_ident("tree_gen") {
        let attr: Meta = attr.parse_args()?;
        match &attr {
            Meta::Path(path) => {
                return Err(syn::Error::new_spanned(
                    path,
                    "tree_gen requires arguments such as node(), token(),...",
                ));
            }
            Meta::List(meta_list) => VariantParser::from_list(meta_list),
            Meta::NameValue(meta_name_value) => VariantParser::from_name_value(meta_name_value),
        }
    } else {
        VariantParser::None
    };
    syntax_variant.parser = parser;

    Ok(syntax_variant)
}
