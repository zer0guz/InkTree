#[macro_export]
macro_rules! make_parser {
    // with params: list of idents
    ($lang:ident, [$($arg:ident),*], $body:block) => {
        fn parser<'src, 'cache, 'interner, Err>(
            $(
                $arg: impl $crate::chumsky_ext::BuilderParser<
                    'src, 'cache, 'interner, (), Err, $lang
                > + Clone + 'src
            ),*
        ) -> $crate::impl_type!($lang)
        where
            Err: chumsky::error::Error<'src, &'src str> + 'src,
            'cache: 'src,
            'interner: 'cache,
        {
            use tree_gen::chumsky_ext::*;
            use tree_gen::chumsky::prelude::*;
            $body
        }
    };
}

#[macro_export]
macro_rules! impl_type {
    ($lang:ident) => {
        impl ::tree_gen::chumsky_ext::BuilderParser<
            'src, 'cache, 'interner, (), Err, $lang
        > + Clone + 'src
    };
}

#[macro_export]
macro_rules! make_anchored_parser {
    ($lang:ident, [$($anchor:ident),*], [$($param:ident),*], $body:block) => {
        fn anchored_parser<'src, 'cache, 'interner, Err>(
            $(
                $anchor: $crate::impl_type!($lang),
            )*
            $(
                $param: $crate::impl_type!($lang),
            )*
        ) -> $crate::impl_type!($lang)
        where
            Err: chumsky::error::Error<'src, &'src str> + 'src,
            'cache: 'src,
            'interner: 'cache,
        {
            use tree_gen::chumsky_ext::*;
            use tree_gen::chumsky::prelude::*;

            $body
        }
    };
}

#[macro_export]
macro_rules! parseable {
    ($lang_name:ident::$name:ident, [$($param:ident),*], $body:block) => {
        impl ::tree_gen::Parseable for $name {
            type Syntax = $lang_name;
            tree_gen::make_parser!($lang_name, [$($param),*], $body);
        }
    };
}

#[macro_export]
macro_rules! token {
    ($lang_name:ident::$name:ident, $body:block) => {
        struct $name;
        tree_gen::parseable!($lang_name::$name, [], {
            use tree_gen::chumsky::Parser;
            use tree_gen::chumsky::prelude::*;
            use tree_gen::chumsky_ext::*;
            $body.as_token($lang_name::$name)
        });
    };
}
#[macro_export]
macro_rules! static_token {
    ($lang_name:ident :: $name:ident, $text:literal) => {
        struct $name;
        tree_gen::parseable!($lang_name::$name, [], {
            use tree_gen::chumsky::prelude::*;
            use tree_gen::chumsky_ext::*;

            just($text).as_static_token($lang_name::$name)
        });
    };
}
