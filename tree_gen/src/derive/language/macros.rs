#[macro_export]
macro_rules! make_parser {
    // with params: list of idents
    ($lang:ident, [$($arg:ident),*], $body:block) => {
        fn parser<'src, 'cache, 'interner,'borrow, Err>(
            $(
                $arg: impl $crate::chumsky_ext::BuilderParser<
                    'src, 'cache, 'interner,'borrow, (), Err, $lang
                > + Clone  + 'src
            ),*
        ) -> $crate::impl_type!($lang)
        where
            Err: chumsky::error::Error<'src, &'src str> + 'src,
            Builder<'cache, 'interner, 'borrow, $lang>: chumsky::inspector::Inspector<'src, & 'src str, Checkpoint = $crate::cstree::build::Checkpoint> + 'src,

            'interner: 'cache,
            'borrow: 'interner,
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
            'src, 'cache, 'interner,'borrow, (), Err, $lang
        > + Clone + 'src
    };
}

#[macro_export]
macro_rules! make_anchored_parser {
    ($lang:ident, [$($anchor:ident),*], [$($param:ident),*], $body:block) => {
        fn anchored_parser<'src, 'cache, 'interner,'borrow, Err>(
            $(
                $anchor: $crate::impl_type!($lang),
            )*
            $(
                $param: $crate::impl_type!($lang),
            )*
        ) -> $crate::impl_type!($lang)
        where
            Err: chumsky::error::Error<'src, &'src str> + 'src,
            Builder<'cache, 'interner, 'borrow, $lang>: $crate::chumsky::inspector::Inspector<'src, &'src str> + GreenState<'src, $lang> + 'src,
            Builder<'cache, 'interner, 'borrow, $lang>: chumsky::inspector::Inspector<'src, & 'src str, Checkpoint = $crate::cstree::build::Checkpoint> + 'src,

            'interner: 'cache,
            'borrow: 'interner,

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
    // without sink
    ($lang_name:ident :: $name:ident, $body:block) => {
        pub(crate) struct $name;
        $crate::parseable!($lang_name::$name, [], {
            use $crate::chumsky::Parser;
            use $crate::chumsky::prelude::*;
            use $crate::chumsky_ext::*;
            $body.as_token($lang_name::$name)
        });
    };

    // with sink
    ($lang_name:ident :: $name:ident, $body:block, has_extras) => {
        pub(crate) struct $name;
        $crate::parseable!($lang_name::$name, [], {
            use $crate::chumsky::Parser;
            use $crate::chumsky::prelude::*;
            use $crate::chumsky_ext::*;
            $lang_name::sink($body.as_token($lang_name::$name))
        });
    };
}

#[macro_export]
macro_rules! static_token {
    // no sink
    ($lang_name:ident :: $name:ident, $text:literal) => {
        struct $name;
        $crate::parseable!($lang_name::$name, [], {
            use $crate::chumsky::prelude::*;
            use $crate::chumsky_ext::*;
            just($text).as_static_token($lang_name::$name)
        });
    };

    // with sink (extras)
    ($lang_name:ident :: $name:ident, $text:literal, has_extras) => {
        struct $name;
        $crate::parseable!($lang_name::$name, [], {
            use $crate::chumsky::prelude::*;
            use $crate::chumsky_ext::*;
            $lang_name::sink(just($text).as_static_token($lang_name::$name))
        });
    };
}

#[macro_export]
macro_rules! make_sink {
    ($lang:ident, [$($extra:ident),+ $(,)?]) => {
        impl $lang {
            fn sink<'src, 'cache, 'interner,'borrow, Err>(
                parser: $crate::impl_type!($lang)
            ) -> $crate::impl_type!($lang)
            where
                Err: chumsky::error::Error<'src, &'src str> + 'src,
                Builder<'cache, 'interner, 'borrow, $lang>: chumsky::inspector::Inspector<'src, & 'src str, Checkpoint = $crate::cstree::build::Checkpoint> + 'src,
                'interner: 'cache,
                'borrow: 'interner,
            {
                use $crate::chumsky::prelude::*;
                use $crate::chumsky_ext::*;

                // one “extras” parser that matches ANY of the extras and produces ()
                let extras = choice((
                    $(
                        $extra::parser(),
                    )*
                ));

                // consume zero or more extras, then the real token
                extras.repeated().to_slice().as_node(TestLang::Expr).ignore_then(parser)
            }
        }
    };
}
