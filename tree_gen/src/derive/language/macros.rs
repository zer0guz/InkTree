#[macro_export]
macro_rules! make_parser {
    // with params: list of idents
    ($lang:ident, [$($arg:ident),*], $body:block) => {
        fn parser<'src, 'cache, 'interner, Err>(
            $(
                $arg: impl $crate::chumksy_ext::BuilderParser<
                    'src, 'cache, 'interner, (), Err, $lang
                > + Clone + 'src
            ),*
        ) -> $crate::impl_type!($lang)
        where
            Err: chumsky::error::Error<'src, &'src str> + 'src,
            'cache: 'src,
            'interner: 'cache,
        {
            use tree_gen::chumksy_ext::*;
            $body
        }
    };
}

#[macro_export]
macro_rules! impl_type {
    ($lang:ident) => {
        impl ::tree_gen::chumksy_ext::BuilderParser<
            'src, 'cache, 'interner, (), Err, $lang
        > + Clone + 'src
    };
}

#[macro_export]
macro_rules! make_anchored_parser {
    ($lang:ident, [$($anchors:ident),*], [$($args:ident),*], $body:block) => {
        fn anchored_parser<'src, 'cache, 'interner, Err>(
            $(
                $anchors: $crate::impl_type!($lang),
            )*
            $(
                $args: $crate::impl_type!($lang),
            )*
        ) -> $crate::impl_type!($lang)
        where
            Err: chumsky::error::Error<'src, &'src str> + 'src,
            'cache: 'src,
            'interner: 'cache,
        {
            use $crate::chumksy_ext::*;
            $body
        }
    };
}

#[macro_export]
macro_rules! make_recursive_parser {
    ($lang:ident, $name:ident, [
        $( $peer:ident ( $( $anchor:ident ),* ) ),+
    ]) => {
        fn parser<'src, 'cache, 'interner, Err>()
            -> $crate::impl_type!($lang)
        where
            Err: chumsky::error::Error<'src, &'src str> + 'src,
            'cache: 'src,
            'interner: 'cache,
        {
            use ::tree_gen::chumsky::prelude::*;
            use ::tree_gen::chumsky::recursive::Recursive;

            // one declare for each SCC member
            $( let mut $peer = Recursive::declare(); )*

            // define each SCC member with its specific anchors
            $(
                $peer.define($peer::anchored_parser($( $anchor.clone() ),*));
            )*

            // return this one’s handle
            $name
        }
    };
}

#[macro_export]
macro_rules! parseable {
    ($lang_name:ident :: $name:ident, $body:block) => {
        impl ::tree_gen::Parseable for $name {
            type Syntax = $lang_name;
            tree_gen::make_parser!($lang_name, [], $body);
        }
    };
}

#[macro_export]
macro_rules! rule {
    // acyclic
    ($lang_name:ident :: $name:ident, [$($arg:ident),*], $body:block) => {
        struct $name;
        impl $name {
            $crate::make_parser!($lang_name, [$($arg),*], $body);
        }
    };

    // cyclic (SCC): extra list of peers
    ($lang_name:ident :: $name:ident, [$($arg:ident),*], $body:block, [$($peer:ident ( $( $anchor:ident ),* ) ),+]) => {
    struct $name;
    impl $name {
        // 1. define the anchored_parser
        $crate::make_anchored_parser!($lang_name, [$($peer),*], [$($arg),*], $body);

        // 2. define the recursive parser just for `$name`,
        //    but include all peers + their anchors
        $crate::make_recursive_parser!($lang_name, $name, [$($peer ( $( $anchor ),* ) ),+]);
    }
};

}

#[macro_export]
macro_rules! node {
    ($lang_name:ident :: $name:ident, [$($arg:ident),*], $body:block) => {
        struct $name;
        impl $name {
            tree_gen::make_parser!($lang_name,[$($arg),*] ,$body);
        }
    };
    ($lang_name:ident :: $name:ident, $body:block) => {
        struct $name;
        impl $name {
            tree_gen::make_parser!($lang_name, [],$body);
        }
    };
}

#[macro_export]
macro_rules! token {
    ($lang_name:ident :: $name:ident, $body:block) => {
        struct $name;
        tree_gen::parseable!($lang_name::$name, {
            use tree_gen::chumksy_ext::*;
            $body.as_token($lang_name::$name)
        });
    };
}
#[macro_export]
macro_rules! static_token {
    ($lang_name:ident :: $name:ident, $text:literal) => {
        struct $name;
        tree_gen::parseable!($lang_name::$name, {
            use tree_gen::chumksy_ext::*;
            ::tree_gen::chumsky::prelude::just($text).as_static_token($lang_name::$name)
        });
    };
}
