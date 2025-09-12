#[macro_export]
macro_rules! make_parser {
    // with params: list of idents
    ($lang:ident, [$($arg:ident),*], $body:block) => {
        fn parser<'src, 'cache, 'interner,'borrow,'extra, Err>(
            $(
                $arg: impl $crate::chumsky_ext::BuilderParser<
                    'src, 'cache, 'interner,'borrow, (), Err, $lang
                > + Clone + 'extra
            ),*
        ) -> $crate::impl_type!($lang)
        where
            Err: $crate::chumsky::error::Error<'src, &'src str>  + 'extra,
            'interner: 'cache,
            'borrow: 'interner,
            'src: 'extra,
            'cache: 'extra

        {
            use inktree::chumsky_ext::*;
            use inktree::chumsky::prelude::*;
            $body
        }
    };
}

#[macro_export]
macro_rules! impl_type {
    ($lang:ident) => {
        impl ::inktree::chumsky_ext::BuilderParser<
            'src, 'cache, 'interner,'borrow, (), Err, $lang
        > + Clone + 'extra
    };
}

#[macro_export]
macro_rules! make_anchored_parser {
    ($lang:ident, [$($anchor:ident),*], [$($param:ident),*], $body:block) => {
        fn anchored_parser<'src, 'cache, 'interner,'borrow,'extra, Err>(
            $(
                $anchor: $crate::impl_type!($lang),
            )*
            $(
                $param: $crate::impl_type!($lang),
            )*
        ) -> $crate::impl_type!($lang)
        where
            Err:  $crate::chumsky::error::Error<'src, &'src str> + 'extra,
            'interner: 'cache,
            'borrow: 'interner,
            'src: 'extra,
            'cache: 'extra

        {
            use inktree::chumsky_ext::*;
            use inktree::chumsky::prelude::*;

            $body
        }
    };
}

#[macro_export]
macro_rules! parseable {
    ($lang_name:ident::$name:ident, [$($param:ident),*], $body:block) => {
        impl ::inktree::Parseable for $name {
            type Syntax = $lang_name;
            inktree::make_parser!($lang_name, [$($param),*], $body);
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
            fn sink<'src, 'cache, 'interner,'borrow,'extra, Err>(
                parser: $crate::impl_type!($lang)
            ) -> $crate::impl_type!($lang)
            where
                Err:  $crate::chumsky::error::Error<'src, &'src str> + 'extra,
                'interner: 'cache,
                'borrow: 'interner,
                'cache: 'extra,
                'src: 'extra,

            {
                use $crate::chumsky::prelude::*;
                use $crate::chumsky_ext::*;

                let extras = choice((
                    $(
                        $extra::parser(),
                    )*
                ));

                extras.repeated().ignore_then(parser)
            }
        }
    };
}

#[macro_export]
macro_rules! define_pratt_ext {
    (
        $lang_name:ident,
        prefix: [ $( ($p_prec:literal, $p_parser:expr) ),* $(,)? ],
        infix:  [ $( ($i_prec:literal, $i_assoc:ident, $i_parser:expr) ),* $(,)? ],
        postfix:[ $( ($x_prec:literal, $x_parser:expr) ),* $(,)? ]
    ) => {
        #[derive(Clone)]
        pub struct PrattExt_<Atom> {
            atom: Atom,
            kind: $lang_name,
        }

        pub type PrattExt<Atom> = ::inktree::chumsky::extension::v1::Ext<PrattExt_<Atom>>;

        pub fn pratt_ext<Atom>(atom: Atom, kind:  $lang_name) -> PrattExt<Atom> {
            ::inktree::chumsky::extension::v1::Ext(PrattExt_ { atom, kind })
        }

        impl<'src, 'cache, 'interner, 'borrow,'extra, Err, Atom>
            ::inktree::chumsky::extension::v1::ExtParser<
                'src,
                &'src str,
                (),
                ::inktree::chumsky_ext::GreenExtra<'cache, 'interner, 'borrow, Err, $lang_name>,
            > for PrattExt_<Atom>
        where
            Err: ::inktree::chumsky::error::Error<'src, &'src str> + 'extra,
            Atom: ::inktree::chumsky_ext::BuilderParser<
                'src, 'cache, 'interner, 'borrow, (), Err, $lang_name
            > + Clone,
        {
            fn parse(
                &self,
                inp: &mut ::inktree::chumsky::input::InputRef<'src,'_,&'src str,::inktree::chumsky_ext::GreenExtra<'cache, 'interner, 'borrow, Err, $lang_name>>,
            ) -> Result<(), Err> {
                use ::inktree::engine::Builder;
                use ::inktree::chumsky_ext::*;

                fn go<'src, 'cache, 'interner, 'borrow,'extra, Err, Atom>(
                    inp: &mut ::inktree::chumsky::input::InputRef<
                        'src,
                        '_,
                        &'src str,
                        ::inktree::chumsky_ext::GreenExtra<'cache, 'interner, 'borrow, Err, $lang_name>,
                    >,
                    atom: &Atom,
                    kind: $lang_name,
                    min_power: u16,
                ) -> Result<::inktree::cstree::build::Checkpoint, Err>
                where
                    Err: ::inktree::chumsky::error::Error<'src, &'src str> + 'extra,
                    Atom: ::inktree::chumsky_ext::BuilderParser<
                        'src, 'cache, 'interner, 'borrow, (), Err, $lang_name
                    > + Clone,
                {
                    // NUD: prefix-or-atom (once)
                    let mut lhs_cp = 'nud: {
                        let inp_cp = inp.save();

                        // try exactly one prefix; chains/nesting happen in recursive `go`
                        $(
                            {
                                // capture BEFORE consuming the prefix token so the node includes it
                                let cp = inp.state().checkpoint();
                                if let Ok(_) = inp.parse($p_parser) {
                                    let _ = go(inp, atom, kind, $p_prec)?;
                                    let builder = inp.state();
                                    builder.start_node_at(cp, kind);
                                    builder.finish_node();
                                    break 'nud cp; // the expression starts at the prefix position
                                }
                            }
                        )*

                        // no prefix matched: rewind and parse atom
                        inp.rewind(inp_cp);
                        break 'nud inp.parse(&atom.clone().with_cp())?;
                    };

                    // LED: postfix + infix climbing loop
                    loop {
                        let checkpoint = inp.save();

                        // postfix
                        $(
                            if let Ok(_) = inp.parse($x_parser) {
                                if $x_prec < min_power {
                                    inp.rewind(checkpoint);
                                    break;
                                }
                                let builder = inp.state();
                                // wrap from the start of the current expression
                                builder.start_node_at(lhs_cp, kind);
                                builder.finish_node();
                                continue;
                            }
                        )*

                        // infix
                        $(
                            if let Ok(_) = inp.parse($i_parser) {
                                if $i_prec < min_power {
                                    inp.rewind(checkpoint);
                                    break;
                                }
                                let next_min = $crate::pratt_next_min_power!($i_prec, $i_assoc);

                                // parse RHS; the combined node still starts at lhs_cp
                                let _ = go(inp, atom, kind, next_min)?;
                                let builder = inp.state();
                                builder.start_node_at(lhs_cp, kind);
                                builder.finish_node();
                                // lhs_cp stays as the start of the whole (lhs op rhs)
                                continue;
                            }
                        )*

                        inp.rewind(checkpoint);
                        break;
                    }

                    Ok(lhs_cp)
                }

                let cp =  inp.state().checkpoint();
                let _root_cp = go(inp, &self.atom, self.kind, 0)?;
                let builder = inp.state();
                builder.start_node_at(cp, self.kind);
                builder.finish_node();


                Ok(())
            }
        }
    };
}

#[macro_export]
macro_rules! pratt_next_min_power {
    ( $prec:expr, Left ) => {
        $prec + 1
    };
    ( $prec:expr, Right ) => {
        $prec
    };
    ( $prec:expr, None ) => {
        compile_error!("Associativity::None not supported yet in pratt_next_min_power!");
    };
}
