#[macro_export]
macro_rules! make_parser {
    // with params: list of idents
    ($name:ident, $lang:ident, [$($arg:ident),*], $body:block) => {
        fn $name<'src, 'cache, 'interner,'borrow,'extra, Err>(
            $(
                $arg: impl $crate::chumsky_ext::BuilderParser<
                    'src, 'cache, 'interner,'borrow, (), Err, $lang
                > + Clone + 'extra
            ),*
        ) -> $crate::impl_type!($lang)
        where
            Err: $crate::chumsky::error::Error<'src, &'src str> + 'extra,
            'interner: 'cache,
            'borrow: 'interner,
            'cache: 'extra,
            'src: 'extra

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
            'cache: 'extra,


        {
            use inktree::chumsky_ext::*;
            use inktree::chumsky::prelude::*;

            $body
        }
    };
}

#[macro_export]
macro_rules! parseable {
    ($lang_name:ident::$name:ident, [$($param:ident),*], $body:block, $recovery:path, Node) => {
        impl ::inktree::Parseable for $name {
            type Syntax = $lang_name;
            inktree::make_parser!(base_parser,$lang_name, [$($param),*], $body);
            inktree::make_parser!(wrap,$lang_name, [p], {
                p.as_node($lang_name::$name)
            });
            type RecoverySpec = $recovery;
        }
    };
    ($lang_name:ident::$name:ident, [$($param:ident),*], $body:block) => {
        impl ::inktree::Parseable for $name {
            type Syntax = $lang_name;
            inktree::make_parser!(base_parser,$lang_name, [$($param),*], $body);
            inktree::make_parser!(wrap,$lang_name, [p], {
                p
            });
            type RecoverySpec = ::inktree::engine::recovery::NoRecovery;
        }
    };
}

#[macro_export]
macro_rules! token {
    // without sink
    ($lang_name:ident :: $name:ident, $body:block) => {
        #[derive(Debug)]
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
        #[derive(Debug)]
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
    ($lang_name:ident :: $name:ident, $text:literal,) => {
        #[derive(Debug)]
        pub struct $name;
        $crate::parseable!($lang_name::$name, [], {
            use $crate::chumsky::prelude::*;
            use $crate::chumsky_ext::*;
            just($text).as_static_token($lang_name::$name)
        });
    };

    // with sink (extras)
    ($lang_name:ident :: $name:ident, $text:literal, has_extras) => {
        #[derive(Debug)]
        pub struct $name;
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
            fn sink<'src, 'cache, 'interner,'borrow, 'extra,Err>(
                parser: $crate::impl_type!($lang)
            ) -> $crate::impl_type!($lang)
            where
                Err:  $crate::chumsky::error::Error<'src, &'src str> + 'extra,
                'interner: 'cache,
                'borrow: 'interner,
                'src: 'extra,
                'cache: 'extra,

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

        pub type PrattExt<Atom> =
            ::inktree::chumsky::extension::v1::Ext<PrattExt_<Atom>>;

        pub fn pratt_ext<Atom>(atom: Atom, kind: $lang_name) -> PrattExt<Atom> {
            ::inktree::chumsky::extension::v1::Ext(PrattExt_ { atom, kind })
        }

        impl<'src, 'cache, 'interner, 'borrow,'extra, Err, Atom>
            ::inktree::chumsky::extension::v1::ExtParser<
                'src,
                &'src str,
                (),
                ::inktree::chumsky_ext::GreenExtra<
                    'cache,
                    'interner,
                    'borrow,
                    Err,
                    $lang_name,
                >,
            > for PrattExt_<Atom>
        where
            Err: ::inktree::chumsky::error::Error<'src, &'src str> + 'extra,
            Atom: ::inktree::chumsky_ext::BuilderParser<
                    'src,
                    'cache,
                    'interner,
                    'borrow,
                    (),
                    Err,
                    $lang_name,
                > + Clone,
        {
            fn parse(
                &self,
                inp: &mut ::inktree::chumsky::input::InputRef<
                    'src,
                    '_,
                    &'src str,
                    ::inktree::chumsky_ext::GreenExtra<
                        'cache,
                        'interner,
                        'borrow,
                        Err,
                        $lang_name,
                    >,
                >,
            ) -> Result<(), Err> {
                use ::inktree::engine::Builder;
                use ::inktree::chumsky_ext::*;

                fn go<'src, 'cache, 'interner, 'borrow,'extra, Err, Atom>(
                    inp: &mut ::inktree::chumsky::input::InputRef<
                        'src,
                        '_,
                        &'src str,
                        ::inktree::chumsky_ext::GreenExtra<
                            'cache,
                            'interner,
                            'borrow,
                            Err,
                            $lang_name,
                        >,
                    >,
                    atom: &Atom,
                    kind: $lang_name,
                    min_power: u16,
                ) -> Result<::inktree::cstree::build::Checkpoint, Err>
                where
                    Err: ::inktree::chumsky::error::Error<'src, &'src str> + 'extra,
                    Atom: ::inktree::chumsky_ext::BuilderParser<
                            'src,
                            'cache,
                            'interner,
                            'borrow,
                            (),
                            Err,
                            $lang_name,
                        > + Clone,
                {
                    // ---------- NUD: prefix-or-atom ----------
                    let lhs_cp = 'nud: {
                        // Try each prefix operator with its own checkpoint.
                        $(
                            {
                                let pre_op = inp.save();
                                let cp = inp.state().checkpoint(); // where Expr node should start

                                match inp.parse($p_parser) {
                                    Ok(_) => {
                                        // Parse the rest of the expression with this prefix’s precedence
                                        let _ = go(inp, atom, kind, $p_prec)?;
                                        let builder = inp.state();
                                        builder.start_node_at(cp, kind);
                                        builder.finish_node();
                                        break 'nud cp;
                                    }
                                    Err(_) => {
                                        // Parser failed; restore input and try next prefix
                                        inp.rewind(pre_op);
                                    }
                                }
                            }
                        )*

                        // No prefix matched – parse bare atom
                        let atom_cp = inp.parse(&atom.clone().with_cp())?;
                        let builder = inp.state();
                        builder.start_node_at(atom_cp, kind);
                        builder.finish_node();
                        break 'nud atom_cp;
                    };

                    // We keep lhs_cp as "start of the whole expression"
                    let lhs_cp = lhs_cp;

                    // ---------- LED: postfix + infix climbing ----------
                    loop {
                        // Checkpoint for the *whole* expression at this point.
                        // If no operator matches, or an operator has too low precedence,
                        // we rewind to here and stop.
                        let expr_cp = inp.save();

                        // ----- POSTFIX -----
                        let mut postfix_matched = false;
                        $(
                            if !postfix_matched {
                                let pre_op = inp.save();

                                match inp.parse($x_parser) {
                                    Ok(_) => {
                                        if $x_prec < min_power {
                                            // Operator is too weak for this call: let outer level handle it.
                                            inp.rewind(expr_cp);
                                            break;
                                        }

                                        let builder = inp.state();
                                        // Wrap from the *start* of the current expr (lhs_cp).
                                        builder.start_node_at(lhs_cp, kind);
                                        builder.finish_node();
                                        postfix_matched = true;
                                        // lhs_cp stays the same; expression just grew on the right.
                                    }
                                    Err(_) => {
                                        // Not this postfix op → reset and try the next one.
                                        inp.rewind(pre_op);
                                    }
                                }
                            }
                        )*

                        if postfix_matched {
                            // We consumed a postfix op: try to chain more postfix/infix operators.
                            continue;
                        }

                        // ----- INFIX -----
                        let mut infix_matched = false;
                        $(
                            if !infix_matched {
                                let pre_op = inp.save();

                                match inp.parse($i_parser) {
                                    Ok(_) => {
                                        if $i_prec < min_power {
                                            // This operator binds less tightly than our caller, so bail.
                                            inp.rewind(expr_cp);
                                            break;
                                        }

                                        let next_min = $crate::pratt_next_min_power!($i_prec, $i_assoc);

                                        // Parse RHS; the resulting Expr node still starts at lhs_cp.
                                        let _ = go(inp, atom, kind, next_min)?;
                                        let builder = inp.state();
                                        builder.start_node_at(lhs_cp, kind);
                                        builder.finish_node();
                                        infix_matched = true;
                                        // lhs_cp stays as the start of the whole (lhs op rhs) expression.
                                    }
                                    Err(_) => {
                                        // Not this infix op → reset input, try next infix.
                                        inp.rewind(pre_op);
                                    }
                                }
                            }
                        )*

                        if infix_matched {
                            // We consumed an infix op: loop to see if more operators chain.
                            continue;
                        }

                        // No postfix or infix matched here: expression is done at this precedence.
                        inp.rewind(expr_cp);
                        break;
                    }

                    Ok(lhs_cp)
                }

                // Start from min_power = 0 (all your operator precedences are >= 1).
                go(inp, &self.atom, self.kind, 0)?;
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
