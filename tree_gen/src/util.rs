use proc_macro2::Span;
use quote::ToTokens;

pub fn span<'a, T: ToTokens>(to_tokens: T) -> Span {
    let mut iter = to_tokens.into_token_stream().into_iter();
    let start = iter.next().map_or_else(Span::call_site, |t| t.span());
    let end = iter.last().map_or(start, |t| t.span());
    start.join(end).unwrap_or(start)
}
