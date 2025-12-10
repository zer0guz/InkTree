#[derive(Debug, Clone)]
pub struct _InkError {
    pub span: std::ops::Range<usize>, // byte offsets in the file
    pub kind: _InkErrorKind,
}

#[derive(Debug, Clone)]
pub enum _InkErrorKind {
    Expected(Vec<&'static str>),
    UnclosedDelimiter { open: char, close: char },
    // ...
}
