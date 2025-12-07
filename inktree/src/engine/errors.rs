#[derive(Debug, Clone)]
pub struct InkError {
    pub span: std::ops::Range<usize>, // byte offsets in the file
    pub kind: InkErrorKind,
}

#[derive(Debug, Clone)]
pub enum InkErrorKind {
    Expected(Vec<&'static str>),
    UnclosedDelimiter { open: char, close: char },
    // ...
}
