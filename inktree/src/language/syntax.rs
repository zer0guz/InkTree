use cstree::RawSyntaxKind;

pub trait Syntax: cstree::Syntax + Copy + Eq + 'static {
    fn static_text(self) -> Option<&'static str>;

    fn from_raw(raw: RawSyntaxKind) -> Self {
        <Self as cstree::Syntax>::from_raw(raw)
    }

    fn into_raw(self) -> RawSyntaxKind {
        <Self as cstree::Syntax>::into_raw(self)
    }
}


