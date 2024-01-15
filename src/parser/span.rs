use std::ops::Range;

#[derive(Clone, PartialEq, Default, Debug)]
pub struct Span {
    pub source: String,
    pub range: Range<usize>,
}

impl Span {
    pub fn new<S: Into<String>>(source: S, range: Range<usize>) -> Self {
        Span {
            source: source.into(),
            range,
        }
    }

    pub fn from_pest<S: Into<String>>(span: pest::Span, source: S) -> Self {
        Self::new(source, span.start()..span.end())
    }

    pub fn join(&self, other: &Self) -> Option<Self> {
        let start = self.range.start;
        let end = self.range.end;
        (self.source == other.source && start <= end)
            .then_some(Span::new(self.source.clone(), start..end))
    }
}

impl ariadne::Span for Span {
    type SourceId = String;

    fn source(&self) -> &Self::SourceId {
        &self.source
    }

    fn start(&self) -> usize {
        self.range.start
    }

    fn end(&self) -> usize {
        self.range.end
    }
}
