use crate::*;

/// The type that can be used to parse a unit.
pub trait ParseUnit<S: Source>: Sized {
    /// the type of the parse result
    type Result;

    /// you should not call [`ParseUnit::parse`] directly, using methods like [`Parser::once`] instead
    fn parse(self, p: &mut Parser<S>) -> Result<Self::Result, ParseError>;
}

impl<F, S: Source, R> ParseUnit<S> for F
where
    F: FnOnce(&mut Parser<S>) -> Result<R, ParseError>,
{
    type Result = R;

    #[inline]
    fn parse(self, p: &mut Parser<S>) -> Result<R, ParseError> {
        self(p)
    }
}
