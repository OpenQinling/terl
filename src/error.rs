use std::sync::Arc;

use crate::*;

/// possiable error kind of parse error
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseErrorKind {
    /// unmatched
    Unmatch,
    /// matched, but has semantic error
    Semantic,
}

/// bundle of [`Error`] and [`ParseErrorKind`]
#[derive(Debug, Clone)]
pub struct ParseError {
    pub(crate) error: Error,
    pub(crate) kind: ParseErrorKind,
}

impl ParseError {
    /// create an [`ParseError`]
    #[inline]
    pub fn new(loc: Span, buf_name: Arc<str>, reason: impl ToString, kind: ParseErrorKind) -> Self {
        let error = Error::new(loc, buf_name, reason);
        Self::from_error(error, kind)
    }

    /// crate an [`ParseError`] from the error and kind
    #[inline]
    pub fn from_error(error: Error, kind: ParseErrorKind) -> Self {
        Self { error, kind }
    }

    /// same as [`Error::append`]
    #[inline]
    pub fn append(mut self, message: impl Into<Message>) -> Self {
        self.messages.push(message.into());
        self
    }

    /// return the kind of [`ParseError`]
    #[inline]
    pub fn kind(&self) -> ParseErrorKind {
        self.kind
    }

    /// take [`Error`] from [`ParseError`]
    #[inline]
    pub fn error(self) -> Error {
        self.error
    }
}

impl std::ops::Deref for ParseError {
    type Target = Error;

    fn deref(&self) -> &Self::Target {
        &self.error
    }
}

impl std::ops::DerefMut for ParseError {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.error
    }
}

/// an error message
#[derive(Debug, Clone)]
pub enum Message {
    /// an error message with only location
    Location { loc: Span, buf: Arc<str> },
    /// an error message with only text
    Text { msg: String, buf: Arc<str> },
    /// an error message with both location and text
    Rich {
        msg: String,
        buf: Arc<str>,
        loc: Span,
    },
}

impl WithBufName for Message {
    fn buf_name(&self) -> &Arc<str> {
        match self {
            Self::Location { buf, .. } => buf,
            Self::Text { buf, .. } => buf,
            Self::Rich { buf, .. } => buf,
        }
    }
}

impl Message {
    /// create a error message with location and text
    pub fn rich(msg: String, loc: Span, buf: Arc<str>) -> Self {
        Self::Rich { msg, buf, loc }
    }

    pub fn text(msg: String, buf: Arc<str>) -> Self {
        Self::Text { msg, buf }
    }
}

/// an error, with many messages in
#[derive(Debug, Clone)]
pub struct Error {
    pub(crate) messages: Vec<Message>,
}

impl Error {
    /// create an [`Error`] with [`Message::Rich`] in
    pub fn new(loc: Span, buf_name: Arc<str>, reason: impl ToString) -> Self {
        Self {
            messages: vec![Message::rich(reason.to_string(), loc, buf_name)],
        }
    }

    /// append an error [`Message`], and return [`Error`] for chain-calling
    pub fn append(mut self, message: impl Into<Message>) -> Self {
        self.messages.push(message.into());
        self
    }

    pub fn into_parse_error(self, kind: ParseErrorKind) -> ParseError {
        ParseError { error: self, kind }
    }

    pub fn messages(&self) -> &[Message] {
        &self.messages
    }

    pub fn into_mesages(self) -> Vec<Message> {
        self.messages
    }
}

impl Extend<Message> for Error {
    fn extend<T: IntoIterator<Item = Message>>(&mut self, iter: T) {
        self.messages.extend(iter)
    }
}

impl From<Message> for Error {
    fn from(value: Message) -> Self {
        Self {
            messages: vec![value],
        }
    }
}

impl From<Error> for Vec<Message> {
    fn from(value: Error) -> Self {
        value.into_mesages()
    }
}

impl std::ops::Add for Error {
    type Output = Error;

    fn add(mut self, rhs: Self) -> Self::Output {
        self.messages.extend(rhs.messages);
        self
    }
}

impl std::ops::AddAssign for Error {
    fn add_assign(&mut self, rhs: Self) {
        self.messages.extend(rhs.messages);
    }
}

impl<M: Into<Message>> std::ops::Add<M> for Error {
    type Output = Error;

    fn add(mut self, rhs: M) -> Self::Output {
        self.messages.push(rhs.into());
        self
    }
}

impl<M: Into<Message>> std::ops::AddAssign<M> for Error {
    fn add_assign(&mut self, rhs: M) {
        self.messages.push(rhs.into());
    }
}

impl<T> From<Error> for Result<T> {
    fn from(value: Error) -> Self {
        Result::Err(value)
    }
}

impl<T> TryFrom<Result<T>> for Error {
    type Error = T;

    fn try_from(
        value: Result<T>,
    ) -> std::prelude::v1::Result<Self, <Error as TryFrom<Result<T>>>::Error> {
        match value {
            Result::Ok(success) => Err(success),
            Result::Err(e) => Ok(e),
        }
    }
}

/// tag types with a location information in sorce file
pub trait WithSpan {
    /// get the location information
    fn get_span(&self) -> Span;
}

pub trait WithBufName {
    fn buf_name(&self) -> &Arc<str>;
}

/// making [`ParseError`]
pub trait MakeError: WithBufName + WithSpan {
    /// make an error [`Message`]  at location(from [`WithSpan`]) in buffer(name from [`WithBufName`])
    #[inline]
    fn make_message(&self, reason: impl ToString) -> Message {
        Message::Rich {
            loc: self.get_span(),
            buf: self.buf_name().to_owned(),
            msg: reason.to_string(),
        }
    }

    /// make an [`Error`] with the given reason at location(from [`WithSpan`]) in buffer(name from [`WithBufName`])
    #[inline]
    fn make_error(&self, reason: impl ToString) -> Error {
        self.make_message(reason).into()
    }

    /// make an [`ParseError`] at location with ordered [`ParseErrorKind`]
    #[inline]
    fn make_parse_error(&self, reason: impl ToString, kind: ParseErrorKind) -> ParseError {
        self.make_error(reason).into_parse_error(kind)
    }

    /// make an Unmatched [`ParseError`] in [`Result`]
    fn unmatch<T>(&self, reason: impl ToString) -> Result<T, ParseError> {
        Err(self.make_parse_error(reason, ParseErrorKind::Unmatch))
    }

    /// make an Semantic [`ParseError`] in [`Result`]
    fn throw<T>(&self, reason: impl ToString) -> Result<T, ParseError> {
        Err(self.make_parse_error(reason, ParseErrorKind::Semantic))
    }
}

impl<M: WithBufName + WithSpan> MakeError for M {}
