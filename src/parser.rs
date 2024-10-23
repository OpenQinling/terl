use std::sync::Arc;

use crate::*;

#[derive(Debug, Clone, Copy)]
enum StartIdx {
    Unset(usize),
    Set(usize),
}

impl std::ops::Deref for StartIdx {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        match self {
            StartIdx::Unset(idx) | StartIdx::Set(idx) => idx,
        }
    }
}

impl std::ops::DerefMut for StartIdx {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            StartIdx::Unset(idx) | StartIdx::Set(idx) => idx,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct ParserState {
    /// the index of the first character in this [`ParseUnit`]
    start: StartIdx,
    /// parse state: the index of the current character in this [`ParseUnit`]
    idx: usize,
}

impl ParserState {
    pub(crate) fn fork(&self) -> ParserState {
        Self {
            start: StartIdx::Unset(self.idx),
            idx: self.idx,
        }
    }

    /// forward temp parser's work to main parser
    pub(crate) fn force_sync(mut self, tmp: &ParserState) -> Self {
        self.idx = tmp.idx;
        self.start = match (self.start, tmp.start) {
            (StartIdx::Unset(..), StartIdx::Set(idx)) => StartIdx::Set(idx),
            (idx, ..) => StartIdx::Set(*idx),
        };
        self
    }

    /// sync [`ParserState`] with the parsing result from another parser's [`ParserState`]
    pub(crate) fn sync_with<T, E>(self, tmp: &ParserState, result: &Result<T, E>) -> Self {
        if result.is_ok() {
            self.force_sync(tmp)
        } else {
            self
        }
    }
}

#[cfg(feature = "parser_calling_tree")]
pub mod calling_tree {
    use crate::{ParseErrorKind, Span};

    #[derive(Clone, Copy)]
    pub enum Calling {
        Start,
        Success(Span),
        Err(ParseErrorKind, Span),
    }

    impl Calling {
        pub fn new<P>(result: &Result<P, super::ParseError>, span: Span) -> Self {
            match result {
                Ok(_) => Self::Success(span),
                Err(e) => Self::Err(e.kind(), span),
            }
        }

        /// Returns `true` if the calling is [`Start`].
        ///
        /// [`Start`]: Calling::Start
        #[must_use]
        pub fn is_start(&self) -> bool {
            matches!(self, Self::Start)
        }
    }

    impl std::fmt::Debug for Calling {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Start => write!(f, "Start: "),
                Self::Success(span) => write!(f, "Success<{span:?}>"),
                Self::Err(arg0, span) => write!(f, "{:?}<{span:?}>", arg0),
            }
        }
    }

    #[derive(Debug, Clone)]
    enum Record {
        Normal { pu: &'static str, call: Calling },
        Custom { msg: String },
    }

    impl Record {
        fn print(&self, depth: &mut usize, f: &mut impl std::fmt::Write) -> std::fmt::Result {
            match self {
                Record::Normal { pu, call } => {
                    if call.is_start() {
                        for _ in 0..*depth {
                            write!(f, "    ")?;
                        }
                        *depth += 1;
                        writeln!(f, "{:?}{}", call, pu)
                    } else {
                        *depth -= 1;
                        for _ in 0..*depth {
                            write!(f, "    ")?;
                        }
                        writeln!(f, "{:?} {}", call, pu)
                    }
                }
                Record::Custom { msg } => writeln!(f, "{msg}"),
            }
        }
    }

    #[derive(Default, Debug, Clone)]
    pub struct CallingTree {
        records: Vec<Record>,
    }

    impl CallingTree {
        pub fn record_normal<P>(&mut self, call: Calling) {
            self.records.push(Record::Normal {
                pu: std::any::type_name::<P>(),
                call,
            });
        }

        pub fn record_custom(&mut self, msg: impl std::fmt::Display) {
            self.records.push(Record::Custom {
                msg: msg.to_string(),
            });
        }
    }

    impl std::fmt::Display for CallingTree {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let mut depth = 0;
            for record in &self.records {
                record.print(&mut depth, f)?;
            }
            Ok(())
        }
    }
}

/// An implementation of the language parser **without** any [`Clone::clone`] call!
#[derive(Debug)]
pub struct Parser<S: Source> {
    /// source codes
    buffer: Arc<dyn AsBuffer<S>>,
    state: ParserState,
    #[cfg(feature = "parser_calling_tree")]
    calling_tree: calling_tree::CallingTree,
}

impl<S: Source> WithSpan for Parser<S> {
    fn get_span(&self) -> Span {
        // while finishing parsing or throwing an error, the taking may not ever be started
        // so, match the case to make error reporting easier&better
        if self.start_idx() == self.current_idx() {
            Span::new(self.start_idx(), self.start_idx() + 1)
        } else {
            Span::new(self.start_idx(), self.current_idx())
        }
    }
}

impl<S: Source> WithBufName for Parser<S> {
    fn buf_name(&self) -> &Arc<str> {
        self.buffer.buf_name()
    }
}

impl<S: Source> Parser<S> {
    /// create a new parser from a slice of [char]
    pub fn new(src: Arc<dyn AsBuffer<S>>) -> Parser<S> {
        Parser {
            buffer: src,
            state: ParserState {
                start: StartIdx::Unset(0),
                idx: 0,
            },
            #[cfg(feature = "parser_calling_tree")]
            calling_tree: Default::default(),
        }
    }

    /// retuen slice to elements which [`Span`] selected
    ///
    /// # Panic
    ///
    /// panic if [`Span`] is out range
    #[inline]
    pub fn select(&self, span: Span) -> &[S] {
        &self.buffer.as_ref().as_ref()[span.start..span.end]
    }

    /// get the next item
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Option<&S> {
        let next = self.buffer.as_ref().as_ref().get(self.current_idx())?;
        self.state.idx += 1;
        Some(next)
    }

    /// return [`Some`] if the next item if it satisfies the given condition
    ///
    /// return [`None`] if the next item doesnot satisfy the given condition, or no item left
    pub fn next_if<C>(&mut self, cond: C) -> Option<&S>
    where
        C: FnOnce(&S) -> bool,
    {
        if self.peek().is_some_and(cond) {
            self.next()
        } else {
            None
        }
    }

    /// peek the next item
    #[inline]
    pub fn peek(&self) -> Option<&S> {
        let slice = self.buffer.as_ref().as_ref();
        slice.get(self.current_idx())
    }

    #[inline]
    pub(crate) fn start_idx(&self) -> usize {
        *self.state.start
    }

    #[inline]
    pub(crate) fn current_idx(&self) -> usize {
        self.state.idx
    }

    /// start taking items
    ///
    /// if start idx is unset, it will be set to current idx,
    /// or the calling makes no effort
    #[inline]
    pub fn start_taking(&mut self) {
        if let StartIdx::Unset(..) = self.state.start {
            self.state.start = StartIdx::Set(self.current_idx());
        }
    }

    /// get the [`CallingTree`] struct, you can print it directly
    ///
    /// [`CallingTree`]: calling_tree::CallingTree
    #[inline]
    #[cfg(feature = "parser_calling_tree")]
    pub fn calling_tree(&self) -> &calling_tree::CallingTree {
        &self.calling_tree
    }

    /// get a reference to the [`Buffer`] in [`Parser`]
    pub fn buffer(&self) -> &Arc<dyn AsBuffer<S>> {
        &self.buffer
    }

    /// be different from directly call, this kind of parse will log
    /// (if parser_calling_tree feature enabled)
    ///
    /// if the feature is disable, this method has no different with directly call
    pub fn parse_no_try<P: ParseUnit<S>>(&mut self, parser: P) -> ParseResult<P, S> {
        #[cfg(feature = "parser_calling_tree")]
        self.calling_tree
            .record_normal::<P>(calling_tree::Calling::Start);

        // do parsing

        let result = parser.parse(self);

        #[cfg(feature = "parser_calling_tree")]
        self.calling_tree
            .record_normal::<P>(calling_tree::Calling::new(&result, self.get_span()));

        #[cfg_attr(not(feature = "parser_calling_tree"), allow(clippy::let_and_return))]
        result
    }

    /// parse a [`ParseUnit`], if the parsing fail, throw a [`ParseError`] and the [`Parser`] will
    /// not be affected.
    #[inline]
    pub fn parse<P: ParseUnit<S>>(&mut self, parser: P) -> ParseResult<P, S> {
        // create a temp parser and reset its state

        let state = self.state;
        self.state = self.state.fork();

        let result = self.parse_no_try::<P>(parser);
        self.state = state.sync_with(&self.state, &result);

        result
    }

    /// Start to try parsing a [`ParseUnit`], it will try many times until you get a actually [`Error`]
    /// or successfully parse a [`ParseUnit`]
    pub fn r#try<P: ParseUnit<S>>(&mut self, p: P) -> Try<P::Result, S> {
        Try::new(self).or_try(|parser| p.parse(parser))
    }

    /// Try to parse a [`ParseUnit`], if the parsing fail because of unmatch, return [`None`]
    pub fn try_match<P: ParseUnit<S>>(&mut self, p: P) -> Result<Option<P::Result>, ParseError> {
        let result = self.parse(p);
        let is_unmatch = |e: &ParseError| e.kind() == ParseErrorKind::Unmatch;
        if result.as_ref().is_err_and(is_unmatch) {
            Ok(None)
        } else {
            result.map(Some)
        }
    }

    /// Match a [`ParseUnit`], unmatch error will alse be seen as a semantic error, throw a [`ParseError`]
    pub fn r#match<P: ParseUnit<S>>(&mut self, p: P) -> ParseResult<P, S> {
        self.parse(p)
            .map_err(|e| ParseError::from_error(e.error(), ParseErrorKind::Semantic))
    }
}

/// a [`Try`], allow you to try many times until you get a actually [`Error`]
/// or successfully parse a [`ParseUnit`]
pub struct Try<'p, R, S: Source> {
    parser: &'p mut Parser<S>,
    state: Option<Result<R, ParseError>>,
}

impl<'p, R, S: Source> Try<'p, R, S> {
    /// create a new [`Try`] from mutable reference to a [`Parser`]
    pub fn new(parser: &'p mut Parser<S>) -> Self {
        Self {
            parser,
            state: None,
        }
    }

    /// try once again
    ///
    /// do noting if the [`Try`] successfully parsed the [`ParseUnit`],
    /// or got a actually [`Error`]
    pub fn or_try<F>(mut self, parser: F) -> Self
    where
        F: FnOnce(&mut Parser<S>) -> Result<R, ParseError>,
    {
        let is_unmatch = |e: &ParseError| e.kind() == ParseErrorKind::Unmatch;
        let is_unmatch = |result: &Result<R, ParseError>| result.as_ref().is_err_and(is_unmatch);
        let is_unmatch = self.state.as_ref().is_some_and(is_unmatch);

        if self.state.is_none() || is_unmatch {
            let state = self.parser.parse(parser);
            self.state = Some(state);
        }
        self
    }

    /// finish parsing tring
    ///
    ///
    /// there should be at least one [`Self::or_try`] return [`Result::Ok`]
    /// or [`Result::Err`] , or panic
    pub fn finish(self) -> Result<R, ParseError> {
        self.state.expect("never parsed!")
    }
}
