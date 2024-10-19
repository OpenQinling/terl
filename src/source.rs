use std::{fmt::Write, sync::Arc};

use crate::*;

/// the source of [`Parser`]
///
/// Indicate how to format the error to String
pub trait Source: Sized {
    /// type is needed for [`Source::handle_error`]
    type HandleErrorWith<'b>: ?Sized;
    /// handle an [`Error`] by formatting it into a string
    ///
    /// This function formats an error into a string. It iterates through the error's messages,
    /// formatting each one and appending it to the `buffer`. If any formatting fails,
    /// it returns an error.
    ///
    /// # Parameters
    ///
    /// * `with` - A reference to the [`Source::HandleErrorWith`], which is used to format the error.
    /// * `error` - The error to be formatted and displayed.
    ///
    /// # Returns
    ///
    /// A [`String`] containing the formatted error message if successful, or panic if the formatting fails.
    fn handle_error(with: &Self::HandleErrorWith<'_>, error: &Error) -> String {
        (|| -> Result<_, std::fmt::Error> {
            let mut output = String::new();
            for msg in error.messages() {
                Self::handle_message(with, &mut output, msg)?;
            }
            Ok(output)
        })()
        .unwrap()
    }

    /// Handle a message by formatting it into a string.
    ///
    /// This function formats a message into a string. It handles different types of messages and formats them accordingly.
    ///
    /// # Parameters
    ///
    /// * `with` - A reference to the [`Source::HandleErrorWith`], which is used to format the error.
    /// * `buffer` - A mutable reference to a [`Write`] trait object, which is used to write the formatted error message.
    /// * `message` - The message to be formatted and displayed.
    ///
    /// # Returns
    ///
    /// A [`Result`] containing the formatted error message as a [`String`] if successful, or an [`std::fmt::Error`] if the formatting fails.
    fn handle_message<S>(
        with: &Self::HandleErrorWith<'_>,
        output: &mut S,
        message: &Message,
    ) -> std::fmt::Result
    where
        S: Write,
    {
        match message {
            Message::Location { loc, buf } => Self::handle_location(with, output, *loc, buf, ""),
            Message::Text { msg, buf } => {
                writeln!(output, "in {buf}: {msg}")
            }
            Message::Rich { msg, buf, loc } => Self::handle_location(with, output, *loc, buf, msg),
        }?;
        Ok(())
    }

    /// Formats a message on given location to [`String`].
    ///
    /// This function formats a message on the given location to a string. It handles different types of messages and formats them accordingly.
    ///
    /// # Parameters
    ///
    /// * `with` - A reference to the [`Source::HandleErrorWith`] trait object, which is used to format the error.
    /// * `buffer` - A mutable reference to a [`Write`] trait object, which is used to write the formatted error message.
    /// * `loc` - The [`Span`] representing the location where the error occurred.
    /// * `msg` - The message to be formatted and displayed.
    ///
    /// # Returns
    ///
    /// A [`Result`] containing the formatted error message as a [`String`] if successful, or an [`std::fmt::Error`] if the formatting fails.
    fn handle_location<S>(
        with: &Self::HandleErrorWith<'_>,
        output: &mut S,
        loc: Span,
        buf: &str,
        msg: &str,
    ) -> std::fmt::Result
    where
        S: Write;
}

impl Source for char {
    type HandleErrorWith<'b> = [char];

    #[cfg(not(feature = "color"))]
    fn handle_location<S>(
        with: &AsBuffer<char>,
        buffer: &mut S,
        span: Span,
        buf: &str,
        msg: &str,
    ) -> std::fmt::Result
    where
        S: Write,
    {
        let src = with;
        let start_line_start = (0..span.start)
            .rev()
            .find(|idx| src[*idx] == '\n')
            .map(|idx| idx + 1)
            .unwrap_or(0);
        let mut line_num = (0..span.start).filter(|idx| src[*idx] == '\n').count() + 1;
        let mut idx = start_line_start;

        let row_num = span.start - start_line_start + 1;
        let location = format!("[{}:{}:{}]", src.name(), line_num, row_num,);

        writeln!(buffer, "{location}: {}", msg)?;

        while idx < span.end && idx < src.len() {
            let line_start = idx;

            let head = format!("at line {line_num} | ");

            let mut space_len = head.len();
            while idx < span.start {
                space_len += 1;
                idx += 1;
            }
            let mut hats = (0..space_len).map(|_| ' ').collect::<String>();
            while idx < src.len() && src[idx] != '\n' {
                if idx < span.end {
                    hats.push('^');
                }
                idx += 1;
            }

            let line = src[line_start..idx].iter().collect::<String>();
            if !line.is_empty() {
                writeln!(buffer, "{head}{line}")?;
                writeln!(buffer, "{hats}")?;
            }

            idx = (idx + 1).min(src.len());
            line_num += 1;
        }
        Ok(())
    }

    #[cfg(feature = "color")]
    fn handle_location<S>(
        src: &Self::HandleErrorWith<'_>,
        buffer: &mut S,
        loc: Span,
        buf_name: &str,
        msg: &str,
    ) -> std::fmt::Result
    where
        S: Write,
    {
        use colored::Colorize;

        let (start_line, start_line_start) = (0..loc.start).fold((0, 0), |(lines, start), idx| {
            if src[idx] == '\n' {
                (lines + 1, idx + 1)
            } else {
                (lines, start)
            }
        });
        let (end_line, end_line_end) = (loc.start..)
            .filter(|idx| src[*idx] == '\n')
            .enumerate()
            .find_map(|(lines, idx)| {
                if idx >= loc.end {
                    Some((start_line + lines, idx))
                } else {
                    None
                }
            })
            .unwrap();

        let start_row = loc.start - start_line_start;
        let location = format!("[{}:{}:{}]", buf_name, start_line + 1, start_row + 1,);
        writeln!(buffer, "{location}: {}", msg)?;

        let text_before_len = loc.start - start_line_start;
        let last_line_error_len = end_line_end - loc.end;

        for (line_num, mut line) in src[start_line_start..end_line_end]
            .split(|c| *c == '\n')
            .enumerate()
        {
            let line_num = line_num + start_line;
            buffer.write_fmt(format_args!("at line {} | ", line_num + 1))?;
            if line_num == start_line && text_before_len != 0 {
                for char in line[..text_before_len].iter() {
                    buffer.write_char(*char)?;
                }
                line = &line[text_before_len..];
            }

            if line_num == end_line {
                let error = line[..last_line_error_len].iter().collect::<String>();
                buffer.write_fmt(format_args!("{}", error.red().underline()))?;

                for char in line[last_line_error_len..].iter() {
                    buffer.write_char(*char)?;
                }
            } else {
                let error = line[..].iter().collect::<String>();
                buffer.write_fmt(format_args!("{}", error.red().underline()))?;
            }
        }

        Ok(())
    }
}

/// The buffer,store source whihch is needed by [`Parser`]
pub trait AsBuffer<S>: core::fmt::Debug + AsRef<[S]> {
    /// Returns the name of the source, usually the file name.
    fn buf_name(&self) -> &Arc<str>;

    /// `trait_upcasting` is unstable.
    fn debug(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.fmt(f)
    }
}

pub struct Buffer<'a, S>(&'a dyn AsBuffer<S>);

impl<S> Buffer<'_, S> {
    pub fn new(src: &'_ dyn AsBuffer<S>) -> Buffer<'_, S> {
        Buffer(src)
    }
}

impl<S> core::fmt::Debug for Buffer<'_, S> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        struct Healper<F>(F)
        where
            F: Fn(&mut core::fmt::Formatter) -> core::fmt::Result;

        impl<F> core::fmt::Debug for Healper<F>
        where
            F: Fn(&mut core::fmt::Formatter) -> core::fmt::Result,
        {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                self.0(f)
            }
        }

        // while core::fmt::Formatter::field_with is unstable...
        f.debug_tuple("Buffer")
            .field(&Healper(|f| self.0.debug(f)))
            .finish()
    }
}

impl<S> AsBuffer<S> for Buffer<'_, S> {
    fn buf_name(&self) -> &Arc<str> {
        self.0.buf_name()
    }
}

impl<S> AsRef<[S]> for Buffer<'_, S> {
    fn as_ref(&self) -> &[S] {
        self.0.as_ref()
    }
}

impl<S> core::ops::Deref for Buffer<'_, S> {
    type Target = [S];

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}

#[derive(Debug)]
pub struct FileBuffer {
    path: Arc<str>,
    src: Vec<char>,
}

impl FileBuffer {
    pub fn new(path: Arc<str>, src: Vec<char>) -> Self {
        Self { path, src }
    }
}

impl AsBuffer<char> for FileBuffer {
    fn buf_name(&self) -> &Arc<str> {
        &self.path
    }
}

impl AsRef<[char]> for FileBuffer {
    fn as_ref(&self) -> &[char] {
        &self.src
    }
}
