//! Generates a stream of tokens from a string.

use logos::{Lexer, Logos, Span};

#[derive(Debug, Clone, Default, PartialEq)]
pub struct FailedToLexCharacter;

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\n\f]+", error = FailedToLexCharacter)]
pub enum Token {
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,

    #[token("{")]
    LeftBracket,
    #[token("}")]
    RightBracket,

    #[token("[")]
    LeftBrace,
    #[token("]")]
    RightBrace,

    #[token("=")]
    Equals,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("/")]
    Slash,
    #[token("*")]
    Asterisk,
    #[token("%")]
    Modulo,

    #[token("!")]
    Not,
    #[token("^")]
    Xor,

    #[token(".", priority = 10)]
    Dot,
    #[token("&")]
    Ampersand,

    #[token("loop")]
    Loop,
    #[token("for")]
    For,
    #[token("while")]
    While,
    #[token("if")]
    If,

    #[token("in")]
    In,

    #[token(":")]
    Colon,
    #[token(";")]
    SemiColon,
    #[token(",")]
    Comma,
    #[token("|")]
    Pipe,

    #[token("true")]
    True,
    #[token("false")]
    False,

    #[regex(r#""(\\[\\"]|[^"])*""#)]
    String,

    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Identifier,

    #[regex(r#"[0-9]+[A-Za-z0-9_]*"#)]
    IntegerNumber,
    #[regex(r#"[0-9]+\.[0-9]*[A-Za-z0-9_]*"#)]
    FloatNumber,
}

/// A wrapper for the lexer which provides token peeking and other helper functions
#[derive(Debug)]
pub struct TokenStream<'a> {
    lexer: Lexer<'a, Token>,
    next: Option<Result<Token, FailedToLexCharacter>>,
    current_slice: &'a str,
    current_span: Span,
}

impl<'a> TokenStream<'a> {
    /// Creates a new [`TokenStream`] from `src`.
    #[must_use]
    pub fn new(src: &'a str) -> Self {
        let mut lexer = Token::lexer(src);

        let next = lexer.next();
        let (current_span, current_slice) = if let Some(Err(FailedToLexCharacter)) = next {
            (0..1, &src[0..1])
        } else {
            (lexer.span(), lexer.slice())
        };

        Self {
            lexer,
            next,
            current_slice,
            current_span,
        }
    }

    /// Returns the next [`Token`] and advances the iterator
    pub fn next(&mut self) -> Option<Result<Token, FailedToLexCharacter>> {
        let val = self.next.take();
        self.current_slice = self.lexer.slice();
        self.current_span = self.lexer.span();
        self.next = self.lexer.next();

        val
    }

    /// Returns advances the iterator and discards the [`Token`]
    pub fn skip_one(&mut self) -> &mut Self {
        self.next();
        self
    }

    // pub fn next_pe(&mut self) -> Result<Token, ParseError> {
    //     let token = self.next();

    //     self.to_parse_error(token)
    // }

    // pub fn to_parse_error(
    //     &mut self,
    //     token: Option<Result<Token, FailedToLexCharacter>>,
    // ) -> Result<Token, ParseError> {
    //     Ok(token
    //         .ok_or(ParseError::ExpectedMoreTokens(self.span()))?
    //         .map_err(|FailedToLexCharacter| ParseError::FailedToLexCharacter(self.span()))?)
    // }

    /// Returns a reference to next [`Token`] without advancing the iterator
    #[inline]
    #[must_use]
    pub fn peek(&self) -> &Option<Result<Token, FailedToLexCharacter>> {
        &self.next
    }

    /// Get the range for the current [`Token`] in `Source`.
    #[inline]
    #[must_use]
    pub fn span(&self) -> Span {
        self.current_span.clone()
    }

    /// Advances the stream until a certain [`Token`] is reached and returns the entire span between now and that [`Token`].
    #[must_use]
    pub fn span_until(&mut self, token: Token) -> Span {
        let start = self.current_span.start;
        loop {
            match self.next() {
                Some(Ok(t)) if t == token => break,
                Some(Err(_)) | None => break,
                Some(Ok(_)) => {}
            }
        }

        Span {
            start,
            end: self.current_span.end,
        }
    }

    /// Get a [`str`] slice of the current [`Token`].
    #[inline]
    #[must_use]
    pub fn slice(&self) -> &str {
        self.current_slice
    }

    /// Get a [`str`] slice of the next [`Token`].
    #[inline]
    #[must_use]
    pub fn peek_slice(&self) -> &str {
        self.lexer.slice()
    }

    /// Get a [`Span`] of the next [`Token`].
    #[inline]
    #[must_use]
    pub fn peek_span(&self) -> Span {
        self.lexer.span()
    }
}

impl Iterator for TokenStream<'_> {
    type Item = Result<Token, FailedToLexCharacter>;

    #[inline]
    fn next(&mut self) -> Option<Result<Token, FailedToLexCharacter>> {
        self.next()
    }
}

#[cfg(test)]
mod tests {
    use super::{Token, TokenStream};

    #[test]
    fn var_assign() {
        let mut lexer = TokenStream::new("x = 1 + 2 - 30.6");

        assert_eq!(lexer.next(), Some(Ok(Token::Identifier)));
        assert_eq!(lexer.slice(), "x");

        assert_eq!(lexer.next(), Some(Ok(Token::Equals)));
        assert_eq!(lexer.slice(), "=");

        assert_eq!(lexer.next(), Some(Ok(Token::IntegerNumber)));
        assert_eq!(lexer.slice(), "1");

        assert_eq!(lexer.next(), Some(Ok(Token::Plus)));
        assert_eq!(lexer.slice(), "+");

        assert_eq!(lexer.next(), Some(Ok(Token::IntegerNumber)));
        assert_eq!(lexer.slice(), "2");

        assert_eq!(lexer.next(), Some(Ok(Token::Minus)));
        assert_eq!(lexer.slice(), "-");

        assert_eq!(lexer.next(), Some(Ok(Token::FloatNumber)));
        assert_eq!(lexer.slice(), "30.6");
    }
}
