//! Generates a stream of tokens from a string.

use logos::{Lexer, Logos, Span};

use super::Spanned;

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

    #[token(".")]
    Dot,
    #[token("&")]
    Ampersand,

    #[token("for")]
    For,

    #[token("in")]
    In,

    #[token(":")]
    Colon,
    #[token(";")]
    SemiColon,
    #[token(",")]
    Comma,

    #[token("true")]
    True,
    #[token("false")]
    False,

    #[regex(r#""(\\[\\"]|[^"])*""#)]
    String,

    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Identifer,

    #[regex(r#"[0-9]+"#)]
    IntegerNumber,
    #[regex(r#"[0-9]*\.?[0-9]*"#)]
    FloatNumber,

    #[token("i8")]
    #[token("i16")]
    #[token("i32")]
    #[token("i64")]
    #[token("u8")]
    #[token("u16")]
    #[token("u32")]
    #[token("u64")]
    #[token("f32")]
    #[token("f64")]
    NumberType,
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
    pub fn new(src: &'a str) -> Self {
        let mut lexer = Token::lexer(src);

        let current_slice = lexer.slice();
        let current_span = lexer.span();
        let next = lexer.next();

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
    pub fn peek(&self) -> &Option<Result<Token, FailedToLexCharacter>> {
        &self.next
    }

    /// Get the range for the current [`Token`] in `Source`.
    pub fn span(&self) -> Span {
        self.current_span.clone()
    }

    /// Get a [`str`] slice of the current [`Token`].
    pub fn slice(&self) -> &str {
        self.current_slice
    }

    /// Get a [`str`] slice of the next [`Token`].
    pub fn peek_slice(&self) -> &str {
        self.lexer.slice()
    }

    /// Get a [`Span`] of the next [`Token`].
    pub fn peek_span(&self) -> Span {
        self.lexer.span()
    }

    /// Wraps `T` in a [`Spanned<T>`] which contains the range for the current token in `Source`.
    pub fn wrap_span<T>(&self, value: T) -> Spanned<T> {
        Spanned {
            value,
            span: self.span(),
        }
    }
}

impl<'a> Iterator for TokenStream<'a> {
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

        assert_eq!(lexer.next(), Some(Ok(Token::Identifer)));
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
