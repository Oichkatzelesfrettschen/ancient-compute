"""
System F Lexer - Tokenizes System F (polymorphic lambda calculus) source code

System F features:
  - Lambda abstractions: \\x : T => expr
  - Type abstractions: /\\t => expr (or Lambda t => expr)
  - Type applications: expr [T]
  - Universal quantification: forall t. T or ∀ t. T
  - Base types: Nat, Int, Bool, String
  - Function types: T -> U
  - Polymorphic types: forall a. a -> a

Token types:
  - Keywords: forall, let, in, if, then, else, fix
  - Operators: ->, =>, :, ;
  - Delimiters: (, ), [, ], {, }
  - Literals: numbers, strings, identifiers
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum


class TokenType(str, Enum):
    """Token type enumeration for System F"""
    # Keywords
    FORALL = "FORALL"
    LET = "LET"
    IN = "IN"
    IF = "IF"
    THEN = "THEN"
    ELSE = "ELSE"
    FIX = "FIX"
    LAMBDA = "LAMBDA"
    TLAMBDA = "TLAMBDA"  # Type lambda /\

    # Type keywords
    NAT = "NAT"
    INT = "INT"
    BOOL = "BOOL"
    STRING = "STRING"

    # Literals and identifiers
    NUMBER = "NUMBER"
    STRING_LIT = "STRING_LIT"
    IDENTIFIER = "IDENTIFIER"
    TYPE_VAR = "TYPE_VAR"  # Lowercase t, u, v, etc.

    # Operators
    ARROW = "ARROW"  # ->
    FAT_ARROW = "FAT_ARROW"  # =>
    COLON = "COLON"  # :
    DOT = "DOT"  # .
    SEMICOLON = "SEMICOLON"  # ;

    # Delimiters
    LPAREN = "LPAREN"
    RPAREN = "RPAREN"
    LBRACKET = "LBRACKET"
    RBRACKET = "RBRACKET"
    LBRACE = "LBRACE"
    RBRACE = "RBRACE"

    # Special
    BACKSLASH = "BACKSLASH"  # \
    TYPE_BACKSLASH = "TYPE_BACKSLASH"  # /\
    COMMA = "COMMA"
    EQUALS = "EQUALS"

    # Control
    COMMENT = "COMMENT"
    WHITESPACE = "WHITESPACE"
    EOF = "EOF"


@dataclass
class Token:
    """Represents a single token"""
    type: TokenType
    value: str
    line: int
    column: int

    def __repr__(self) -> str:
        return f"Token({self.type}, {self.value!r}, {self.line}, {self.column})"


class SystemFLexer:
    """Tokenizes System F source code"""

    KEYWORDS = {
        'forall': TokenType.FORALL,
        'let': TokenType.LET,
        'in': TokenType.IN,
        'if': TokenType.IF,
        'then': TokenType.THEN,
        'else': TokenType.ELSE,
        'fix': TokenType.FIX,
        'Nat': TokenType.NAT,
        'Int': TokenType.INT,
        'Bool': TokenType.BOOL,
        'String': TokenType.STRING,
    }

    def __init__(self, source: str) -> None:
        """Initialize lexer with source code"""
        self.source = source
        self.pos = 0
        self.line = 1
        self.column = 1
        self.tokens: list[Token] = []

    def tokenize(self) -> list[Token]:
        """Tokenize entire source and return token list"""
        while self.pos < len(self.source):
            # Skip whitespace
            if self.current() in ' \t':
                self._skip_whitespace()
                continue

            # Handle newlines
            if self.current() == '\n':
                self.line += 1
                self.column = 0
                self._advance()
                continue

            # Line comments
            if self.current() == '-' and self.peek() == '-':
                self._skip_line_comment()
                continue

            # Block comments
            if self.current() == '{' and self.peek() == '-':
                self._skip_block_comment()
                continue

            ch = self.current()

            # Type lambda /\
            if ch == '/' and self.peek() == '\\':
                self._add_token(TokenType.TYPE_BACKSLASH, '/\\')
                self._advance()
                self._advance()
                continue

            # Arrow operators
            if ch == '-' and self.peek() == '>':
                self._add_token(TokenType.ARROW, '->')
                self._advance()
                self._advance()
                continue

            if ch == '=' and self.peek() == '>':
                self._add_token(TokenType.FAT_ARROW, '=>')
                self._advance()
                self._advance()
                continue

            # Single character tokens
            if ch == '(':
                self._add_token(TokenType.LPAREN, '(')
                self._advance()
            elif ch == ')':
                self._add_token(TokenType.RPAREN, ')')
                self._advance()
            elif ch == '[':
                self._add_token(TokenType.LBRACKET, '[')
                self._advance()
            elif ch == ']':
                self._add_token(TokenType.RBRACKET, ']')
                self._advance()
            elif ch == '{':
                self._add_token(TokenType.LBRACE, '{')
                self._advance()
            elif ch == '}':
                self._add_token(TokenType.RBRACE, '}')
                self._advance()
            elif ch == ':':
                self._add_token(TokenType.COLON, ':')
                self._advance()
            elif ch == '.':
                self._add_token(TokenType.DOT, '.')
                self._advance()
            elif ch == ';':
                self._add_token(TokenType.SEMICOLON, ';')
                self._advance()
            elif ch == ',':
                self._add_token(TokenType.COMMA, ',')
                self._advance()
            elif ch == '=':
                self._add_token(TokenType.EQUALS, '=')
                self._advance()
            elif ch == '\\':
                self._add_token(TokenType.BACKSLASH, '\\')
                self._advance()

            # Number
            elif ch.isdigit():
                self._scan_number()

            # String literal
            elif ch == '"':
                self._scan_string()

            # Identifier or keyword
            elif ch.isalpha() or ch == '_':
                self._scan_identifier()

            else:
                self._advance()

        self._add_token(TokenType.EOF, '')
        return self.tokens

    def current(self) -> str | None:
        """Get current character"""
        if self.pos < len(self.source):
            return self.source[self.pos]
        return None

    def peek(self, offset: int = 1) -> str | None:
        """Peek at character at current position + offset"""
        pos = self.pos + offset
        if pos < len(self.source):
            return self.source[pos]
        return None

    def _advance(self) -> None:
        """Move to next character"""
        if self.pos < len(self.source):
            if self.source[self.pos] == '\n':
                self.line += 1
                self.column = 1
            else:
                self.column += 1
            self.pos += 1

    def _skip_whitespace(self) -> None:
        """Skip whitespace"""
        while self.current() in ' \t':
            self._advance()

    def _skip_line_comment(self) -> None:
        """Skip line comment"""
        while self.current() and self.current() != '\n':
            self._advance()

    def _skip_block_comment(self) -> None:
        """Skip block comment {- ... -}"""
        self._advance()  # {
        self._advance()  # -

        depth = 1
        while self.pos < len(self.source) and depth > 0:
            if self.current() == '{' and self.peek() == '-':
                depth += 1
                self._advance()
                self._advance()
            elif self.current() == '-' and self.peek() == '}':
                depth -= 1
                self._advance()
                self._advance()
            else:
                self._advance()

    def _scan_number(self) -> None:
        """Scan numeric literal"""
        start_line = self.line
        start_col = self.column
        value = ''

        while self.current() and self.current().isdigit():
            value += self.current()
            self._advance()

        # Float
        if self.current() == '.' and self.peek() and self.peek().isdigit():
            value += '.'
            self._advance()
            while self.current() and self.current().isdigit():
                value += self.current()
                self._advance()

        self.tokens.append(Token(TokenType.NUMBER, value, start_line, start_col))

    def _scan_string(self) -> None:
        """Scan string literal"""
        start_line = self.line
        start_col = self.column
        self._advance()  # consume opening "

        value = ''
        while self.pos < len(self.source) and self.current() != '"':
            if self.current() == '\\' and self.peek():
                self._advance()
                next_ch = self.current()
                if next_ch == 'n':
                    value += '\n'
                elif next_ch == 't':
                    value += '\t'
                elif next_ch == 'r':
                    value += '\r'
                elif next_ch == '\\':
                    value += '\\'
                elif next_ch == '"':
                    value += '"'
                else:
                    value += next_ch
                self._advance()
            else:
                value += self.current()
                self._advance()

        if self.current() == '"':
            self._advance()

        self.tokens.append(Token(TokenType.STRING_LIT, value, start_line, start_col))

    def _scan_identifier(self) -> None:
        """Scan identifier or keyword"""
        start_line = self.line
        start_col = self.column
        value = ''

        while self.current() and (self.current().isalnum() or self.current() in '_\''):
            value += self.current()
            self._advance()

        # Check if keyword
        token_type = self.KEYWORDS.get(value, TokenType.IDENTIFIER)

        # Type variables are single lowercase letters (t, u, v, a, b, etc.)
        if token_type == TokenType.IDENTIFIER and len(value) == 1 and value.islower():
            token_type = TokenType.TYPE_VAR

        self.tokens.append(Token(token_type, value, start_line, start_col))

    def _add_token(self, token_type: TokenType, value: str) -> None:
        """Add token to token list"""
        self.tokens.append(Token(token_type, value, self.line, self.column))
