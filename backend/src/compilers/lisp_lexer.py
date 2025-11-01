"""
LISP Lexer - Tokenizes LISP source code for compilation to Babbage ISA

Features:
  - S-expression tokenization (parentheses, atoms, symbols)
  - Symbol, number, and string literal parsing
  - Comment handling (;)
  - Quote/quasiquote/unquote shorthand
  - Error reporting with line/column information

Token types:
  - Delimiters: LPAREN, RPAREN
  - Literals: NUMBER, STRING, SYMBOL, KEYWORD
  - Operators: QUOTE, QUASIQUOTE, UNQUOTE
  - Control: COMMENT, WHITESPACE, EOF
"""

from __future__ import annotations
from enum import Enum
from dataclasses import dataclass
from typing import List, Optional


class TokenType(str, Enum):
    """Token type enumeration for LISP"""
    # Delimiters
    LPAREN = "LPAREN"
    RPAREN = "RPAREN"

    # Literals
    NUMBER = "NUMBER"
    STRING = "STRING"
    SYMBOL = "SYMBOL"
    KEYWORD = "KEYWORD"

    # Quote operators
    QUOTE = "QUOTE"           # '
    QUASIQUOTE = "QUASIQUOTE" # `
    UNQUOTE = "UNQUOTE"       # ,
    UNQUOTE_SPLICING = "UNQUOTE_SPLICING"  # ,@

    # Keywords (special forms)
    IF = "IF"
    LET = "LET"
    DEFUN = "DEFUN"
    LAMBDA = "LAMBDA"
    QUOTE_KW = "QUOTE_KW"
    COND = "COND"
    CASE = "CASE"

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


class LISPLexer:
    """Tokenizes LISP source code"""

    KEYWORDS = {
        'if': TokenType.IF,
        'let': TokenType.LET,
        'defun': TokenType.DEFUN,
        'lambda': TokenType.LAMBDA,
        'quote': TokenType.QUOTE_KW,
        'cond': TokenType.COND,
        'case': TokenType.CASE,
    }

    def __init__(self, source: str) -> None:
        """Initialize lexer with source code"""
        self.source = source
        self.pos = 0
        self.line = 1
        self.column = 1
        self.tokens: List[Token] = []

    def tokenize(self) -> List[Token]:
        """Tokenize entire source and return token list"""
        while self.pos < len(self.source):
            self._skip_whitespace()

            if self.pos >= len(self.source):
                break

            ch = self.source[self.pos]

            # Comments (line comments with ;)
            if ch == ';':
                self._skip_comment()
                continue

            # Left paren
            if ch == '(':
                self._add_token(TokenType.LPAREN, '(')
                self._advance()
                continue

            # Right paren
            if ch == ')':
                self._add_token(TokenType.RPAREN, ')')
                self._advance()
                continue

            # Quote shorthand
            if ch == "'":
                self._add_token(TokenType.QUOTE, "'")
                self._advance()
                continue

            # Quasiquote shorthand
            if ch == '`':
                self._add_token(TokenType.QUASIQUOTE, '`')
                self._advance()
                continue

            # Unquote shorthand
            if ch == ',':
                if self.pos + 1 < len(self.source) and self.source[self.pos + 1] == '@':
                    self._add_token(TokenType.UNQUOTE_SPLICING, ',@')
                    self._advance()
                    self._advance()
                else:
                    self._add_token(TokenType.UNQUOTE, ',')
                    self._advance()
                continue

            # String literal
            if ch == '"':
                self._scan_string()
                continue

            # Number (including negative)
            if ch.isdigit() or (ch == '-' and self.pos + 1 < len(self.source) and
                                self.source[self.pos + 1].isdigit()):
                self._scan_number()
                continue

            # Symbol/Keyword
            if ch.isalpha() or ch in '_+-*/<>=!?#':
                self._scan_atom()
                continue

            # Unknown character - skip (lenient)
            self._advance()

        self._add_token(TokenType.EOF, '')
        return self.tokens

    def _advance(self) -> None:
        """Move to next character"""
        if self.pos < len(self.source):
            if self.source[self.pos] == '\n':
                self.line += 1
                self.column = 1
            else:
                self.column += 1
            self.pos += 1

    def _peek(self, offset: int = 0) -> Optional[str]:
        """Peek at character at current position + offset"""
        pos = self.pos + offset
        if pos < len(self.source):
            return self.source[pos]
        return None

    def _skip_whitespace(self) -> None:
        """Skip whitespace (space, tab, newline)"""
        while self.pos < len(self.source) and self.source[self.pos].isspace():
            self._advance()

    def _skip_comment(self) -> None:
        """Skip line comment until end of line"""
        while self.pos < len(self.source) and self.source[self.pos] != '\n':
            self._advance()
        if self.pos < len(self.source):
            self._advance()

    def _scan_string(self) -> None:
        """Scan string literal with escape sequence handling"""
        start_line = self.line
        start_col = self.column
        self._advance()  # consume opening "

        value = ''
        while self.pos < len(self.source) and self.source[self.pos] != '"':
            if self.source[self.pos] == '\\' and self.pos + 1 < len(self.source):
                self._advance()
                next_ch = self.source[self.pos]
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
                value += self.source[self.pos]
                self._advance()

        if self.pos < len(self.source):
            self._advance()  # consume closing "

        self.tokens.append(Token(TokenType.STRING, value, start_line, start_col))

    def _scan_number(self) -> None:
        """Scan numeric literal (integer or float)"""
        start_line = self.line
        start_col = self.column
        value = ''

        # Handle negative sign
        if self.source[self.pos] == '-':
            value += '-'
            self._advance()

        # Integer part
        while self.pos < len(self.source) and self.source[self.pos].isdigit():
            value += self.source[self.pos]
            self._advance()

        # Decimal part
        if self.pos < len(self.source) and self.source[self.pos] == '.':
            value += '.'
            self._advance()

            while self.pos < len(self.source) and self.source[self.pos].isdigit():
                value += self.source[self.pos]
                self._advance()

        self.tokens.append(Token(TokenType.NUMBER, value, start_line, start_col))

    def _scan_atom(self) -> None:
        """Scan atom (symbol, keyword, or special operator name)"""
        start_line = self.line
        start_col = self.column
        value = ''

        while (self.pos < len(self.source) and
               (self.source[self.pos].isalnum() or self.source[self.pos] in '_+-*/<>=!?#*')):
            value += self.source[self.pos]
            self._advance()

        # Check if it's a keyword
        token_type = self.KEYWORDS.get(value.lower(), TokenType.SYMBOL)
        self.tokens.append(Token(token_type, value, start_line, start_col))

    def _add_token(self, token_type: TokenType, value: str) -> None:
        """Add token to token list"""
        self.tokens.append(Token(token_type, value, self.line, self.column))
