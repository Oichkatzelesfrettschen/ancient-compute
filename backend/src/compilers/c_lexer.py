"""C Lexer -- tokenisation for the C freestanding subset.

Provides TokenType, Token, and CLexer for downstream use by the C parser.
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum


class TokenType(Enum):
    """Token types for C lexical analysis."""

    # Literals
    INT_LIT = "INT_LIT"
    FLOAT_LIT = "FLOAT_LIT"
    STR_LIT = "STR_LIT"
    CHAR_LIT = "CHAR_LIT"

    # Identifiers and keywords
    IDENT = "IDENT"
    INT = "INT"
    FLOAT = "FLOAT"
    VOID = "VOID"
    IF = "IF"
    ELSE = "ELSE"
    WHILE = "WHILE"
    FOR = "FOR"
    RETURN = "RETURN"

    # Operators
    PLUS = "PLUS"
    MINUS = "MINUS"
    STAR = "STAR"
    SLASH = "SLASH"
    PERCENT = "PERCENT"
    ASSIGN = "ASSIGN"
    EQ = "EQ"
    NE = "NE"
    LT = "LT"
    LE = "LE"
    GT = "GT"
    GE = "GE"
    AND = "AND"
    OR = "OR"
    NOT = "NOT"
    AMPERSAND = "AMPERSAND"

    # Delimiters
    LPAREN = "LPAREN"
    RPAREN = "RPAREN"
    LBRACE = "LBRACE"
    RBRACE = "RBRACE"
    LBRACKET = "LBRACKET"
    RBRACKET = "RBRACKET"
    SEMICOLON = "SEMICOLON"
    COMMA = "COMMA"

    # Special
    EOF = "EOF"
    NEWLINE = "NEWLINE"


@dataclass
class Token:
    """Represents a single token in the C source."""

    type: TokenType
    value: str
    line: int
    col: int


class CLexer:
    """Lexical analyzer for C code."""

    KEYWORDS = {
        "int": TokenType.INT,
        "float": TokenType.FLOAT,
        "void": TokenType.VOID,
        "if": TokenType.IF,
        "else": TokenType.ELSE,
        "while": TokenType.WHILE,
        "for": TokenType.FOR,
        "return": TokenType.RETURN,
    }

    def __init__(self, source: str) -> None:
        """Initialize lexer with C source code."""
        self.source = source
        self.pos = 0
        self.line = 1
        self.col = 1
        self.tokens: list[Token] = []

    def tokenize(self) -> list[Token]:
        """Tokenize the entire source and return list of tokens."""
        while self.pos < len(self.source):
            self._skip_whitespace_and_comments()

            if self.pos >= len(self.source):
                break

            ch = self.source[self.pos]

            # Newline tracking
            if ch == "\n":
                self.line += 1
                self.col = 1
                self.pos += 1
                continue

            # Numbers
            if ch.isdigit():
                self._read_number()
            # Identifiers and keywords
            elif ch.isalpha() or ch == "_":
                self._read_identifier()
            # String literals
            elif ch == '"':
                self._read_string()
            # Character literals
            elif ch == "'":
                self._read_char()
            # Operators and delimiters
            elif self._read_operator():
                pass
            else:
                self.pos += 1
                self.col += 1

        self.tokens.append(Token(TokenType.EOF, "", self.line, self.col))
        return self.tokens

    def _skip_whitespace_and_comments(self) -> None:
        """Skip whitespace and comments."""
        while self.pos < len(self.source):
            ch = self.source[self.pos]

            if ch in " \t\r":
                self.pos += 1
                self.col += 1
            elif ch == "\n":
                break
            elif self.pos + 1 < len(self.source) and ch == "/" and self.source[self.pos + 1] == "/":
                # Skip line comment
                while self.pos < len(self.source) and self.source[self.pos] != "\n":
                    self.pos += 1
            elif self.pos + 1 < len(self.source) and ch == "/" and self.source[self.pos + 1] == "*":
                # Skip block comment
                self.pos += 2
                self.col += 2
                while self.pos + 1 < len(self.source):
                    if self.source[self.pos] == "*" and self.source[self.pos + 1] == "/":
                        self.pos += 2
                        self.col += 2
                        break
                    if self.source[self.pos] == "\n":
                        self.line += 1
                        self.col = 1
                    else:
                        self.col += 1
                    self.pos += 1
            else:
                break

    def _read_number(self) -> None:
        """Read an integer or float literal."""
        start = self.pos
        start_col = self.col

        while self.pos < len(self.source) and self.source[self.pos].isdigit():
            self.pos += 1
            self.col += 1

        # Check for float
        is_float = False
        if self.pos < len(self.source) and self.source[self.pos] == ".":
            is_float = True
            self.pos += 1
            self.col += 1
            while self.pos < len(self.source) and self.source[self.pos].isdigit():
                self.pos += 1
                self.col += 1

        value = self.source[start : self.pos]
        token_type = TokenType.FLOAT_LIT if is_float else TokenType.INT_LIT
        self.tokens.append(Token(token_type, value, self.line, start_col))

    def _read_identifier(self) -> None:
        """Read an identifier or keyword."""
        start = self.pos
        start_col = self.col

        while self.pos < len(self.source) and (
            self.source[self.pos].isalnum() or self.source[self.pos] == "_"
        ):
            self.pos += 1
            self.col += 1

        value = self.source[start : self.pos]
        token_type = self.KEYWORDS.get(value, TokenType.IDENT)
        self.tokens.append(Token(token_type, value, self.line, start_col))

    def _read_string(self) -> None:
        """Read a string literal."""
        start_col = self.col
        self.pos += 1  # Skip opening quote
        self.col += 1

        value = ""
        while self.pos < len(self.source) and self.source[self.pos] != '"':
            if self.source[self.pos] == "\\" and self.pos + 1 < len(self.source):
                self.pos += 2
                self.col += 2
                value += self.source[self.pos - 2 : self.pos]
            else:
                value += self.source[self.pos]
                if self.source[self.pos] == "\n":
                    self.line += 1
                    self.col = 1
                else:
                    self.col += 1
                self.pos += 1

        if self.pos < len(self.source):
            self.pos += 1  # Skip closing quote
            self.col += 1

        self.tokens.append(Token(TokenType.STR_LIT, value, self.line, start_col))

    def _read_char(self) -> None:
        """Read a character literal."""
        start_col = self.col
        self.pos += 1  # Skip opening quote
        self.col += 1

        value = ""
        if self.pos < len(self.source) and self.source[self.pos] == "\\":
            value = self.source[self.pos : self.pos + 2]
            self.pos += 2
            self.col += 2
        elif self.pos < len(self.source):
            value = self.source[self.pos]
            self.pos += 1
            self.col += 1

        if self.pos < len(self.source) and self.source[self.pos] == "'":
            self.pos += 1
            self.col += 1

        self.tokens.append(Token(TokenType.CHAR_LIT, value, self.line, start_col))

    def _read_operator(self) -> bool:
        """Read an operator or delimiter."""
        ch = self.source[self.pos]
        start_col = self.col

        # Two-character operators
        if self.pos + 1 < len(self.source):
            two_ch = ch + self.source[self.pos + 1]
            token_type = None

            if two_ch == "==":
                token_type = TokenType.EQ
            elif two_ch == "!=":
                token_type = TokenType.NE
            elif two_ch == "<=":
                token_type = TokenType.LE
            elif two_ch == ">=":
                token_type = TokenType.GE
            elif two_ch == "&&":
                token_type = TokenType.AND
            elif two_ch == "||":
                token_type = TokenType.OR

            if token_type:
                self.tokens.append(Token(token_type, two_ch, self.line, start_col))
                self.pos += 2
                self.col += 2
                return True

        # Single-character operators
        token_type = None
        if ch == "+":
            token_type = TokenType.PLUS
        elif ch == "-":
            token_type = TokenType.MINUS
        elif ch == "*":
            token_type = TokenType.STAR
        elif ch == "/":
            token_type = TokenType.SLASH
        elif ch == "%":
            token_type = TokenType.PERCENT
        elif ch == "=":
            token_type = TokenType.ASSIGN
        elif ch == "<":
            token_type = TokenType.LT
        elif ch == ">":
            token_type = TokenType.GT
        elif ch == "!":
            token_type = TokenType.NOT
        elif ch == "&":
            token_type = TokenType.AMPERSAND
        elif ch == "(":
            token_type = TokenType.LPAREN
        elif ch == ")":
            token_type = TokenType.RPAREN
        elif ch == "{":
            token_type = TokenType.LBRACE
        elif ch == "}":
            token_type = TokenType.RBRACE
        elif ch == "[":
            token_type = TokenType.LBRACKET
        elif ch == "]":
            token_type = TokenType.RBRACKET
        elif ch == ";":
            token_type = TokenType.SEMICOLON
        elif ch == ",":
            token_type = TokenType.COMMA

        if token_type:
            self.tokens.append(Token(token_type, ch, self.line, start_col))
            self.pos += 1
            self.col += 1
            return True

        return False
