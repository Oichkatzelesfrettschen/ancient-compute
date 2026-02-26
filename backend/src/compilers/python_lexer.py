"""
Python Lexer - Tokenizes Python source code for compilation to Babbage ISA

Features:
  - Indentation-based syntax handling (INDENT/DEDENT tokens)
  - Full Python keyword support
  - String/number literal parsing
  - Line continuation with backslash
  - Comment handling
  - Error reporting with line/column information

Token types:
  - Keywords: def, return, if, elif, else, while, for, in, pass, break, continue
  - Identifiers: variable names, function names
  - Numbers: integers and floats
  - Strings: single/double quoted with escape sequences
  - Operators: +, -, *, /, //, %, ==, !=, <, <=, >, >=, and, or, not, =
  - Delimiters: (, ), :, ,, [, ], {, }
  - Whitespace/Indentation: INDENT/DEDENT tokens for scope
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum


class TokenType(str, Enum):
    """Token type enumeration"""
    # Keywords
    KEYWORD = "KEYWORD"
    DEF = "DEF"
    RETURN = "RETURN"
    IF = "IF"
    ELIF = "ELIF"
    ELSE = "ELSE"
    WHILE = "WHILE"
    FOR = "FOR"
    IN = "IN"
    PASS = "PASS"
    BREAK = "BREAK"
    CONTINUE = "CONTINUE"
    AND = "AND"
    OR = "OR"
    NOT = "NOT"
    TRUE = "TRUE"
    FALSE = "FALSE"
    NONE = "NONE"

    # Literals
    IDENTIFIER = "IDENTIFIER"
    NUMBER = "NUMBER"
    STRING = "STRING"

    # Operators
    PLUS = "PLUS"
    MINUS = "MINUS"
    STAR = "STAR"
    SLASH = "SLASH"
    DOUBLE_SLASH = "DOUBLE_SLASH"
    PERCENT = "PERCENT"
    POWER = "POWER"
    EQUAL = "EQUAL"
    EQUAL_EQUAL = "EQUAL_EQUAL"
    NOT_EQUAL = "NOT_EQUAL"
    LESS = "LESS"
    LESS_EQUAL = "LESS_EQUAL"
    GREATER = "GREATER"
    GREATER_EQUAL = "GREATER_EQUAL"

    # Delimiters
    LPAREN = "LPAREN"
    RPAREN = "RPAREN"
    LBRACKET = "LBRACKET"
    RBRACKET = "RBRACKET"
    LBRACE = "LBRACE"
    RBRACE = "RBRACE"
    COLON = "COLON"
    COMMA = "COMMA"
    DOT = "DOT"

    # Indentation
    INDENT = "INDENT"
    DEDENT = "DEDENT"
    NEWLINE = "NEWLINE"

    # Special
    EOF = "EOF"


@dataclass
class Token:
    """Token with type, value, line, column"""
    type: TokenType
    value: str
    line: int
    column: int

    def __repr__(self) -> str:
        return f"Token({self.type}, {self.value!r}, {self.line}:{self.column})"


class PythonLexer:
    """Lexer for Python source code"""

    KEYWORDS = {
        'def': TokenType.DEF,
        'return': TokenType.RETURN,
        'if': TokenType.IF,
        'elif': TokenType.ELIF,
        'else': TokenType.ELSE,
        'while': TokenType.WHILE,
        'for': TokenType.FOR,
        'in': TokenType.IN,
        'pass': TokenType.PASS,
        'break': TokenType.BREAK,
        'continue': TokenType.CONTINUE,
        'and': TokenType.AND,
        'or': TokenType.OR,
        'not': TokenType.NOT,
        'True': TokenType.TRUE,
        'False': TokenType.FALSE,
        'None': TokenType.NONE,
    }

    def __init__(self, source: str) -> None:
        """Initialize lexer with source code"""
        self.source = source
        self.pos = 0
        self.line = 1
        self.column = 1
        self.tokens: list[Token] = []
        self.indent_stack: list[int] = [0]  # Track indentation levels
        self.pending_dedents = 0  # DEDENT tokens to emit

    def tokenize(self) -> list[Token]:
        """Tokenize entire source and return token list"""
        while self.pos < len(self.source):
            self._skip_whitespace_except_indent()

            if self.pos >= len(self.source):
                break

            # Handle newlines
            if self._current_char() == '\n':
                self.tokens.append(Token(TokenType.NEWLINE, '\n', self.line, self.column))
                self._advance()
                self._handle_indentation()
                continue

            # Handle comments
            if self._current_char() == '#':
                self._skip_comment()
                continue

            # Handle strings
            if self._current_char() in ('"', "'"):
                self.tokens.append(self._read_string())
                continue

            # Handle numbers
            if self._current_char().isdigit():
                self.tokens.append(self._read_number())
                continue

            # Handle identifiers and keywords
            if self._current_char().isalpha() or self._current_char() == '_':
                self.tokens.append(self._read_identifier())
                continue

            # Handle operators and delimiters
            token = self._read_operator_or_delimiter()
            if token:
                self.tokens.append(token)
                continue

            # Unknown character
            raise SyntaxError(f"Unexpected character '{self._current_char()}' at {self.line}:{self.column}")

        # Emit remaining DEDENT tokens
        while len(self.indent_stack) > 1:
            self.indent_stack.pop()
            self.tokens.append(Token(TokenType.DEDENT, '', self.line, self.column))

        self.tokens.append(Token(TokenType.EOF, '', self.line, self.column))
        return self.tokens

    def _current_char(self) -> str:
        """Get current character without advancing"""
        if self.pos >= len(self.source):
            return ''
        return self.source[self.pos]

    def _peek_char(self, offset: int = 1) -> str:
        """Peek ahead at character"""
        pos = self.pos + offset
        if pos >= len(self.source):
            return ''
        return self.source[pos]

    def _advance(self) -> str:
        """Move to next character"""
        if self.pos >= len(self.source):
            return ''

        char = self.source[self.pos]
        self.pos += 1

        if char == '\n':
            self.line += 1
            self.column = 1
        else:
            self.column += 1

        return char

    def _skip_whitespace_except_indent(self) -> None:
        """Skip spaces and tabs, but not newlines"""
        while self.pos < len(self.source) and self._current_char() in (' ', '\t'):
            self._advance()

    def _skip_comment(self) -> None:
        """Skip comment to end of line"""
        while self.pos < len(self.source) and self._current_char() != '\n':
            self._advance()

    def _handle_indentation(self) -> None:
        """Handle indentation changes at line start"""
        indent_level = 0
        while self.pos < len(self.source) and self._current_char() in (' ', '\t'):
            if self._current_char() == ' ':
                indent_level += 1
            else:  # tab
                indent_level += 8

            self._advance()

        # Skip blank lines and comments
        if self.pos >= len(self.source) or self._current_char() in ('\n', '#'):
            return

        current_indent = self.indent_stack[-1]

        if indent_level > current_indent:
            self.indent_stack.append(indent_level)
            self.tokens.append(Token(TokenType.INDENT, '', self.line, self.column))
        elif indent_level < current_indent:
            while len(self.indent_stack) > 1 and self.indent_stack[-1] > indent_level:
                self.indent_stack.pop()
                self.tokens.append(Token(TokenType.DEDENT, '', self.line, self.column))

            if self.indent_stack[-1] != indent_level:
                raise SyntaxError(f"Indentation error at {self.line}:{self.column}")

    def _read_string(self) -> Token:
        """Read string literal"""
        start_line = self.line
        start_col = self.column
        quote_char = self._current_char()
        self._advance()

        value = ''
        while self.pos < len(self.source) and self._current_char() != quote_char:
            if self._current_char() == '\\':
                self._advance()
                if self.pos < len(self.source):
                    escape_char = self._current_char()
                    if escape_char == 'n':
                        value += '\n'
                    elif escape_char == 't':
                        value += '\t'
                    elif escape_char == 'r':
                        value += '\r'
                    elif escape_char == '\\':
                        value += '\\'
                    elif escape_char == quote_char:
                        value += quote_char
                    else:
                        value += escape_char
                    self._advance()
            else:
                value += self._current_char()
                self._advance()

        if self.pos >= len(self.source):
            raise SyntaxError(f"Unterminated string at {start_line}:{start_col}")

        self._advance()  # closing quote
        return Token(TokenType.STRING, value, start_line, start_col)

    def _read_number(self) -> Token:
        """Read numeric literal"""
        start_line = self.line
        start_col = self.column
        value = ''

        # Integer part
        while self.pos < len(self.source) and self._current_char().isdigit():
            value += self._current_char()
            self._advance()

        # Decimal part
        if self.pos < len(self.source) and self._current_char() == '.' and \
           self._peek_char().isdigit():
            value += self._current_char()
            self._advance()

            while self.pos < len(self.source) and self._current_char().isdigit():
                value += self._current_char()
                self._advance()

        return Token(TokenType.NUMBER, value, start_line, start_col)

    def _read_identifier(self) -> Token:
        """Read identifier or keyword"""
        start_line = self.line
        start_col = self.column
        value = ''

        while self.pos < len(self.source) and \
              (self._current_char().isalnum() or self._current_char() == '_'):
            value += self._current_char()
            self._advance()

        token_type = self.KEYWORDS.get(value, TokenType.IDENTIFIER)
        return Token(token_type, value, start_line, start_col)

    def _read_operator_or_delimiter(self) -> Token | None:
        """Read operator or delimiter"""
        start_line = self.line
        start_col = self.column
        char = self._current_char()

        # Two-character operators
        if char == '=' and self._peek_char() == '=':
            self._advance()
            self._advance()
            return Token(TokenType.EQUAL_EQUAL, '==', start_line, start_col)

        if char == '!' and self._peek_char() == '=':
            self._advance()
            self._advance()
            return Token(TokenType.NOT_EQUAL, '!=', start_line, start_col)

        if char == '<' and self._peek_char() == '=':
            self._advance()
            self._advance()
            return Token(TokenType.LESS_EQUAL, '<=', start_line, start_col)

        if char == '>' and self._peek_char() == '=':
            self._advance()
            self._advance()
            return Token(TokenType.GREATER_EQUAL, '>=', start_line, start_col)

        if char == '/' and self._peek_char() == '/':
            self._advance()
            self._advance()
            return Token(TokenType.DOUBLE_SLASH, '//', start_line, start_col)

        if char == '*' and self._peek_char() == '*':
            self._advance()
            self._advance()
            return Token(TokenType.POWER, '**', start_line, start_col)

        # Single-character operators and delimiters
        operators = {
            '+': TokenType.PLUS,
            '-': TokenType.MINUS,
            '*': TokenType.STAR,
            '/': TokenType.SLASH,
            '%': TokenType.PERCENT,
            '=': TokenType.EQUAL,
            '<': TokenType.LESS,
            '>': TokenType.GREATER,
            '(': TokenType.LPAREN,
            ')': TokenType.RPAREN,
            '[': TokenType.LBRACKET,
            ']': TokenType.RBRACKET,
            '{': TokenType.LBRACE,
            '}': TokenType.RBRACE,
            ':': TokenType.COLON,
            ',': TokenType.COMMA,
            '.': TokenType.DOT,
        }

        if char in operators:
            self._advance()
            return Token(operators[char], char, start_line, start_col)

        return None
