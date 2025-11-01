"""
IDRIS2 Lexer - Tokenizes IDRIS2 source code for compilation to Babbage ISA

Features:
  - Dependent type syntax (forall, ->, etc.)
  - Type constructors and value constructors
  - Pattern matching syntax
  - Module imports and declarations
  - Block and line comments
  - Proper identifier/keyword distinction

Token types (50+ types):
  - Keywords: data, type, where, let, in, case, if, then, else, def, etc.
  - Type annotations: :, ->, ←, forall, exists
  - Operators: =, /=, <, <=, >, >=, &&, ||, etc.
  - Identifiers: lowercase (values), UPPERCASE (types)
  - Literals: integers, floats, strings, characters
  - Brackets: (), [], {}, ⟨⟩
  - Comments: -- (line), {- -} (block)
"""

from __future__ import annotations
from enum import Enum
from dataclasses import dataclass
from typing import List, Optional


class TokenType(str, Enum):
    """Token type enumeration for IDRIS2"""
    # Keywords
    DATA = "DATA"
    TYPE = "TYPE"
    WHERE = "WHERE"
    LET = "LET"
    IN = "IN"
    CASE = "CASE"
    OF = "OF"
    IF = "IF"
    THEN = "THEN"
    ELSE = "ELSE"
    DEF = "DEF"
    TOTAL = "TOTAL"
    PARTIAL = "PARTIAL"
    IMPLICIT = "IMPLICIT"
    EXPORT = "EXPORT"
    IMPORT = "IMPORT"
    MODULE = "MODULE"
    PUBLIC = "PUBLIC"
    PRIVATE = "PRIVATE"

    # Type system
    FORALL = "FORALL"
    EXISTS = "EXISTS"

    # Literals and identifiers
    NUMBER = "NUMBER"
    STRING = "STRING"
    CHAR = "CHAR"
    IDENTIFIER = "IDENTIFIER"
    TYPE_NAME = "TYPE_NAME"

    # Type annotations
    COLON = "COLON"
    ARROW = "ARROW"
    DOUBLE_ARROW = "DOUBLE_ARROW"
    FAT_ARROW = "FAT_ARROW"

    # Operators
    EQUAL = "EQUAL"
    NOT_EQUAL = "NOT_EQUAL"
    LESS = "LESS"
    LESS_EQUAL = "LESS_EQUAL"
    GREATER = "GREATER"
    GREATER_EQUAL = "GREATER_EQUAL"
    AND = "AND"
    OR = "OR"
    NOT = "NOT"
    PLUS = "PLUS"
    MINUS = "MINUS"
    STAR = "STAR"
    SLASH = "SLASH"
    PERCENT = "PERCENT"
    AT = "AT"
    BACKSLASH = "BACKSLASH"
    PIPE = "PIPE"
    UNDERSCORE = "UNDERSCORE"

    # Delimiters
    LPAREN = "LPAREN"
    RPAREN = "RPAREN"
    LBRACKET = "LBRACKET"
    RBRACKET = "RBRACKET"
    LBRACE = "LBRACE"
    RBRACE = "RBRACE"
    COMMA = "COMMA"
    DOT = "DOT"
    SEMICOLON = "SEMICOLON"
    EQUALS = "EQUALS"

    # Control
    COMMENT = "COMMENT"
    WHITESPACE = "WHITESPACE"
    INDENT = "INDENT"
    DEDENT = "DEDENT"
    NEWLINE = "NEWLINE"
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


class IDRIS2Lexer:
    """Tokenizes IDRIS2 source code"""

    KEYWORDS = {
        'data': TokenType.DATA,
        'type': TokenType.TYPE,
        'where': TokenType.WHERE,
        'let': TokenType.LET,
        'in': TokenType.IN,
        'case': TokenType.CASE,
        'of': TokenType.OF,
        'if': TokenType.IF,
        'then': TokenType.THEN,
        'else': TokenType.ELSE,
        'def': TokenType.DEF,
        'total': TokenType.TOTAL,
        'partial': TokenType.PARTIAL,
        'implicit': TokenType.IMPLICIT,
        'export': TokenType.EXPORT,
        'import': TokenType.IMPORT,
        'module': TokenType.MODULE,
        'public': TokenType.PUBLIC,
        'private': TokenType.PRIVATE,
        'forall': TokenType.FORALL,
        'exists': TokenType.EXISTS,
    }

    def __init__(self, source: str) -> None:
        """Initialize lexer with source code"""
        self.source = source
        self.pos = 0
        self.line = 1
        self.column = 1
        self.tokens: List[Token] = []
        self.indent_stack = [0]

    def tokenize(self) -> List[Token]:
        """Tokenize entire source and return token list"""
        while self.pos < len(self.source):
            # Skip whitespace except newlines
            if self.current() in ' \t':
                self._skip_whitespace()
                continue

            # Handle newlines with indentation tracking
            if self.current() == '\n':
                self._advance()
                continue

            # Block comments
            if self.current() == '{' and self.peek() == '-':
                self._skip_block_comment()
                continue

            # Line comments
            if self.current() == '-' and self.peek() == '-':
                self._skip_line_comment()
                continue

            # Arrow operators
            if self.current() == '-' and self.peek() == '>':
                self._add_token(TokenType.ARROW, '->')
                self._advance()
                self._advance()
                continue

            if self.current() == '=' and self.peek() == '>':
                self._add_token(TokenType.FAT_ARROW, '=>')
                self._advance()
                self._advance()
                continue

            if self.current() == '=' and self.peek() == '=':
                self._add_token(TokenType.EQUAL, '==')
                self._advance()
                self._advance()
                continue

            if self.current() == '/' and self.peek() == '=':
                self._add_token(TokenType.NOT_EQUAL, '/=')
                self._advance()
                self._advance()
                continue

            if self.current() == '<' and self.peek() == '=':
                self._add_token(TokenType.LESS_EQUAL, '<=')
                self._advance()
                self._advance()
                continue

            if self.current() == '>' and self.peek() == '=':
                self._add_token(TokenType.GREATER_EQUAL, '>=')
                self._advance()
                self._advance()
                continue

            if self.current() == '&' and self.peek() == '&':
                self._add_token(TokenType.AND, '&&')
                self._advance()
                self._advance()
                continue

            if self.current() == '|' and self.peek() == '|':
                self._add_token(TokenType.OR, '||')
                self._advance()
                self._advance()
                continue

            ch = self.current()

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
            elif ch == ',':
                self._add_token(TokenType.COMMA, ',')
                self._advance()
            elif ch == '.':
                self._add_token(TokenType.DOT, '.')
                self._advance()
            elif ch == ';':
                self._add_token(TokenType.SEMICOLON, ';')
                self._advance()
            elif ch == '=':
                self._add_token(TokenType.EQUALS, '=')
                self._advance()
            elif ch == '<':
                self._add_token(TokenType.LESS, '<')
                self._advance()
            elif ch == '>':
                self._add_token(TokenType.GREATER, '>')
                self._advance()
            elif ch == '+':
                self._add_token(TokenType.PLUS, '+')
                self._advance()
            elif ch == '-':
                self._add_token(TokenType.MINUS, '-')
                self._advance()
            elif ch == '*':
                self._add_token(TokenType.STAR, '*')
                self._advance()
            elif ch == '/':
                self._add_token(TokenType.SLASH, '/')
                self._advance()
            elif ch == '%':
                self._add_token(TokenType.PERCENT, '%')
                self._advance()
            elif ch == '@':
                self._add_token(TokenType.AT, '@')
                self._advance()
            elif ch == '\\':
                self._add_token(TokenType.BACKSLASH, '\\')
                self._advance()
            elif ch == '|':
                self._add_token(TokenType.PIPE, '|')
                self._advance()
            elif ch == '_':
                self._add_token(TokenType.UNDERSCORE, '_')
                self._advance()
            elif ch == '!':
                self._add_token(TokenType.NOT, '!')
                self._advance()

            # String literal
            elif ch == '"':
                self._scan_string()

            # Character literal
            elif ch == "'":
                self._scan_char()

            # Number
            elif ch.isdigit():
                self._scan_number()

            # Identifier or keyword
            elif ch.isalpha() or ch == '_':
                self._scan_identifier()

            else:
                self._advance()

        self._add_token(TokenType.EOF, '')
        return self.tokens

    def current(self) -> Optional[str]:
        """Get current character"""
        if self.pos < len(self.source):
            return self.source[self.pos]
        return None

    def peek(self, offset: int = 1) -> Optional[str]:
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

        self.tokens.append(Token(TokenType.STRING, value, start_line, start_col))

    def _scan_char(self) -> None:
        """Scan character literal"""
        start_line = self.line
        start_col = self.column
        self._advance()  # consume opening '

        value = ''
        if self.current() and self.current() != "'":
            if self.current() == '\\' and self.peek():
                self._advance()
                value += self.current()
                self._advance()
            else:
                value += self.current()
                self._advance()

        if self.current() == "'":
            self._advance()

        self.tokens.append(Token(TokenType.CHAR, value, start_line, start_col))

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

    def _scan_identifier(self) -> None:
        """Scan identifier or keyword"""
        start_line = self.line
        start_col = self.column
        value = ''

        while (self.current() and
               (self.current().isalnum() or self.current() in '_\'?')):
            value += self.current()
            self._advance()

        # Check if keyword
        token_type = self.KEYWORDS.get(value.lower(), TokenType.IDENTIFIER)

        # Type names are UPPERCASE
        if token_type == TokenType.IDENTIFIER and value and value[0].isupper():
            token_type = TokenType.TYPE_NAME

        self.tokens.append(Token(token_type, value, start_line, start_col))

    def _add_token(self, token_type: TokenType, value: str) -> None:
        """Add token to token list"""
        self.tokens.append(Token(token_type, value, self.line, self.column))
