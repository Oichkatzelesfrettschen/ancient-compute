"""
Haskell Lexer - Tokenizes Haskell source code

Handles:
- Keywords: let, in, where, if, then, else, case, of, do, etc.
- Operators: ->, =>, ::, |, <-, =, etc.
- Indentation-based blocks (INDENT/DEDENT tokens)
- Lambda expressions (\)
- Pattern matching
- Type annotations
- Comments (-- and {- -})
- List and tuple syntax
- String and number literals
"""

from __future__ import annotations
from enum import Enum, auto
from typing import List, Optional, Tuple
from dataclasses import dataclass


class TokenType(Enum):
    """Haskell token types"""

    # Keywords
    LET = auto()
    IN = auto()
    WHERE = auto()
    IF = auto()
    THEN = auto()
    ELSE = auto()
    CASE = auto()
    OF = auto()
    DO = auto()
    LAMBDA = auto()  # \
    MODULE = auto()
    IMPORT = auto()
    DATA = auto()
    TYPE = auto()
    CLASS = auto()
    INSTANCE = auto()
    DERIVING = auto()

    # Literals
    IDENTIFIER = auto()
    NUMBER = auto()
    STRING = auto()
    CHAR = auto()

    # Operators
    ARROW = auto()  # ->
    FAT_ARROW = auto()  # =>
    DOUBLE_COLON = auto()  # ::
    PIPE = auto()  # |
    BACKSLASH = auto()  # \
    BACKTICK = auto()  # `
    CONS = auto()  # :
    EQUALS = auto()  # =
    PLUS = auto()  # +
    MINUS = auto()  # -
    STAR = auto()  # *
    SLASH = auto()  # /
    PERCENT = auto()  # %
    POWER = auto()  # ^
    DOT = auto()  # .
    COMMA = auto()  # ,
    SEMICOLON = auto()  # ;
    LPAREN = auto()  # (
    RPAREN = auto()  # )
    LBRACKET = auto()  # [
    RBRACKET = auto()  # ]
    LBRACE = auto()  # {
    RBRACE = auto()  # }

    # Comparison
    EQUAL_EQUAL = auto()  # ==
    NOT_EQUAL = auto()  # /=
    LESS = auto()  # <
    LESS_EQUAL = auto()  # <=
    GREATER = auto()  # >
    GREATER_EQUAL = auto()  # >=

    # Special
    INDENT = auto()
    DEDENT = auto()
    NEWLINE = auto()
    EOF = auto()


@dataclass
class Token:
    """Haskell token"""

    type: TokenType
    value: str
    line: int
    column: int


class HaskellLexer:
    """Lexer for Haskell source code"""

    KEYWORDS = {
        "let": TokenType.LET,
        "in": TokenType.IN,
        "where": TokenType.WHERE,
        "if": TokenType.IF,
        "then": TokenType.THEN,
        "else": TokenType.ELSE,
        "case": TokenType.CASE,
        "of": TokenType.OF,
        "do": TokenType.DO,
        "module": TokenType.MODULE,
        "import": TokenType.IMPORT,
        "data": TokenType.DATA,
        "type": TokenType.TYPE,
        "class": TokenType.CLASS,
        "instance": TokenType.INSTANCE,
        "deriving": TokenType.DERIVING,
    }

    def __init__(self, source: str) -> None:
        """Initialize lexer with source code"""
        self.source = source
        self.pos = 0
        self.line = 1
        self.column = 1
        self.tokens: List[Token] = []
        self.indent_stack: List[int] = [0]

    def tokenize(self) -> List[Token]:
        """Tokenize source and return list of tokens"""
        while self.pos < len(self.source):
            # Track line starts for indentation
            line_start_pos = self.pos

            # Skip whitespace except newlines
            self._skip_horizontal_whitespace()

            # Handle comments
            if self._match("--"):
                self._skip_line_comment()
                continue

            if self._match("{-"):
                self._skip_block_comment()
                continue

            # Handle newlines
            if self.pos < len(self.source) and self.source[self.pos] == "\n":
                self.tokens.append(Token(TokenType.NEWLINE, "\n", self.line, self.column))
                self._advance()

                # Handle indentation on next line
                self._handle_indentation()
                continue

            if self.pos >= len(self.source):
                break

            char = self.source[self.pos]

            # String literals
            if char == '"':
                self._read_string()
                continue

            # Character literals
            if char == "'":
                self._read_char()
                continue

            # Numbers
            if char.isdigit():
                self._read_number()
                continue

            # Identifiers and keywords
            if char.isalpha() or char == "_":
                self._read_identifier()
                continue

            # Two-character operators
            if self.pos + 1 < len(self.source):
                two_char = self.source[self.pos : self.pos + 2]
                token_type = None

                if two_char == "->":
                    token_type = TokenType.ARROW
                elif two_char == "=>":
                    token_type = TokenType.FAT_ARROW
                elif two_char == "::":
                    token_type = TokenType.DOUBLE_COLON
                elif two_char == "==":
                    token_type = TokenType.EQUAL_EQUAL
                elif two_char == "/=":
                    token_type = TokenType.NOT_EQUAL
                elif two_char == "<=":
                    token_type = TokenType.LESS_EQUAL
                elif two_char == ">=":
                    token_type = TokenType.GREATER_EQUAL
                elif two_char == "^":
                    token_type = TokenType.POWER

                if token_type:
                    self.tokens.append(Token(token_type, two_char, self.line, self.column))
                    self.pos += 2
                    self.column += 2
                    continue

            # Single-character operators
            token_type = None
            if char == "|":
                token_type = TokenType.PIPE
            elif char == "\\":
                token_type = TokenType.BACKSLASH
            elif char == "`":
                token_type = TokenType.BACKTICK
            elif char == ":":
                token_type = TokenType.CONS
            elif char == "=":
                token_type = TokenType.EQUALS
            elif char == "+":
                token_type = TokenType.PLUS
            elif char == "-":
                token_type = TokenType.MINUS
            elif char == "*":
                token_type = TokenType.STAR
            elif char == "/":
                token_type = TokenType.SLASH
            elif char == "%":
                token_type = TokenType.PERCENT
            elif char == ".":
                token_type = TokenType.DOT
            elif char == ",":
                token_type = TokenType.COMMA
            elif char == ";":
                token_type = TokenType.SEMICOLON
            elif char == "(":
                token_type = TokenType.LPAREN
            elif char == ")":
                token_type = TokenType.RPAREN
            elif char == "[":
                token_type = TokenType.LBRACKET
            elif char == "]":
                token_type = TokenType.RBRACKET
            elif char == "{":
                token_type = TokenType.LBRACE
            elif char == "}":
                token_type = TokenType.RBRACE
            elif char == "<":
                token_type = TokenType.LESS
            elif char == ">":
                token_type = TokenType.GREATER

            if token_type:
                self.tokens.append(Token(token_type, char, self.line, self.column))
                self._advance()
                continue

            # Unknown character
            raise SyntaxError(f"Unexpected character '{char}' at {self.line}:{self.column}")

        # Emit remaining DEDENT tokens
        while len(self.indent_stack) > 1:
            self.indent_stack.pop()
            self.tokens.append(Token(TokenType.DEDENT, "", self.line, self.column))

        self.tokens.append(Token(TokenType.EOF, "", self.line, self.column))
        return self.tokens

    def _read_string(self) -> None:
        """Read string literal"""
        start_line = self.line
        start_col = self.column
        self._advance()  # Skip opening "

        value = ""
        while self.pos < len(self.source) and self.source[self.pos] != '"':
            if self.source[self.pos] == "\\" and self.pos + 1 < len(self.source):
                self._advance()
                escape_char = self.source[self.pos]
                if escape_char == "n":
                    value += "\n"
                elif escape_char == "t":
                    value += "\t"
                elif escape_char == "r":
                    value += "\r"
                elif escape_char == "\\":
                    value += "\\"
                elif escape_char == '"':
                    value += '"'
                else:
                    value += escape_char
                self._advance()
            else:
                value += self.source[self.pos]
                self._advance()

        if self.pos >= len(self.source):
            raise SyntaxError(f"Unterminated string at {start_line}:{start_col}")

        self._advance()  # Skip closing "
        self.tokens.append(Token(TokenType.STRING, value, start_line, start_col))

    def _read_char(self) -> None:
        """Read character literal"""
        start_line = self.line
        start_col = self.column
        self._advance()  # Skip opening '

        value = ""
        if self.pos < len(self.source):
            if self.source[self.pos] == "\\" and self.pos + 1 < len(self.source):
                self._advance()
                escape_char = self.source[self.pos]
                if escape_char == "n":
                    value = "\n"
                elif escape_char == "t":
                    value = "\t"
                else:
                    value = escape_char
                self._advance()
            else:
                value = self.source[self.pos]
                self._advance()

        if self.pos >= len(self.source) or self.source[self.pos] != "'":
            raise SyntaxError(f"Unterminated character literal at {start_line}:{start_col}")

        self._advance()  # Skip closing '
        self.tokens.append(Token(TokenType.CHAR, value, start_line, start_col))

    def _read_number(self) -> None:
        """Read number literal"""
        start_line = self.line
        start_col = self.column
        value = ""

        while self.pos < len(self.source) and self.source[self.pos].isdigit():
            value += self.source[self.pos]
            self._advance()

        # Handle floating point
        if self.pos < len(self.source) and self.source[self.pos] == ".":
            if self.pos + 1 < len(self.source) and self.source[self.pos + 1].isdigit():
                value += self.source[self.pos]
                self._advance()
                while self.pos < len(self.source) and self.source[self.pos].isdigit():
                    value += self.source[self.pos]
                    self._advance()

        self.tokens.append(Token(TokenType.NUMBER, value, start_line, start_col))

    def _read_identifier(self) -> None:
        """Read identifier or keyword"""
        start_line = self.line
        start_col = self.column
        value = ""

        while self.pos < len(self.source) and (
            self.source[self.pos].isalnum()
            or self.source[self.pos] == "_"
            or self.source[self.pos] == "'"
        ):
            value += self.source[self.pos]
            self._advance()

        # Check if it's a keyword
        token_type = self.KEYWORDS.get(value, TokenType.IDENTIFIER)
        self.tokens.append(Token(token_type, value, start_line, start_col))

    def _skip_horizontal_whitespace(self) -> None:
        """Skip spaces and tabs but not newlines"""
        while self.pos < len(self.source) and self.source[self.pos] in " \t":
            self._advance()

    def _skip_line_comment(self) -> None:
        """Skip line comment (-- to end of line)"""
        while self.pos < len(self.source) and self.source[self.pos] != "\n":
            self._advance()

    def _skip_block_comment(self) -> None:
        """Skip block comment ({- to -})"""
        self.pos += 2
        self.column += 2
        while self.pos + 1 < len(self.source):
            if self.source[self.pos : self.pos + 2] == "-}":
                self.pos += 2
                self.column += 2
                return
            if self.source[self.pos] == "\n":
                self.line += 1
                self.column = 1
            else:
                self.column += 1
            self.pos += 1

    def _handle_indentation(self) -> None:
        """Handle indentation tokens for block structure"""
        indent_level = 0
        while self.pos < len(self.source) and self.source[self.pos] in " \t":
            if self.source[self.pos] == " ":
                indent_level += 1
            else:
                indent_level += 8  # Tab = 8 spaces
            self._advance()

        # Skip blank lines and comments
        if self.pos < len(self.source) and self.source[self.pos] not in "\n":
            if self._peek_chars(2) == "--":
                return
            if self._peek_chars(2) == "{-":
                return

            current_indent = self.indent_stack[-1]

            if indent_level > current_indent:
                self.indent_stack.append(indent_level)
                self.tokens.append(Token(TokenType.INDENT, "", self.line, self.column))
            elif indent_level < current_indent:
                while len(self.indent_stack) > 1 and self.indent_stack[-1] > indent_level:
                    self.indent_stack.pop()
                    self.tokens.append(Token(TokenType.DEDENT, "", self.line, self.column))

    def _advance(self) -> None:
        """Move to next character"""
        if self.pos < len(self.source):
            if self.source[self.pos] == "\n":
                self.line += 1
                self.column = 1
            else:
                self.column += 1
            self.pos += 1

    def _match(self, text: str) -> bool:
        """Check if current position matches text"""
        if self.pos + len(text) > len(self.source):
            return False
        return self.source[self.pos : self.pos + len(text)] == text

    def _peek_chars(self, count: int) -> str:
        """Peek ahead at multiple characters"""
        return self.source[self.pos : self.pos + count]
