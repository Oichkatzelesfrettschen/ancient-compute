"""
Java Lexer - Tokenizes Java source code

Supports:
  - Keywords (class, interface, void, int, etc.)
  - Operators (arithmetic, comparison, logical, bitwise, assignment)
  - Literals (numbers, strings, characters, boolean)
  - Comments (// and /* */)
  - Identifiers and type names
  - Delimiters (brackets, braces, parentheses)
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum, auto


class TokenType(Enum):
    """Java token types"""
    # Literals
    NUMBER = auto()
    STRING_LIT = auto()
    CHAR_LIT = auto()
    BOOLEAN = auto()
    NULL = auto()

    # Keywords
    CLASS = auto()
    INTERFACE = auto()
    ENUM = auto()
    EXTENDS = auto()
    IMPLEMENTS = auto()
    PUBLIC = auto()
    PRIVATE = auto()
    PROTECTED = auto()
    STATIC = auto()
    FINAL = auto()
    ABSTRACT = auto()
    NATIVE = auto()
    SYNCHRONIZED = auto()
    VOLATILE = auto()
    TRANSIENT = auto()

    # Types
    VOID = auto()
    INT = auto()
    LONG = auto()
    FLOAT = auto()
    DOUBLE = auto()
    BOOLEAN_TYPE = auto()
    BYTE = auto()
    SHORT = auto()
    CHAR_TYPE = auto()
    STRING = auto()
    VAR = auto()

    # Control flow
    IF = auto()
    ELSE = auto()
    SWITCH = auto()
    CASE = auto()
    DEFAULT = auto()
    WHILE = auto()
    FOR = auto()
    DO = auto()
    BREAK = auto()
    CONTINUE = auto()
    RETURN = auto()
    THROW = auto()
    THROWS = auto()
    TRY = auto()
    CATCH = auto()
    FINALLY = auto()

    # Other keywords
    NEW = auto()
    THIS = auto()
    SUPER = auto()
    INSTANCEOF = auto()
    IMPORT = auto()
    PACKAGE = auto()

    # Operators
    PLUS = auto()
    MINUS = auto()
    STAR = auto()
    SLASH = auto()
    PERCENT = auto()
    POWER = auto()

    EQUAL = auto()
    EQUAL_EQUAL = auto()
    NOT_EQUAL = auto()
    LESS = auto()
    LESS_EQUAL = auto()
    GREATER = auto()
    GREATER_EQUAL = auto()

    AND = auto()  # &&
    OR = auto()   # ||
    NOT = auto()  # !

    BIT_AND = auto()  # &
    BIT_OR = auto()   # |
    BIT_XOR = auto()  # ^
    BIT_NOT = auto()  # ~
    LSHIFT = auto()   # <<
    RSHIFT = auto()   # >>
    URSHIFT = auto()  # >>>

    PLUS_EQUAL = auto()
    MINUS_EQUAL = auto()
    STAR_EQUAL = auto()
    SLASH_EQUAL = auto()
    PERCENT_EQUAL = auto()

    INCREMENT = auto()  # ++
    DECREMENT = auto()  # --

    # Delimiters
    LPAREN = auto()
    RPAREN = auto()
    LBRACE = auto()
    RBRACE = auto()
    LBRACKET = auto()
    RBRACKET = auto()
    SEMICOLON = auto()
    COMMA = auto()
    DOT = auto()
    COLON = auto()
    DOUBLE_COLON = auto()  # ::
    QUESTION = auto()
    ARROW = auto()  # ->
    ELLIPSIS = auto()  # ...
    AT = auto()  # @

    # Special
    IDENTIFIER = auto()
    TYPE_NAME = auto()
    NEWLINE = auto()
    EOF = auto()


@dataclass
class Token:
    """Represents a single token"""
    type: TokenType
    value: str
    line: int
    column: int


class JavaLexer:
    """Lexer for Java source code"""

    KEYWORDS = {
        'class': TokenType.CLASS,
        'interface': TokenType.INTERFACE,
        'enum': TokenType.ENUM,
        'extends': TokenType.EXTENDS,
        'implements': TokenType.IMPLEMENTS,
        'public': TokenType.PUBLIC,
        'private': TokenType.PRIVATE,
        'protected': TokenType.PROTECTED,
        'static': TokenType.STATIC,
        'final': TokenType.FINAL,
        'abstract': TokenType.ABSTRACT,
        'native': TokenType.NATIVE,
        'synchronized': TokenType.SYNCHRONIZED,
        'volatile': TokenType.VOLATILE,
        'transient': TokenType.TRANSIENT,
        'void': TokenType.VOID,
        'int': TokenType.INT,
        'long': TokenType.LONG,
        'float': TokenType.FLOAT,
        'double': TokenType.DOUBLE,
        'boolean': TokenType.BOOLEAN_TYPE,
        'byte': TokenType.BYTE,
        'short': TokenType.SHORT,
        'char': TokenType.CHAR_TYPE,
        'String': TokenType.STRING,
        'var': TokenType.VAR,
        'if': TokenType.IF,
        'else': TokenType.ELSE,
        'switch': TokenType.SWITCH,
        'case': TokenType.CASE,
        'default': TokenType.DEFAULT,
        'while': TokenType.WHILE,
        'for': TokenType.FOR,
        'do': TokenType.DO,
        'break': TokenType.BREAK,
        'continue': TokenType.CONTINUE,
        'return': TokenType.RETURN,
        'throw': TokenType.THROW,
        'throws': TokenType.THROWS,
        'try': TokenType.TRY,
        'catch': TokenType.CATCH,
        'finally': TokenType.FINALLY,
        'new': TokenType.NEW,
        'this': TokenType.THIS,
        'super': TokenType.SUPER,
        'instanceof': TokenType.INSTANCEOF,
        'import': TokenType.IMPORT,
        'package': TokenType.PACKAGE,
        'true': TokenType.BOOLEAN,
        'false': TokenType.BOOLEAN,
        'null': TokenType.NULL,
    }

    def __init__(self, source: str) -> None:
        """Initialize lexer with source code"""
        self.source = source
        self.pos = 0
        self.line = 1
        self.column = 1
        self.tokens: list[Token] = []

    def tokenize(self) -> list[Token]:
        """Tokenize entire source"""
        while self.pos < len(self.source):
            self._skip_whitespace_and_comments()

            if self.pos >= len(self.source):
                break

            ch = self.source[self.pos]

            # Numbers
            if ch.isdigit():
                self._read_number()
            # Strings
            elif ch == '"':
                self._read_string()
            # Characters
            elif ch == "'":
                self._read_char()
            # Identifiers and keywords
            elif ch.isalpha() or ch == '_':
                self._read_identifier()
            # Operators and delimiters
            else:
                self._read_operator_or_delimiter()

        self.tokens.append(Token(TokenType.EOF, '', self.line, self.column))
        return self.tokens

    def _current_char(self) -> str | None:
        """Get current character without advancing"""
        if self.pos < len(self.source):
            return self.source[self.pos]
        return None

    def _peek(self, offset: int = 1) -> str | None:
        """Peek ahead"""
        pos = self.pos + offset
        if pos < len(self.source):
            return self.source[pos]
        return None

    def _advance(self) -> str:
        """Advance position and return character"""
        ch = self.source[self.pos]
        self.pos += 1
        if ch == '\n':
            self.line += 1
            self.column = 1
        else:
            self.column += 1
        return ch

    def _skip_whitespace_and_comments(self) -> None:
        """Skip whitespace and comments"""
        while self.pos < len(self.source):
            ch = self._current_char()

            # Whitespace
            if ch in ' 	\n\r':
                self._advance()
            # Line comment
            elif ch == '/' and self._peek() == '/':
                self._advance()  # /
                self._advance()  # /
                while self.pos < len(self.source) and self._current_char() != '\n':
                    self._advance()
            # Block comment
            elif ch == '/' and self._peek() == '*':
                self._advance()  # /
                self._advance()  # *
                while self.pos < len(self.source):
                    if self._current_char() == '*' and self._peek() == '/':
                        self._advance()  # *
                        self._advance()  # /
                        break
                    self._advance()
            else:
                break

    def _read_number(self) -> None:
        """Read numeric literal"""
        start_line = self.line
        start_col = self.column
        num_str = ''

        while self.pos < len(self.source) and (self._current_char().isdigit() or self._current_char() in '.fFdDlL'):
            num_str += self._advance()

        self.tokens.append(Token(TokenType.NUMBER, num_str, start_line, start_col))

    def _read_string(self) -> None:
        """Read string literal"""
        start_line = self.line
        start_col = self.column
        self._advance()  # Opening "
        string_val = ''

        while self.pos < len(self.source) and self._current_char() != '"':
            if self._current_char() == '\\':
                self._advance()
                if self.pos < len(self.source):
                    string_val += self._advance()
            else:
                string_val += self._advance()

        if self.pos < len(self.source):
            self._advance()  # Closing "

        self.tokens.append(Token(TokenType.STRING_LIT, string_val, start_line, start_col))

    def _read_char(self) -> None:
        """Read character literal"""
        start_line = self.line
        start_col = self.column
        self._advance()  # Opening '
        char_val = ''

        if self._current_char() == '\\':
            self._advance()
            if self.pos < len(self.source):
                char_val += self._advance()
        else:
            if self.pos < len(self.source):
                char_val += self._advance()

        if self.pos < len(self.source) and self._current_char() == "'":
            self._advance()  # Closing '

        self.tokens.append(Token(TokenType.CHAR_LIT, char_val, start_line, start_col))

    def _read_identifier(self) -> None:
        """Read identifier or keyword"""
        start_line = self.line
        start_col = self.column
        ident = ''

        while self.pos < len(self.source) and (self._current_char().isalnum() or self._current_char() in '_$'):
            ident += self._advance()

        # Check if keyword
        token_type = self.KEYWORDS.get(ident, TokenType.IDENTIFIER)

        # Type names are capitalized identifiers
        if token_type == TokenType.IDENTIFIER and ident[0].isupper():
            token_type = TokenType.TYPE_NAME

        self.tokens.append(Token(token_type, ident, start_line, start_col))

    def _read_operator_or_delimiter(self) -> None:
        """Read operator or delimiter"""
        start_line = self.line
        start_col = self.column
        ch = self._current_char()

        self._advance()
        next_ch = self._current_char()

        # Three-character operators
        if ch == '.' and next_ch == '.' and self._peek() == '.':
            self._advance() # 2nd dot
            self._advance() # 3rd dot
            self.tokens.append(Token(TokenType.ELLIPSIS, '...', start_line, start_col))
            return

        # Two-character operators
        two_char = ch + (next_ch or '')

        if two_char == '==':
            self._advance()
            self.tokens.append(Token(TokenType.EQUAL_EQUAL, '==', start_line, start_col))
        elif two_char == '!=':
            self._advance()
            self.tokens.append(Token(TokenType.NOT_EQUAL, '!=', start_line, start_col))
        elif two_char == '<=':
            self._advance()
            self.tokens.append(Token(TokenType.LESS_EQUAL, '<=', start_line, start_col))
        elif two_char == '>=':
            self._advance()
            self.tokens.append(Token(TokenType.GREATER_EQUAL, '>=', start_line, start_col))
        elif two_char == '&&':
            self._advance()
            self.tokens.append(Token(TokenType.AND, '&&', start_line, start_col))
        elif two_char == '||':
            self._advance()
            self.tokens.append(Token(TokenType.OR, '||', start_line, start_col))
        elif two_char == '++':
            self._advance()
            self.tokens.append(Token(TokenType.INCREMENT, '++', start_line, start_col))
        elif two_char == '--':
            self._advance()
            self.tokens.append(Token(TokenType.DECREMENT, '--', start_line, start_col))
        elif two_char == '<<':
            self._advance()
            self.tokens.append(Token(TokenType.LSHIFT, '<<', start_line, start_col))
        elif two_char == '>>':
            self._advance()
            if self._current_char() == '>':
                self._advance()
                self.tokens.append(Token(TokenType.URSHIFT, '>>>', start_line, start_col))
            else:
                self.tokens.append(Token(TokenType.RSHIFT, '>>', start_line, start_col))
        elif two_char == '+=':
            self._advance()
            self.tokens.append(Token(TokenType.PLUS_EQUAL, '+=', start_line, start_col))
        elif two_char == '-=':
            self._advance()
            self.tokens.append(Token(TokenType.MINUS_EQUAL, '-=', start_line, start_col))
        elif two_char == '*=':
            self._advance()
            self.tokens.append(Token(TokenType.STAR_EQUAL, '*=', start_line, start_col))
        elif two_char == '/=':
            self._advance()
            self.tokens.append(Token(TokenType.SLASH_EQUAL, '/=', start_line, start_col))
        elif two_char == '%=':
            self._advance()
            self.tokens.append(Token(TokenType.PERCENT_EQUAL, '%=', start_line, start_col))
        elif two_char == '->':
            self._advance()
            self.tokens.append(Token(TokenType.ARROW, '->', start_line, start_col))
        elif two_char == '::':
            self._advance()
            self.tokens.append(Token(TokenType.DOUBLE_COLON, '::', start_line, start_col))
        # Single-character operators
        elif ch == '+':
            self.tokens.append(Token(TokenType.PLUS, '+', start_line, start_col))
        elif ch == '-':
            self.tokens.append(Token(TokenType.MINUS, '-', start_line, start_col))
        elif ch == '*':
            self.tokens.append(Token(TokenType.STAR, '*', start_line, start_col))
        elif ch == '/':
            self.tokens.append(Token(TokenType.SLASH, '/', start_line, start_col))
        elif ch == '%':
            self.tokens.append(Token(TokenType.PERCENT, '%', start_line, start_col))
        elif ch == '!':
            self.tokens.append(Token(TokenType.NOT, '!', start_line, start_col))
        elif ch == '<':
            self.tokens.append(Token(TokenType.LESS, '<', start_line, start_col))
        elif ch == '>':
            self.tokens.append(Token(TokenType.GREATER, '>', start_line, start_col))
        elif ch == '=':
            self.tokens.append(Token(TokenType.EQUAL, '=', start_line, start_col))
        elif ch == '&':
            self.tokens.append(Token(TokenType.BIT_AND, '&', start_line, start_col))
        elif ch == '|':
            self.tokens.append(Token(TokenType.BIT_OR, '|', start_line, start_col))
        elif ch == '^':
            self.tokens.append(Token(TokenType.BIT_XOR, '^', start_line, start_col))
        elif ch == '~':
            self.tokens.append(Token(TokenType.BIT_NOT, '~', start_line, start_col))
        elif ch == '(':
            self.tokens.append(Token(TokenType.LPAREN, '(', start_line, start_col))
        elif ch == ')':
            self.tokens.append(Token(TokenType.RPAREN, ')', start_line, start_col))
        elif ch == '{':
            self.tokens.append(Token(TokenType.LBRACE, '{', start_line, start_col))
        elif ch == '}':
            self.tokens.append(Token(TokenType.RBRACE, '}', start_line, start_col))
        elif ch == '[':
            self.tokens.append(Token(TokenType.LBRACKET, '[', start_line, start_col))
        elif ch == ']':
            self.tokens.append(Token(TokenType.RBRACKET, ']', start_line, start_col))
        elif ch == ';':
            self.tokens.append(Token(TokenType.SEMICOLON, ';', start_line, start_col))
        elif ch == ',':
            self.tokens.append(Token(TokenType.COMMA, ',', start_line, start_col))
        elif ch == '.':
            self.tokens.append(Token(TokenType.DOT, '.', start_line, start_col))
        elif ch == ':':
            self.tokens.append(Token(TokenType.COLON, ':', start_line, start_col))
        elif ch == '?':
            self.tokens.append(Token(TokenType.QUESTION, '?', start_line, start_col))
        elif ch == '@':
            self.tokens.append(Token(TokenType.AT, '@', start_line, start_col))
