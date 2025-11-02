"""C Abstract Syntax Tree (AST) definitions and parser.

This module provides the AST representation for C programs and a
recursive descent parser for a subset of C (suitable for Babbage ISA target).

Supported C features:
  - Global variable declarations
  - Function declarations with parameters
  - Local variable declarations
  - Arithmetic expressions
  - Control flow (if, while, for)
  - Function calls
  - Assignment statements

Unsupported (for Babbage compilation):
  - Structures/unions
  - Pointers (limited to function pointers)
  - Arrays (limited support)
  - Preprocessing directives
  - Complex type declarations
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import List, Optional, Union, Dict, Any
from enum import Enum
import re


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
        self.tokens: List[Token] = []

    def tokenize(self) -> List[Token]:
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


# AST Node definitions


@dataclass
class Type:
    """Base type representation."""

    name: str
    is_pointer: bool = False
    array_size: Optional[int] = None


@dataclass
class Variable:
    """Variable declaration with type."""

    name: str
    type: Type
    is_global: bool = False


@dataclass
class Expression:
    """Base expression node."""

    pass


@dataclass
class IntLiteral(Expression):
    """Integer literal expression."""

    value: int


@dataclass
class FloatLiteral(Expression):
    """Float literal expression."""

    value: float


@dataclass
class StringLiteral(Expression):
    """String literal expression."""

    value: str


@dataclass
class VariableRef(Expression):
    """Variable reference expression."""

    name: str


@dataclass
class BinaryOp(Expression):
    """Binary operation expression."""

    op: str  # '+', '-', '*', '/', '%', '==', '!=', '<', '<=', '>', '>=', '&&', '||'
    left: Expression
    right: Expression


@dataclass
class UnaryOp(Expression):
    """Unary operation expression."""

    op: str  # '-', '!', '&', '*'
    operand: Expression


@dataclass
class FunctionCall(Expression):
    """Function call expression."""

    name: str
    args: List[Expression]


@dataclass
class ArrayAccess(Expression):
    """Array access expression."""

    name: str
    index: Expression


@dataclass
class Assignment(Expression):
    """Assignment expression."""

    target: str
    value: Expression


@dataclass
class Statement:
    """Base statement node."""

    pass


@dataclass
class ExpressionStatement(Statement):
    """Expression statement."""

    expr: Expression


@dataclass
class VariableDeclaration(Statement):
    """Variable declaration statement."""

    variables: List[Variable]


@dataclass
class Block(Statement):
    """Block of statements."""

    statements: List[Statement]


@dataclass
class IfStatement(Statement):
    """If statement."""

    condition: Expression
    then_stmt: Statement
    else_stmt: Optional[Statement] = None


@dataclass
class WhileStatement(Statement):
    """While loop statement."""

    condition: Expression
    body: Statement


@dataclass
class ForStatement(Statement):
    """For loop statement."""

    init: Optional[Expression]
    condition: Optional[Expression]
    increment: Optional[Expression]
    body: Statement


@dataclass
class ReturnStatement(Statement):
    """Return statement."""

    value: Optional[Expression] = None


@dataclass
class FunctionParameter:
    """Function parameter declaration."""

    name: str
    type: Type


@dataclass
class Function:
    """Function declaration."""

    name: str
    return_type: Type
    parameters: List[FunctionParameter]
    body: Block


@dataclass
class GlobalDeclaration:
    """Global variable declaration."""

    variables: List[Variable]


@dataclass
class Program:
    """Complete C program (top-level declarations)."""

    declarations: List[Union[GlobalDeclaration, Function]] = field(default_factory=list)


class CParser:
    """Recursive descent parser for C."""

    def __init__(self, tokens: List[Token]) -> None:
        """Initialize parser with token list."""
        self.tokens = tokens
        self.pos = 0

    def parse(self) -> Program:
        """Parse a C program."""
        program = Program()

        while not self._is_at_end():
            if (
                self._check(TokenType.INT)
                or self._check(TokenType.FLOAT)
                or self._check(TokenType.VOID)
            ):
                # Could be function or global variable
                decl = self._parse_declaration()
                if decl:
                    program.declarations.append(decl)
            else:
                self._advance()

        return program

    def _parse_declaration(self) -> Optional[Union[GlobalDeclaration, Function]]:
        """Parse a declaration (function or global variable)."""
        ret_type = self._parse_type()
        if not ret_type:
            return None

        name = self._consume(TokenType.IDENT, "Expected identifier").value

        # Check if function or variable
        if self._check(TokenType.LPAREN):
            return self._parse_function(ret_type, name)
        else:
            return self._parse_global_variables(ret_type, name)

    def _parse_function(self, ret_type: Type, name: str) -> Function:
        """Parse a function declaration."""
        self._consume(TokenType.LPAREN, "Expected '('")

        params: List[FunctionParameter] = []
        if not self._check(TokenType.RPAREN):
            while True:
                param_type = self._parse_type()
                param_name = self._consume(TokenType.IDENT, "Expected parameter name").value
                params.append(FunctionParameter(param_name, param_type))

                if not self._check(TokenType.COMMA):
                    break
                self._advance()  # consume comma

        self._consume(TokenType.RPAREN, "Expected ')'")
        self._consume(TokenType.LBRACE, "Expected '{'")

        body = self._parse_block()

        return Function(name, ret_type, params, body)

    def _parse_global_variables(self, var_type: Type, first_name: str) -> GlobalDeclaration:
        """Parse global variable declarations."""
        variables: List[Variable] = []

        variables.append(Variable(first_name, var_type, is_global=True))

        while self._check(TokenType.COMMA):
            self._advance()  # consume comma
            name = self._consume(TokenType.IDENT, "Expected variable name").value
            variables.append(Variable(name, var_type, is_global=True))

        self._consume(TokenType.SEMICOLON, "Expected ';'")

        return GlobalDeclaration(variables)

    def _parse_block(self) -> Block:
        """Parse a block of statements."""
        statements: List[Statement] = []

        while not self._check(TokenType.RBRACE) and not self._is_at_end():
            stmt = self._parse_statement()
            if stmt:
                statements.append(stmt)

        self._consume(TokenType.RBRACE, "Expected '}'")

        return Block(statements)

    def _parse_statement(self) -> Optional[Statement]:
        """Parse a single statement."""
        if self._check(TokenType.LBRACE):
            self._advance()
            return self._parse_block()

        if self._check(TokenType.IF):
            return self._parse_if()

        if self._check(TokenType.WHILE):
            return self._parse_while()

        if self._check(TokenType.FOR):
            return self._parse_for()

        if self._check(TokenType.RETURN):
            return self._parse_return()

        if (
            self._check(TokenType.INT)
            or self._check(TokenType.FLOAT)
            or self._check(TokenType.VOID)
        ):
            return self._parse_variable_declaration()

        # Expression statement
        expr = self._parse_expression()
        self._consume(TokenType.SEMICOLON, "Expected ';'")
        return ExpressionStatement(expr)

    def _parse_if(self) -> IfStatement:
        """Parse an if statement."""
        self._consume(TokenType.IF, "Expected 'if'")
        self._consume(TokenType.LPAREN, "Expected '('")
        condition = self._parse_expression()
        self._consume(TokenType.RPAREN, "Expected ')'")

        then_stmt = self._parse_statement()
        else_stmt = None

        if self._check(TokenType.ELSE):
            self._advance()
            else_stmt = self._parse_statement()

        return IfStatement(condition, then_stmt, else_stmt)

    def _parse_while(self) -> WhileStatement:
        """Parse a while loop."""
        self._consume(TokenType.WHILE, "Expected 'while'")
        self._consume(TokenType.LPAREN, "Expected '('")
        condition = self._parse_expression()
        self._consume(TokenType.RPAREN, "Expected ')'")

        body = self._parse_statement()

        return WhileStatement(condition, body)

    def _parse_for(self) -> ForStatement:
        """Parse a for loop."""
        self._consume(TokenType.FOR, "Expected 'for'")
        self._consume(TokenType.LPAREN, "Expected '('")

        init = None
        if not self._check(TokenType.SEMICOLON):
            init = self._parse_expression()
        self._consume(TokenType.SEMICOLON, "Expected ';'")

        condition = None
        if not self._check(TokenType.SEMICOLON):
            condition = self._parse_expression()
        self._consume(TokenType.SEMICOLON, "Expected ';'")

        increment = None
        if not self._check(TokenType.RPAREN):
            increment = self._parse_expression()
        self._consume(TokenType.RPAREN, "Expected ')'")

        body = self._parse_statement()

        return ForStatement(init, condition, increment, body)

    def _parse_return(self) -> ReturnStatement:
        """Parse a return statement."""
        self._consume(TokenType.RETURN, "Expected 'return'")

        value = None
        if not self._check(TokenType.SEMICOLON):
            value = self._parse_expression()

        self._consume(TokenType.SEMICOLON, "Expected ';'")

        return ReturnStatement(value)

    def _parse_variable_declaration(self) -> VariableDeclaration:
        """Parse a variable declaration."""
        var_type = self._parse_type()

        variables: List[Variable] = []
        name = self._consume(TokenType.IDENT, "Expected variable name").value
        variables.append(Variable(name, var_type, is_global=False))

        while self._check(TokenType.COMMA):
            self._advance()
            name = self._consume(TokenType.IDENT, "Expected variable name").value
            variables.append(Variable(name, var_type, is_global=False))

        self._consume(TokenType.SEMICOLON, "Expected ';'")

        return VariableDeclaration(variables)

    def _parse_expression(self) -> Expression:
        """Parse an expression."""
        return self._parse_assignment()

    def _parse_assignment(self) -> Expression:
        """Parse assignment expression."""
        expr = self._parse_logical_or()

        if self._check(TokenType.ASSIGN):
            self._advance()
            if isinstance(expr, VariableRef):
                value = self._parse_assignment()
                return Assignment(expr.name, value)

        return expr

    def _parse_logical_or(self) -> Expression:
        """Parse logical OR expression."""
        expr = self._parse_logical_and()

        while self._check(TokenType.OR):
            op = self._advance().value
            right = self._parse_logical_and()
            expr = BinaryOp(op, expr, right)

        return expr

    def _parse_logical_and(self) -> Expression:
        """Parse logical AND expression."""
        expr = self._parse_equality()

        while self._check(TokenType.AND):
            op = self._advance().value
            right = self._parse_equality()
            expr = BinaryOp(op, expr, right)

        return expr

    def _parse_equality(self) -> Expression:
        """Parse equality expression."""
        expr = self._parse_comparison()

        while self._check(TokenType.EQ) or self._check(TokenType.NE):
            op = self._advance().value
            right = self._parse_comparison()
            expr = BinaryOp(op, expr, right)

        return expr

    def _parse_comparison(self) -> Expression:
        """Parse comparison expression."""
        expr = self._parse_additive()

        while (
            self._check(TokenType.LT)
            or self._check(TokenType.LE)
            or self._check(TokenType.GT)
            or self._check(TokenType.GE)
        ):
            op = self._advance().value
            right = self._parse_additive()
            expr = BinaryOp(op, expr, right)

        return expr

    def _parse_additive(self) -> Expression:
        """Parse additive expression."""
        expr = self._parse_multiplicative()

        while self._check(TokenType.PLUS) or self._check(TokenType.MINUS):
            op = self._advance().value
            right = self._parse_multiplicative()
            expr = BinaryOp(op, expr, right)

        return expr

    def _parse_multiplicative(self) -> Expression:
        """Parse multiplicative expression."""
        expr = self._parse_unary()

        while (
            self._check(TokenType.STAR)
            or self._check(TokenType.SLASH)
            or self._check(TokenType.PERCENT)
        ):
            op = self._advance().value
            right = self._parse_unary()
            expr = BinaryOp(op, expr, right)

        return expr

    def _parse_unary(self) -> Expression:
        """Parse unary expression."""
        if (
            self._check(TokenType.MINUS)
            or self._check(TokenType.NOT)
            or self._check(TokenType.AMPERSAND)
        ):
            op = self._advance().value
            expr = self._parse_unary()
            return UnaryOp(op, expr)

        return self._parse_postfix()

    def _parse_postfix(self) -> Expression:
        """Parse postfix expression."""
        expr = self._parse_primary()

        while True:
            if self._check(TokenType.LPAREN):
                # Function call
                self._advance()
                args: List[Expression] = []
                if not self._check(TokenType.RPAREN):
                    while True:
                        args.append(self._parse_expression())
                        if not self._check(TokenType.COMMA):
                            break
                        self._advance()
                self._consume(TokenType.RPAREN, "Expected ')'")

                if isinstance(expr, VariableRef):
                    expr = FunctionCall(expr.name, args)

            elif self._check(TokenType.LBRACKET):
                # Array access
                self._advance()
                index = self._parse_expression()
                self._consume(TokenType.RBRACKET, "Expected ']'")

                if isinstance(expr, VariableRef):
                    expr = ArrayAccess(expr.name, index)
            else:
                break

        return expr

    def _parse_primary(self) -> Expression:
        """Parse primary expression."""
        if self._check(TokenType.INT_LIT):
            value = int(self._advance().value)
            return IntLiteral(value)

        if self._check(TokenType.FLOAT_LIT):
            value = float(self._advance().value)
            return FloatLiteral(value)

        if self._check(TokenType.STR_LIT):
            value = self._advance().value
            return StringLiteral(value)

        if self._check(TokenType.IDENT):
            name = self._advance().value
            return VariableRef(name)

        if self._check(TokenType.LPAREN):
            self._advance()
            expr = self._parse_expression()
            self._consume(TokenType.RPAREN, "Expected ')'")
            return expr

        raise Exception(f"Unexpected token: {self._peek()}")

    def _parse_type(self) -> Optional[Type]:
        """Parse a type."""
        if self._check(TokenType.INT):
            self._advance()
            return Type("int")
        elif self._check(TokenType.FLOAT):
            self._advance()
            return Type("float")
        elif self._check(TokenType.VOID):
            self._advance()
            return Type("void")

        return None

    def _check(self, token_type: TokenType) -> bool:
        """Check if current token matches type."""
        if self._is_at_end():
            return False
        return self._peek().type == token_type

    def _advance(self) -> Token:
        """Consume and return current token."""
        if not self._is_at_end():
            self.pos += 1
        return self._previous()

    def _is_at_end(self) -> bool:
        """Check if at end of tokens."""
        return self._peek().type == TokenType.EOF

    def _peek(self) -> Token:
        """Return current token without consuming."""
        return self.tokens[self.pos]

    def _previous(self) -> Token:
        """Return previous token."""
        return self.tokens[self.pos - 1]

    def _consume(self, token_type: TokenType, message: str) -> Token:
        """Consume a token or raise error."""
        if self._check(token_type):
            return self._advance()

        current = self._peek()
        raise Exception(f"{message} at line {current.line}, col {current.col}")
