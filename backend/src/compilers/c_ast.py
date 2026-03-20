"""C AST node definitions for the C freestanding subset.

Defines the tree of dataclasses that the C parser builds from tokens.
For backward compatibility, TokenType, Token, CLexer, and CParser are
re-exported from their respective submodules.
"""

from __future__ import annotations

from dataclasses import dataclass, field

# Lexer re-exports for backward compatibility
from .c_lexer import CLexer, Token, TokenType  # noqa: F401

# AST Node definitions


@dataclass
class Type:
    """Base type representation."""

    name: str
    is_pointer: bool = False
    array_size: int | None = None


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
    args: list[Expression]


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

    variables: list[Variable]


@dataclass
class Block(Statement):
    """Block of statements."""

    statements: list[Statement]


@dataclass
class IfStatement(Statement):
    """If statement."""

    condition: Expression
    then_stmt: Statement
    else_stmt: Statement | None = None


@dataclass
class WhileStatement(Statement):
    """While loop statement."""

    condition: Expression
    body: Statement


@dataclass
class ForStatement(Statement):
    """For loop statement."""

    init: Expression | Statement | None
    condition: Expression | None
    increment: Expression | None
    body: Statement


@dataclass
class ReturnStatement(Statement):
    """Return statement."""

    value: Expression | None = None


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
    parameters: list[FunctionParameter]
    body: Block


@dataclass
class GlobalDeclaration:
    """Global variable declaration."""

    variables: list[Variable]


@dataclass
class Program:
    """Complete C program (top-level declarations)."""

    declarations: list[GlobalDeclaration | Function] = field(default_factory=list)
