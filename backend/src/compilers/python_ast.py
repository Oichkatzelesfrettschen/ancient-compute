"""
Python Abstract Syntax Tree (AST) Definitions

Provides AST node classes for representing Python syntax structures:
  - Expressions: BinOp, UnaryOp, Call, Name, Constant
  - Statements: Assign, Return, If, While, For, FunctionDef
  - Program structure: Module
"""

from __future__ import annotations
from dataclasses import dataclass
from typing import List, Optional, Any


@dataclass
class Expr:
    """Base class for expressions"""
    pass


@dataclass
class BinOp(Expr):
    """Binary operation: left op right"""
    left: Expr
    op: str  # '+', '-', '*', '/', '//', '%', '**', '==', '!=', '<', '<=', '>', '>=', 'and', 'or'
    right: Expr


@dataclass
class UnaryOp(Expr):
    """Unary operation: op operand"""
    op: str  # 'not', '-', '+'
    operand: Expr


@dataclass
class Call(Expr):
    """Function call: func(args)"""
    func: str  # function name
    args: List[Expr]


@dataclass
class Name(Expr):
    """Variable reference"""
    id: str


@dataclass
class Constant(Expr):
    """Constant value"""
    value: Any  # int, float, str, bool, None


@dataclass
class Subscript(Expr):
    """Array subscript: value[index]"""
    value: Expr
    index: Expr


@dataclass
class Attribute(Expr):
    """Attribute access: value.attr"""
    value: Expr
    attr: str


@dataclass
class Stmt:
    """Base class for statements"""
    pass


@dataclass
class Assign(Stmt):
    """Assignment: target = value"""
    target: str  # variable name
    value: Expr


@dataclass
class Return(Stmt):
    """Return statement"""
    value: Optional[Expr]


@dataclass
class If(Stmt):
    """If/elif/else statement"""
    test: Expr
    body: List[Stmt]
    orelse: List[Stmt]  # elif/else body


@dataclass
class While(Stmt):
    """While loop"""
    test: Expr
    body: List[Stmt]


@dataclass
class For(Stmt):
    """For loop: for target in iter: body"""
    target: str
    iter: Expr
    body: List[Stmt]


@dataclass
class FunctionDef(Stmt):
    """Function definition"""
    name: str
    args: List[str]
    body: List[Stmt]


@dataclass
class Pass(Stmt):
    """Pass statement (no-op)"""
    pass


@dataclass
class Break(Stmt):
    """Break statement"""
    pass


@dataclass
class Continue(Stmt):
    """Continue statement"""
    pass


@dataclass
class ExprStmt(Stmt):
    """Expression statement (for side effects)"""
    value: Expr


@dataclass
class Module:
    """Top-level module"""
    body: List[Stmt]
