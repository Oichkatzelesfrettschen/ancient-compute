"""
LISP Abstract Syntax Tree (AST) - Expression and statement node definitions

Represents LISP programs as a tree of expressions:
  - Atoms: numbers, strings, symbols
  - Lists: function calls, special forms
  - Lambdas, let bindings, conditionals
  - Quote/quasiquote/unquote
"""

from __future__ import annotations
from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import List, Tuple, Optional, Union


class Expr(ABC):
    """Base class for all LISP expressions"""

    @abstractmethod
    def __repr__(self) -> str:
        pass


@dataclass
class Atom(Expr):
    """Atomic literal value (number, string)"""
    value: Union[int, float, str]

    def __repr__(self) -> str:
        return f"Atom({self.value!r})"


@dataclass
class Symbol(Expr):
    """Symbol (variable or function name)"""
    name: str

    def __repr__(self) -> str:
        return f"Symbol({self.name!r})"


@dataclass
class List(Expr):
    """List of expressions (function call, special form, or literal list)"""
    elements: List[Expr]

    def __repr__(self) -> str:
        return f"List({self.elements!r})"


@dataclass
class Quote(Expr):
    """Quoted expression (prevents evaluation)"""
    expr: Expr

    def __repr__(self) -> str:
        return f"Quote({self.expr!r})"


@dataclass
class Quasiquote(Expr):
    """Quasiquoted expression (allows selective unquoting)"""
    expr: Expr

    def __repr__(self) -> str:
        return f"Quasiquote({self.expr!r})"


@dataclass
class Unquote(Expr):
    """Unquoted expression within quasiquote"""
    expr: Expr

    def __repr__(self) -> str:
        return f"Unquote({self.expr!r})"


@dataclass
class UnquoteSplicing(Expr):
    """Unquote-splice within quasiquote (flattens list)"""
    expr: Expr

    def __repr__(self) -> str:
        return f"UnquoteSplicing({self.expr!r})"


@dataclass
class Lambda(Expr):
    """Lambda expression (anonymous function)"""
    parameters: List[str]
    body: Expr

    def __repr__(self) -> str:
        return f"Lambda({self.parameters!r}, {self.body!r})"


@dataclass
class LetBinding(Expr):
    """Let expression with variable bindings"""
    bindings: List[Tuple[str, Expr]]  # [(name, value), ...]
    body: Expr

    def __repr__(self) -> str:
        return f"LetBinding({self.bindings!r}, {self.body!r})"


@dataclass
class IfExpr(Expr):
    """If conditional expression"""
    condition: Expr
    then_expr: Expr
    else_expr: Optional[Expr] = None

    def __repr__(self) -> str:
        return f"IfExpr({self.condition!r}, {self.then_expr!r}, {self.else_expr!r})"


@dataclass
class CondExpr(Expr):
    """Cond (multiple conditions) expression"""
    branches: List[Tuple[Expr, Expr]]  # [(condition, result), ...]

    def __repr__(self) -> str:
        return f"CondExpr({self.branches!r})"


@dataclass
class CaseExpr(Expr):
    """Case expression for pattern matching"""
    scrutinee: Expr
    branches: List[Tuple[Expr, Expr]]  # [(pattern, result), ...]

    def __repr__(self) -> str:
        return f"CaseExpr({self.scrutinee!r}, {self.branches!r})"


@dataclass
class DefunExpr(Expr):
    """Function definition (defun)"""
    name: str
    parameters: List[str]
    body: Expr

    def __repr__(self) -> str:
        return f"DefunExpr({self.name!r}, {self.parameters!r}, {self.body!r})"


@dataclass
class BuiltinFunctionCall(Expr):
    """Built-in function call"""
    name: str
    args: List[Expr]

    def __repr__(self) -> str:
        return f"BuiltinFunctionCall({self.name!r}, {self.args!r})"


@dataclass
class FunctionCall(Expr):
    """User-defined function call"""
    func_expr: Expr
    args: List[Expr]

    def __repr__(self) -> str:
        return f"FunctionCall({self.func_expr!r}, {self.args!r})"


# Built-in function names (for quick lookup)
BUILTIN_FUNCTIONS = {
    # Arithmetic
    '+', '-', '*', '/', '%', 'abs', 'sqrt', 'floor', 'ceil', 'round',
    # Comparison
    '=', '/=', '<', '<=', '>', '>=',
    # Logic
    'and', 'or', 'not',
    # List operations
    'cons', 'car', 'cdr', 'list', 'append', 'length', 'reverse',
    'nth', 'member', 'last', 'init',
    # I/O
    'print', 'read', 'display',
    # Type checking
    'numberp', 'stringp', 'symbolp', 'listp', 'null',
    # List predicates
    'atom', 'null',
    # Other
    'eval', 'quote',
}
