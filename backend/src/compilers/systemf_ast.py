"""
System F Abstract Syntax Tree - Type-aware expression and type nodes

System F (polymorphic lambda calculus):
  - Expressions can be abstracted over values (lambda)
  - Expressions can be abstracted over types (type lambda)
  - Types can be variables, functions, or quantified universally

Example:
  id = /\\a => \\x : a => x      -- polymorphic identity function
  Type: forall a. a -> a

Grammar:
  type ::= TypeVar | BaseType | FunctionType | UniversalType
  expr ::= Var | Literal | Lambda | TypeAbstraction | Application | TypeApplication | ...
"""

from __future__ import annotations
from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import List, Tuple, Optional


class Type(ABC):
    """Base class for types"""

    @abstractmethod
    def __repr__(self) -> str:
        pass


@dataclass
class TypeVar(Type):
    """Type variable (a, b, t, u, etc.)"""
    name: str

    def __repr__(self) -> str:
        return f"TypeVar({self.name!r})"


@dataclass
class BaseType(Type):
    """Base type (Nat, Int, Bool, String)"""
    name: str

    def __repr__(self) -> str:
        return f"BaseType({self.name!r})"


@dataclass
class FunctionType(Type):
    """Function type (a -> b)"""
    arg_type: Type
    return_type: Type

    def __repr__(self) -> str:
        return f"FunctionType({self.arg_type!r} -> {self.return_type!r})"


@dataclass
class UniversalType(Type):
    """Universal type (forall a. T)"""
    type_var: str
    body: Type

    def __repr__(self) -> str:
        return f"UniversalType(forall {self.type_var}. {self.body!r})"


class Expr(ABC):
    """Base class for expressions"""

    @abstractmethod
    def __repr__(self) -> str:
        pass


@dataclass
class Var(Expr):
    """Variable reference"""
    name: str

    def __repr__(self) -> str:
        return f"Var({self.name!r})"


@dataclass
class Literal(Expr):
    """Literal value (number, string)"""
    value: object

    def __repr__(self) -> str:
        return f"Literal({self.value!r})"


@dataclass
class Lambda(Expr):
    """Lambda abstraction (\\x : T => expr)"""
    param_name: str
    param_type: Type
    body: Expr

    def __repr__(self) -> str:
        return f"Lambda({self.param_name!r} : {self.param_type!r} => ...)"


@dataclass
class TypeAbstraction(Expr):
    """Type abstraction (/\\a => expr)"""
    type_var: str
    body: Expr

    def __repr__(self) -> str:
        return f"TypeAbstraction(/\\{self.type_var!r} => ...)"


@dataclass
class Application(Expr):
    """Function application (f x)"""
    func: Expr
    arg: Expr

    def __repr__(self) -> str:
        return f"Application({self.func!r} {self.arg!r})"


@dataclass
class TypeApplication(Expr):
    """Type application (expr [T])"""
    expr: Expr
    type_arg: Type

    def __repr__(self) -> str:
        return f"TypeApplication({self.expr!r} [{self.type_arg!r}])"


@dataclass
class IfExpr(Expr):
    """If expression (if cond then e1 else e2)"""
    condition: Expr
    then_expr: Expr
    else_expr: Expr

    def __repr__(self) -> str:
        return f"IfExpr(if ... then {self.then_expr!r} else {self.else_expr!r})"


@dataclass
class LetExpr(Expr):
    """Let expression (let x : T = e1 in e2)"""
    var_name: str
    var_type: Type
    value_expr: Expr
    body_expr: Expr

    def __repr__(self) -> str:
        return f"LetExpr(let {self.var_name!r} = ... in ...)"


@dataclass
class FixExpr(Expr):
    """Fixed point combinator (fix f)"""
    func: Expr

    def __repr__(self) -> str:
        return f"FixExpr(fix {self.func!r})"


@dataclass
class Annotation(Expr):
    """Type annotation ((e : T))"""
    expr: Expr
    type_annotation: Type

    def __repr__(self) -> str:
        return f"Annotation({self.expr!r} : {self.type_annotation!r})"
