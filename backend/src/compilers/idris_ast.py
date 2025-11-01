"""
IDRIS2 Abstract Syntax Tree - Type-aware expression and type nodes

Supports dependent types: types can be indexed by values
Example: Vect : Nat -> Type -> Type (vector of length n containing elements of type a)

Type hierarchy:
  - Base types: Nat, Int, String, etc.
  - Function types: (a -> b)
  - Dependent types: (n : Nat) -> Vect n Int -> Int
  - Implicit parameters: {a : Type}
  - Type families: Vect n a
  - Refinement types: (x : Nat { x > 0 })
"""

from __future__ import annotations
from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import List, Tuple, Optional, Union


class Type(ABC):
    """Base class for types"""

    @abstractmethod
    def __repr__(self) -> str:
        pass


@dataclass
class BaseType(Type):
    """Base type (Nat, Int, String, etc.)"""
    name: str

    def __repr__(self) -> str:
        return f"BaseType({self.name!r})"


@dataclass
class TypeVariable(Type):
    """Type variable (a, b, etc.)"""
    name: str

    def __repr__(self) -> str:
        return f"TypeVariable({self.name!r})"


@dataclass
class FunctionType(Type):
    """Function type (a -> b)"""
    arg_type: Type
    return_type: Type

    def __repr__(self) -> str:
        return f"FunctionType({self.arg_type!r}, {self.return_type!r})"


@dataclass
class DependentType(Type):
    """Dependent function type ((x : a) -> b)"""
    param_name: str
    param_type: Type
    return_type: Type
    is_implicit: bool = False

    def __repr__(self) -> str:
        return f"DependentType({self.param_name!r}, {self.param_type!r}, {self.return_type!r})"


@dataclass
class TypeFamily(Type):
    """Type family (e.g., Vect n a)"""
    family_name: str
    type_args: List[Type]

    def __repr__(self) -> str:
        return f"TypeFamily({self.family_name!r}, {self.type_args!r})"


@dataclass
class RefinementType(Type):
    """Refinement type (x : Nat { P x })"""
    base_type: Type
    constraint: Expr

    def __repr__(self) -> str:
        return f"RefinementType({self.base_type!r}, {self.constraint!r})"


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
    value: Union[int, float, str]

    def __repr__(self) -> str:
        return f"Literal({self.value!r})"


@dataclass
class Lambda(Expr):
    """Lambda abstraction (\\x => expr)"""
    parameters: List[Tuple[str, Type]]  # (name, type) pairs
    body: Expr

    def __repr__(self) -> str:
        return f"Lambda({self.parameters!r}, {self.body!r})"


@dataclass
class Application(Expr):
    """Function application (f x y)"""
    func: Expr
    args: List[Expr]

    def __repr__(self) -> str:
        return f"Application({self.func!r}, {self.args!r})"


@dataclass
class LetExpr(Expr):
    """Let expression (let x = e1 in e2)"""
    bindings: List[Tuple[str, Type, Expr]]  # (name, type, expr)
    body: Expr

    def __repr__(self) -> str:
        return f"LetExpr({self.bindings!r}, {self.body!r})"


@dataclass
class CaseExpr(Expr):
    """Case expression (case x of patterns)"""
    scrutinee: Expr
    branches: List[Tuple[Pattern, Expr]]  # (pattern, result)

    def __repr__(self) -> str:
        return f"CaseExpr({self.scrutinee!r}, {self.branches!r})"


@dataclass
class IfExpr(Expr):
    """If expression (if cond then e1 else e2)"""
    condition: Expr
    then_expr: Expr
    else_expr: Expr

    def __repr__(self) -> str:
        return f"IfExpr({self.condition!r}, {self.then_expr!r}, {self.else_expr!r})"


@dataclass
class ProofExpr(Expr):
    """Proof of a type (term-level proof)"""
    type_: Type
    proof: Expr

    def __repr__(self) -> str:
        return f"ProofExpr({self.type_!r}, {self.proof!r})"


@dataclass
class DataConstructor(Expr):
    """Data constructor application"""
    constructor_name: str
    args: List[Expr]

    def __repr__(self) -> str:
        return f"DataConstructor({self.constructor_name!r}, {self.args!r})"


class Pattern(ABC):
    """Base class for patterns"""

    @abstractmethod
    def __repr__(self) -> str:
        pass


@dataclass
class VarPattern(Pattern):
    """Variable pattern"""
    name: str

    def __repr__(self) -> str:
        return f"VarPattern({self.name!r})"


@dataclass
class LiteralPattern(Pattern):
    """Literal pattern"""
    value: Union[int, float, str]

    def __repr__(self) -> str:
        return f"LiteralPattern({self.value!r})"


@dataclass
class ConstructorPattern(Pattern):
    """Constructor pattern"""
    constructor_name: str
    arg_patterns: List[Pattern]

    def __repr__(self) -> str:
        return f"ConstructorPattern({self.constructor_name!r}, {self.arg_patterns!r})"


@dataclass
class WildcardPattern(Pattern):
    """Wildcard pattern (_)"""

    def __repr__(self) -> str:
        return "WildcardPattern(_)"


class Declaration(ABC):
    """Base class for top-level declarations"""

    @abstractmethod
    def __repr__(self) -> str:
        pass


@dataclass
class TypeDeclaration(Declaration):
    """Type signature (x : Type)"""
    name: str
    type_: Type

    def __repr__(self) -> str:
        return f"TypeDeclaration({self.name!r}, {self.type_!r})"


@dataclass
class FunctionDef(Declaration):
    """Function definition"""
    name: str
    type_annotation: Type
    parameters: List[str]
    body: Expr
    is_total: bool = True
    is_public: bool = False

    def __repr__(self) -> str:
        return f"FunctionDef({self.name!r}, {self.type_annotation!r}, {self.parameters!r}, ...)"


@dataclass
class DataDef(Declaration):
    """Data type definition"""
    name: str
    type_parameters: List[Tuple[str, Type]]  # (name, type)
    constructors: List[Tuple[str, Type]]  # (constructor_name, constructor_type)
    is_public: bool = False

    def __repr__(self) -> str:
        return f"DataDef({self.name!r}, {self.type_parameters!r}, ...)"


@dataclass
class TypeDef(Declaration):
    """Type synonym definition"""
    name: str
    type_params: List[str]
    body: Type
    is_public: bool = False

    def __repr__(self) -> str:
        return f"TypeDef({self.name!r}, {self.type_params!r}, {self.body!r})"


@dataclass
class Module:
    """Module (collection of declarations)"""
    name: str
    imports: List[str]
    declarations: List[Declaration]

    def __repr__(self) -> str:
        return f"Module({self.name!r}, {len(self.declarations)} declarations)"
