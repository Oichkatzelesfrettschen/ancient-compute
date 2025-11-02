"""
Haskell Abstract Syntax Tree (AST) node definitions

Represents the structure of parsed Haskell code with support for:
- Expressions: literals, variables, binary/unary ops, lambda, case, let
- Patterns: variables, literals, constructors, guards
- Statements: function definitions, type declarations
- Types: simple types, function types, polymorphic types
"""

from __future_ import annotations
from dataclasses import dataclass
from typing import List, Optional, Any


# ============================================================================
# Expressions
# ============================================================================


@dataclass
class Expr:
    """Base class for expressions"""

    pass


@dataclass
class Literal(Expr):
    """Literal value (number, string, char)"""

    value: Any
    type_name: str  # 'int', 'float', 'string', 'char'


@dataclass
class Variable(Expr):
    """Variable reference"""

    name: str


@dataclass
class BinOp(Expr):
    """Binary operation: left op right"""

    left: Expr
    op: str
    right: Expr


@dataclass
class UnaryOp(Expr):
    """Unary operation: op operand"""

    op: str
    operand: Expr


@dataclass
class Lambda(Expr):
    r"""Lambda expression: \param -> body"""

    params: List[str]
    body: Expr


@dataclass
class Application(Expr):
    """Function application: func arg"""

    func: Expr
    args: List[Expr]


@dataclass
class Let(Expr):
    """Let expression: let x = e1 in e2"""

    bindings: List[tuple]  # List of (name, expr) tuples
    body: Expr


@dataclass
class Case(Expr):
    """Case expression: case expr of pattern -> expr ..."""

    expr: Expr
    branches: List[CaseBranch]


@dataclass
class CaseBranch:
    """Single branch in case expression"""

    pattern: Pattern
    guard: Optional[Expr]  # Optional guard condition
    body: Expr


@dataclass
class IfThenElse(Expr):
    """If-then-else expression"""

    condition: Expr
    then_expr: Expr
    else_expr: Expr


@dataclass
class List(Expr):
    """List literal: [e1, e2, ...] or [e1 .. en]"""

    elements: List[Expr]
    is_range: bool = False  # True if [start .. end]


@dataclass
class Tuple(Expr):
    """Tuple literal: (e1, e2, ...)"""

    elements: List[Expr]


@dataclass
class TypeAnnotation(Expr):
    """Type annotation: expr :: type"""

    expr: Expr
    type_sig: str  # Type signature as string


@dataclass
class Constructor(Expr):
    """Constructor application: Name args"""

    name: str
    args: List[Expr]


# ============================================================================
# Patterns
# ============================================================================


@dataclass
class Pattern:
    """Base class for patterns"""

    pass


@dataclass
class PatternLiteral(Pattern):
    """Literal pattern: 42, "string", 'c'"""

    value: Any


@dataclass
class PatternVariable(Pattern):
    """Variable pattern: x"""

    name: str


@dataclass
class PatternConstructor(Pattern):
    """Constructor pattern: Name p1 p2"""

    name: str
    patterns: List[Pattern]


@dataclass
class PatternTuple(Pattern):
    """Tuple pattern: (p1, p2, ...)"""

    patterns: List[Pattern]


@dataclass
class PatternList(Pattern):
    """List pattern: [p1, p2, ...] or [h|t]"""

    patterns: List[Pattern]
    tail: Optional[Pattern] = None  # For [h|t] syntax


@dataclass
class PatternWildcard(Pattern):
    """Wildcard pattern: _"""

    pass


# ============================================================================
# Statements
# ============================================================================


@dataclass
class Stmt:
    """Base class for statements"""

    pass


@dataclass
class FunctionDef(Stmt):
    """Function definition with pattern matching"""

    name: str
    equations: List[FunctionEquation]  # Multiple equations for pattern matching


@dataclass
class FunctionEquation:
    """Single equation in function definition"""

    patterns: List[Pattern]  # Argument patterns
    guard: Optional[Expr]  # Optional guard
    body: Expr


@dataclass
class TypeDecl(Stmt):
    """Type declaration: name :: type"""

    name: str
    type_sig: str


@dataclass
class DataDecl(Stmt):
    """Data type declaration"""

    name: str
    constructors: List[DataConstructor]


@dataclass
class DataConstructor:
    """Single constructor in data declaration"""

    name: str
    fields: List[str]  # Field types


@dataclass
class TypeSynonym(Stmt):
    """Type synonym: type Name = Type"""

    name: str
    definition: str


@dataclass
class ClassDecl(Stmt):
    """Type class declaration"""

    name: str
    methods: List[tuple]  # (name, type_sig) tuples


@dataclass
class InstanceDecl(Stmt):
    """Instance declaration"""

    class_name: str
    type_name: str
    definitions: List[tuple]  # (name, body) tuples


# ============================================================================
# Top-level
# ============================================================================


@dataclass
class Module:
    """Top-level module"""

    name: Optional[str]  # Module name (if declared)
    declarations: List[Stmt]  # Top-level declarations and definitions

