"""
LISP Type System - Type representation and inference

LISP is dynamically typed at the language level, but we infer types
for compilation to Babbage IR which requires static types.

Type hierarchy:
  - Any (untyped - default for unknown)
  - Primitive: Int, Float, String, Bool
  - Composite: List, Function
  - Special: Void (function return with no value)

Babbage mapping (all types compile to registers):
  - Int, Float → i64 or f64 registers
  - String → pointer to string table
  - Bool → i64 (0 or 1)
  - List → pointer to heap (simplified)
  - Function → function reference/closure
"""

from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass


class LISPType(ABC):
    """Base class for LISP types"""

    @abstractmethod
    def __repr__(self) -> str:
        pass

    @abstractmethod
    def __eq__(self, other: object) -> bool:
        pass


@dataclass
class AnyType(LISPType):
    """Untyped (dynamic) value"""

    def __repr__(self) -> str:
        return "Any"

    def __eq__(self, other: object) -> bool:
        return isinstance(other, AnyType)


@dataclass
class IntType(LISPType):
    """Integer type"""

    def __repr__(self) -> str:
        return "Int"

    def __eq__(self, other: object) -> bool:
        return isinstance(other, IntType)


@dataclass
class FloatType(LISPType):
    """Floating-point type"""

    def __repr__(self) -> str:
        return "Float"

    def __eq__(self, other: object) -> bool:
        return isinstance(other, FloatType)


@dataclass
class StringType(LISPType):
    """String type"""

    def __repr__(self) -> str:
        return "String"

    def __eq__(self, other: object) -> bool:
        return isinstance(other, StringType)


@dataclass
class BoolType(LISPType):
    """Boolean type"""

    def __repr__(self) -> str:
        return "Bool"

    def __eq__(self, other: object) -> bool:
        return isinstance(other, BoolType)


@dataclass
class ListType(LISPType):
    """List type (homogeneous elements)"""
    element_type: LISPType

    def __repr__(self) -> str:
        return f"List[{self.element_type}]"

    def __eq__(self, other: object) -> bool:
        return (isinstance(other, ListType) and
                self.element_type == other.element_type)


@dataclass
class FunctionType(LISPType):
    """Function type"""
    arg_types: list[LISPType]
    return_type: LISPType

    def __repr__(self) -> str:
        args_str = ", ".join(str(t) for t in self.arg_types)
        return f"({args_str}) → {self.return_type}"

    def __eq__(self, other: object) -> bool:
        return (isinstance(other, FunctionType) and
                self.arg_types == other.arg_types and
                self.return_type == other.return_type)


@dataclass
class VoidType(LISPType):
    """Void type (function with no return value)"""

    def __repr__(self) -> str:
        return "Void"

    def __eq__(self, other: object) -> bool:
        return isinstance(other, VoidType)


class LISPTypeSystem:
    """Type inference and checking for LISP"""

    # Singleton instances
    ANY = AnyType()
    INT = IntType()
    FLOAT = FloatType()
    STRING = StringType()
    BOOL = BoolType()
    VOID = VoidType()

    def __init__(self) -> None:
        """Initialize type system"""
        self.symbol_types: dict[str, LISPType] = {}

    def infer_type_from_literal(self, value) -> LISPType:
        """Infer type from literal value"""
        if isinstance(value, int):
            return self.INT
        elif isinstance(value, float):
            return self.FLOAT
        elif isinstance(value, str):
            return self.STRING
        elif isinstance(value, bool):
            return self.BOOL
        else:
            return self.ANY

    def infer_operation_type(self, op: str, arg_types: list[LISPType]) -> LISPType:
        """Infer result type of an operation"""

        # Arithmetic operations
        if op in ('+', '-', '*', '/', '%'):
            # If any operand is float, result is float
            if any(isinstance(t, FloatType) for t in arg_types):
                return self.FLOAT
            if all(isinstance(t, IntType) for t in arg_types):
                return self.INT
            return self.ANY

        # Comparison operations
        if op in ('=', '/=', '<', '<=', '>', '>='):
            return self.BOOL

        # Logic operations
        if op in ('and', 'or', 'not'):
            return self.BOOL

        # Math functions
        if op in ('abs', 'sqrt', 'floor', 'ceil', 'round'):
            if arg_types and isinstance(arg_types[0], FloatType):
                return self.FLOAT
            return self.INT

        # List operations
        if op == 'length':
            return self.INT
        if op == 'append':
            if arg_types:
                return arg_types[0]  # Same type as first arg
            return self.ANY
        if op == 'list':
            if arg_types:
                return ListType(arg_types[0])
            return ListType(self.ANY)

        # Type predicates
        if op in ('numberp', 'stringp', 'symbolp', 'listp', 'null',
                 'atom', 'atom', 'numberp'):
            return self.BOOL

        # I/O
        if op == 'print':
            return self.VOID

        # Default to Any
        return self.ANY

    def unify(self, type1: LISPType, type2: LISPType) -> LISPType:
        """Unify two types - return most specific common type"""

        # If types are equal
        if type1 == type2:
            return type1

        # Any unifies with anything
        if isinstance(type1, AnyType):
            return type2
        if isinstance(type2, AnyType):
            return type1

        # Numeric promotion: Int + Float = Float
        if isinstance(type1, IntType) and isinstance(type2, FloatType):
            return self.FLOAT
        if isinstance(type1, FloatType) and isinstance(type2, IntType):
            return self.FLOAT

        # List unification
        if isinstance(type1, ListType) and isinstance(type2, ListType):
            element_type = self.unify(type1.element_type, type2.element_type)
            return ListType(element_type)

        # No unification possible
        return self.ANY

    def register_symbol(self, name: str, lisp_type: LISPType) -> None:
        """Register a symbol with a type"""
        self.symbol_types[name] = lisp_type

    def lookup_symbol(self, name: str) -> LISPType | None:
        """Look up symbol type"""
        return self.symbol_types.get(name)

    def is_numeric(self, lisp_type: LISPType) -> bool:
        """Check if type is numeric"""
        return isinstance(lisp_type, (IntType, FloatType))

    def is_truthy(self, lisp_type: LISPType) -> bool:
        """Check if type can be used in boolean context"""
        return not isinstance(lisp_type, VoidType)


class BabbageTypeMapper:
    """Maps LISP types to Babbage IR types"""

    # Babbage only has two primitive types: i64 and f64
    # Everything else is represented as pointers

    @staticmethod
    def to_babbage_type(lisp_type: LISPType) -> str:
        """Map LISP type to Babbage IR type string"""

        if isinstance(lisp_type, IntType):
            return "i64"
        elif isinstance(lisp_type, FloatType):
            return "f64"
        elif isinstance(lisp_type, StringType):
            return "ptr"  # Pointer to string in memory
        elif isinstance(lisp_type, BoolType):
            return "i64"  # Bool as 0/1 in i64
        elif isinstance(lisp_type, ListType):
            return "ptr"  # Pointer to list structure in heap
        elif isinstance(lisp_type, FunctionType):
            return "ptr"  # Function reference/closure
        elif isinstance(lisp_type, VoidType):
            return "void"
        else:  # AnyType
            return "i64"  # Default to integer register

    @staticmethod
    def register_for_type(lisp_type: LISPType) -> str:
        """Get register name for type (A, B, C, D)"""
        # In Babbage ISA, we use registers A-D
        # Typically:
        #   A = accumulator (primary computation)
        #   B = secondary (operations, function args)
        #   C = temporary
        #   D = result
        return "A"  # Simplified: all use A by default
