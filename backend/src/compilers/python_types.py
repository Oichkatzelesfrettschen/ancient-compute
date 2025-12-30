"""
Python Type System for Babbage Compilation

Python is dynamically typed, but for Babbage compilation we need static types.
This module provides:
  - PythonType: Representation of Python types (int, float, str, bool, None, Any)
  - TypeInference: Basic type inference from literals and operations
  - BabbageTypeMapper: Maps Python types to Babbage IR types
"""

from __future__ import annotations
from enum import Enum
from typing import Dict, Optional, Set


class PythonType:
    """Represents a Python type for compilation purposes"""

    def __init__(self, kind: str) -> None:
        """
        Create a Python type

        Args:
            kind: Type kind - 'int', 'float', 'str', 'bool', 'None', 'Any'
        """
        self.kind = kind

    def __str__(self) -> str:
        return self.kind

    def __repr__(self) -> str:
        return f"PythonType({self.kind!r})"

    def __eq__(self, other) -> bool:
        if not isinstance(other, PythonType):
            return False
        return self.kind == other.kind

    def __hash__(self) -> int:
        return hash(self.kind)

    @staticmethod
    def int() -> PythonType:
        return PythonType('int')

    @staticmethod
    def float() -> PythonType:
        return PythonType('float')

    @staticmethod
    def str() -> PythonType:
        return PythonType('str')

    @staticmethod
    def bool() -> PythonType:
        return PythonType('bool')

    @staticmethod
    def none() -> PythonType:
        return PythonType('None')

    @staticmethod
    def any() -> PythonType:
        return PythonType('Any')


class PythonTypeSystem:
    """Type system for Python with basic type inference"""

    def __init__(self) -> None:
        """Initialize type system"""
        pass

    def infer_literal_type(self, value) -> PythonType:
        """Infer type from literal value"""
        if isinstance(value, bool):  # bool before int (bool is subclass of int)
            return PythonType.bool()
        elif isinstance(value, int):
            return PythonType.int()
        elif isinstance(value, float):
            return PythonType.float()
        elif isinstance(value, str):
            return PythonType.str()
        elif value is None:
            return PythonType.none()
        else:
            return PythonType.any()

    def promote_type(self, left: PythonType, right: PythonType) -> PythonType:
        """Promote types in binary operation"""
        # None of the same type
        if left.kind == right.kind:
            return left

        # int + float → float
        if (left.kind == 'int' and right.kind == 'float') or \
           (left.kind == 'float' and right.kind == 'int'):
            return PythonType.float()

        # bool + int → int (bool is subclass of int)
        if (left.kind == 'bool' and right.kind == 'int') or \
           (left.kind == 'int' and right.kind == 'bool'):
            return PythonType.int()

        # bool + float → float
        if (left.kind == 'bool' and right.kind == 'float') or \
           (left.kind == 'float' and right.kind == 'bool'):
            return PythonType.float()

        # Default to Any for incompatible types
        return PythonType.any()

    def is_numeric(self, ptype: PythonType) -> bool:
        """Check if type is numeric"""
        return ptype.kind in ('int', 'float', 'bool')

    def is_truthy(self, ptype: PythonType) -> bool:
        """Check if type can be used in boolean context"""
        return ptype.kind != 'None'

    def operation_type(self, op: str, left: PythonType, right: Optional[PythonType] = None) -> PythonType:
        """Determine result type of operation"""
        if right is None:
            # Unary operation
            if op == 'not':
                return PythonType.bool()
            elif op in ('-', '+'):
                return left
            else:
                return PythonType.any()

        # Binary operation
        if op in ('==', '!=', '<', '<=', '>', '>='):
            return PythonType.bool()

        if op in ('and', 'or'):
            return self.promote_type(left, right)

        if op in ('+', '-', '*', '//', '%', '**'):
            if self.is_numeric(left) and self.is_numeric(right):
                return self.promote_type(left, right)
            else:
                return PythonType.any()

        if op == '/':
            # Division always returns float
            if self.is_numeric(left) and self.is_numeric(right):
                return PythonType.float()
            else:
                return PythonType.any()

        return PythonType.any()


class BabbageTypeMapper:
    """Maps Python types to Babbage IR types"""

    TYPE_MAP = {
        'int': 'i64',
        'float': 'f64',
        'str': 'i64',  # Strings as memory pointers
        'bool': 'i64',  # Booleans as 0/1
        'None': 'void',
        'Any': 'i64',  # Default to integer
    }

    @staticmethod
    def to_babbage_type(ptype: PythonType) -> str:
        """Map Python type to Babbage IR type"""
        return BabbageTypeMapper.TYPE_MAP.get(ptype.kind, 'i64')

    @staticmethod
    def default_value_for_type(ptype: PythonType):
        """Get default value for Python type"""
        if ptype.kind == 'int':
            return 0
        elif ptype.kind == 'float':
            return 0.0
        elif ptype.kind == 'bool':
            return False
        elif ptype.kind == 'str':
            return ''
        elif ptype.kind == 'None':
            return None
        else:
            return 0
