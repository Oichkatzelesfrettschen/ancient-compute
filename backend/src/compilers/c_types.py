"""C type system and mapping to Babbage ISA representation.

This module handles:
  - C type representations (int, float, void, pointers)
  - Type compatibility checking
  - Mapping C types to Babbage dec50 representation
  - Type conversion and promotion rules
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum


class CTypeKind(Enum):
    """C type classification."""
    INT = "int"           # 64-bit signed integer
    FLOAT = "float"       # 64-bit IEEE 754 double
    VOID = "void"         # Void type (no value)
    POINTER = "pointer"   # Pointer type
    ARRAY = "array"       # Array type
    FUNCTION = "function" # Function type


@dataclass
class CType:
    """C type representation."""
    kind: CTypeKind
    base_type: CType | None = None  # For pointers and arrays
    size: int | None = None  # For arrays

    def __str__(self) -> str:
        """String representation of type."""
        if self.kind == CTypeKind.POINTER:
            return f"{self.base_type}*"
        elif self.kind == CTypeKind.ARRAY:
            return f"{self.base_type}[{self.size}]"
        else:
            return self.kind.value

    def is_integral(self) -> bool:
        """Check if type is integral."""
        return self.kind == CTypeKind.INT

    def is_floating(self) -> bool:
        """Check if type is floating point."""
        return self.kind == CTypeKind.FLOAT

    def is_numeric(self) -> bool:
        """Check if type is numeric (int or float)."""
        return self.is_integral() or self.is_floating()

    def is_void(self) -> bool:
        """Check if type is void."""
        return self.kind == CTypeKind.VOID

    def is_pointer(self) -> bool:
        """Check if type is pointer."""
        return self.kind == CTypeKind.POINTER

    def is_array(self) -> bool:
        """Check if type is array."""
        return self.kind == CTypeKind.ARRAY

    def get_size_bytes(self) -> int:
        """Get type size in bytes."""
        # On Babbage, all values are 50-digit decimal (stored as i64 internally)
        if self.kind == CTypeKind.INT:
            return 8  # 64-bit integer
        elif self.kind == CTypeKind.FLOAT:
            return 8  # 64-bit float
        elif self.kind == CTypeKind.POINTER:
            return 8  # 64-bit address
        elif self.kind == CTypeKind.ARRAY:
            elem_size = self.base_type.get_size_bytes()
            return elem_size * (self.size or 1)
        elif self.kind == CTypeKind.VOID:
            return 0
        else:
            return 8


class CTypeSystem:
    """C type system with type checking and promotion rules."""

    # Predefined types
    INT_TYPE = CType(CTypeKind.INT)
    FLOAT_TYPE = CType(CTypeKind.FLOAT)
    VOID_TYPE = CType(CTypeKind.VOID)

    def __init__(self) -> None:
        """Initialize type system."""
        self.type_cache: dict[str, CType] = {
            'int': self.INT_TYPE,
            'float': self.FLOAT_TYPE,
            'void': self.VOID_TYPE,
        }

    def make_type(self, name: str) -> CType | None:
        """Create a type from name string."""
        if name in self.type_cache:
            return self.type_cache[name]

        if name == 'int':
            return self.INT_TYPE
        elif name == 'float':
            return self.FLOAT_TYPE
        elif name == 'void':
            return self.VOID_TYPE

        return None

    def make_pointer_type(self, base: CType) -> CType:
        """Create a pointer type."""
        return CType(CTypeKind.POINTER, base_type=base)

    def make_array_type(self, base: CType, size: int) -> CType:
        """Create an array type."""
        return CType(CTypeKind.ARRAY, base_type=base, size=size)

    def get_common_type(self, t1: CType, t2: CType) -> CType:
        """Get the common type for two types (type promotion)."""
        # If types are identical, return that type
        if self._types_equal(t1, t2):
            return t1

        # Numeric type promotion rules
        if t1.is_numeric() and t2.is_numeric():
            # float + int -> float
            if (t1.is_floating() and t2.is_integral()) or (t1.is_integral() and t2.is_floating()):
                return self.FLOAT_TYPE

            # int + int -> int
            if t1.is_integral() and t2.is_integral():
                return self.INT_TYPE

        # Default: cannot promote
        return t1

    def is_assignable(self, target: CType, source: CType) -> bool:
        """Check if source can be assigned to target."""
        # Exact match
        if self._types_equal(target, source):
            return True

        # Numeric conversions
        if target.is_numeric() and source.is_numeric():
            return True  # Allow int <-> float conversion

        # Pointer assignments
        if target.is_pointer() and source.is_pointer():
            # Pointer to same type
            if self._types_equal(target.base_type, source.base_type):
                return True

        return False

    def is_compatible(self, t1: CType, t2: CType) -> bool:
        """Check if two types are compatible."""
        return self._types_equal(t1, t2)

    def _types_equal(self, t1: CType | None, t2: CType | None) -> bool:
        """Check if two types are structurally equal."""
        if t1 is None or t2 is None:
            return t1 == t2

        if t1.kind != t2.kind:
            return False

        if t1.kind == CTypeKind.POINTER:
            return self._types_equal(t1.base_type, t2.base_type)

        if t1.kind == CTypeKind.ARRAY:
            return (self._types_equal(t1.base_type, t2.base_type) and
                    t1.size == t2.size)

        return True


class BabbageTypeMapper:
    """Maps C types to Babbage IR types."""

    def __init__(self) -> None:
        """Initialize mapper."""
        self.type_system = CTypeSystem()

    def c_type_to_babbage_ir_type(self, c_type: CType) -> str:
        """Map C type to Babbage IR type string.

        All C types ultimately map to Babbage's single numeric type (dec50),
        but we preserve type information in IR for validation.
        """
        if c_type.is_integral():
            return "i64"  # 64-bit signed integer
        elif c_type.is_floating():
            return "f64"  # 64-bit floating point
        elif c_type.is_pointer():
            return "ptr"  # Pointer (address)
        elif c_type.is_array():
            elem_type = self.c_type_to_babbage_ir_type(c_type.base_type)
            return f"array({elem_type})"
        elif c_type.is_void():
            return "void"
        else:
            return "unknown"

    def get_conversion_code(self, source_type: str, target_type: str) -> str | None:
        """Get IR code snippet to convert between types.

        Returns the conversion operation needed, or None if no conversion needed.
        """
        # No conversion needed for identical types
        if source_type == target_type:
            return None

        # Numeric conversions
        if source_type == "i64" and target_type == "f64":
            return "to_float"
        elif source_type == "f64" and target_type == "i64":
            return "to_int"

        # Pointer conversions
        if source_type == "ptr":
            if target_type == "i64":
                return "ptr_to_int"
        elif target_type == "ptr":
            if source_type == "i64":
                return "int_to_ptr"

        return None


# Built-in type constants
INT_TYPE = CType(CTypeKind.INT)
FLOAT_TYPE = CType(CTypeKind.FLOAT)
VOID_TYPE = CType(CTypeKind.VOID)
