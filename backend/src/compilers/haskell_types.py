"""
Haskell Type System for Babbage Compilation

Supports:
- Simple types: int, float, string, char, bool
- Polymorphic types: a, b, c (type variables)
- Compound types: List, Tuple, function types (a -> b)
- Type inference and unification
- Mapping to Babbage IR types
"""

from __future__ import annotations
from typing import Dict, Optional, Set, Tuple, Any
from dataclasses import dataclass


# ============================================================================
# Type Representation
# ============================================================================


@dataclass
class HaskellType:
    """Represents a Haskell type"""

    def __init__(self, kind: str, args: Optional[list] = None) -> None:
        """
        Create a Haskell type

        Args:
            kind: Type kind - 'int', 'float', 'string', 'char', 'bool', 'a', 'List', '->', etc.
            args: Type arguments for compound types (e.g., List -> args=[Int])
        """
        self.kind = kind
        self.args = args or []

    def __str__(self) -> str:
        if not self.args:
            return self.kind
        if self.kind == "->":
            # Function type: a -> b -> c
            if len(self.args) >= 2:
                return f"({self.args[0]} -> {self.args[1]})"
            return self.kind
        if self.kind == "List":
            return f"[{self.args[0]}]"
        if self.kind == "Tuple":
            return f"({', '.join(str(a) for a in self.args)})"
        return f"{self.kind}({', '.join(str(a) for a in self.args)})"

    def __repr__(self) -> str:
        if not self.args:
            return f"HaskellType({self.kind!r})"
        return f"HaskellType({self.kind!r}, {self.args!r})"

    def __eq__(self, other) -> bool:
        if not isinstance(other, HaskellType):
            return False
        if self.kind != other.kind:
            return False
        if len(self.args) != len(other.args):
            return False
        return all(a == b for a, b in zip(self.args, other.args))

    def __hash__(self) -> int:
        return hash((self.kind, tuple(self.args)))

    @staticmethod
    def int() -> HaskellType:
        return HaskellType("int")

    @staticmethod
    def float() -> HaskellType:
        return HaskellType("float")

    @staticmethod
    def string() -> HaskellType:
        return HaskellType("string")

    @staticmethod
    def char() -> HaskellType:
        return HaskellType("char")

    @staticmethod
    def bool() -> HaskellType:
        return HaskellType("bool")

    @staticmethod
    def var(name: str) -> HaskellType:
        """Create type variable (a, b, c, etc.)"""
        return HaskellType(name)

    @staticmethod
    def list(elem_type: HaskellType) -> HaskellType:
        return HaskellType("List", [elem_type])

    @staticmethod
    def tuple(elem_types: list) -> HaskellType:
        return HaskellType("Tuple", elem_types)

    @staticmethod
    def function(arg_type: HaskellType, return_type: HaskellType) -> HaskellType:
        """Create function type: arg -> return"""
        return HaskellType("->", [arg_type, return_type])

    def is_polymorphic(self) -> bool:
        """Check if type contains type variables"""
        if len(self.kind) == 1 and self.kind.islower():
            return True
        return any(arg.is_polymorphic() for arg in self.args)

    def is_var(self) -> bool:
        """Check if this is a type variable"""
        return len(self.kind) == 1 and self.kind.islower() and len(self.args) == 0

    def is_function(self) -> bool:
        """Check if this is a function type"""
        return self.kind == "->"

    def is_list(self) -> bool:
        """Check if this is a list type"""
        return self.kind == "List"

    def apply_substitution(self, subst: Dict[str, HaskellType]) -> HaskellType:
        """Apply type variable substitution"""
        if self.is_var() and self.kind in subst:
            return subst[self.kind]
        if self.args:
            new_args = [arg.apply_substitution(subst) for arg in self.args]
            return HaskellType(self.kind, new_args)
        return self


# ============================================================================
# Type Inference
# ============================================================================


class TypeVariable:
    """Generates unique type variables"""

    _counter = 0

    @classmethod
    def fresh(cls) -> HaskellType:
        """Generate fresh type variable"""
        var_name = f"t{cls._counter}"
        cls._counter += 1
        return HaskellType.var(var_name)

    @classmethod
    def reset(cls) -> None:
        """Reset counter (for testing)"""
        cls._counter = 0


class HaskellTypeSystem:
    """Type system for Haskell with inference and unification"""

    def __init__(self) -> None:
        """Initialize type system"""
        self.substitution: Dict[str, HaskellType] = {}
        TypeVariable.reset()

    def infer_literal_type(self, value: Any) -> HaskellType:
        """Infer type from literal value"""
        if isinstance(value, bool):
            return HaskellType.bool()
        elif isinstance(value, int):
            return HaskellType.int()
        elif isinstance(value, float):
            return HaskellType.float()
        elif isinstance(value, str):
            if len(value) == 1:
                return HaskellType.char()
            return HaskellType.string()
        else:
            return TypeVariable.fresh()

    def unify(self, type1: HaskellType, type2: HaskellType) -> bool:
        """
        Unify two types, updating substitution if successful

        Returns: True if unification succeeds, False otherwise
        """
        # Apply current substitutions
        type1 = self._deref(type1)
        type2 = self._deref(type2)

        # Same type
        if type1 == type2:
            return True

        # Type variable cases
        if type1.is_var():
            if type1.kind in self.substitution:
                return self.unify(self.substitution[type1.kind], type2)
            if type2.is_var():
                self.substitution[type1.kind] = type2
                return True
            if not self._occurs_check(type1.kind, type2):
                self.substitution[type1.kind] = type2
                return True
            return False

        if type2.is_var():
            if type2.kind in self.substitution:
                return self.unify(type1, self.substitution[type2.kind])
            if not self._occurs_check(type2.kind, type1):
                self.substitution[type2.kind] = type1
                return True
            return False

        # Both are compound types
        if type1.kind != type2.kind or len(type1.args) != len(type2.args):
            return False

        # Recursively unify arguments
        for arg1, arg2 in zip(type1.args, type2.args):
            if not self.unify(arg1, arg2):
                return False

        return True

    def _deref(self, typ: HaskellType) -> HaskellType:
        """Dereference type variable"""
        if typ.is_var() and typ.kind in self.substitution:
            return self._deref(self.substitution[typ.kind])
        return typ

    def _occurs_check(self, var: str, typ: HaskellType) -> bool:
        """Check if variable occurs in type (prevents infinite types)"""
        typ = self._deref(typ)
        if typ.is_var():
            return typ.kind == var
        return any(self._occurs_check(var, arg) for arg in typ.args)

    def generalize(self, typ: HaskellType) -> HaskellType:
        """Generalize type by applying current substitution"""
        return typ.apply_substitution(self.substitution)

    def operation_type(
        self, op: str, left: HaskellType, right: Optional[HaskellType] = None
    ) -> HaskellType:
        """Determine result type of operation"""
        if right is None:
            # Unary operation
            if op == "-" or op == "+":
                return left
            else:
                return TypeVariable.fresh()

        # Binary operation
        left = self._deref(left)
        right = self._deref(right)

        # Comparison operations return Bool
        if op in ("==", "/=", "<", "<=", ">", ">="):
            return HaskellType.bool()

        # Arithmetic operations
        if op in ("+", "-", "*", "/", "%", "^"):
            # Type promotion: int + float -> float
            if left == HaskellType.int() and right == HaskellType.float():
                return HaskellType.float()
            if left == HaskellType.float() and right == HaskellType.int():
                return HaskellType.float()
            if left == HaskellType.float() or right == HaskellType.float():
                return HaskellType.float()
            if left == HaskellType.int() and right == HaskellType.int():
                return HaskellType.int()

        # List concatenation
        if op == "++":
            if left.is_list() and right.is_list():
                return left

        return TypeVariable.fresh()

    def is_numeric(self, typ: HaskellType) -> bool:
        """Check if type is numeric"""
        typ = self._deref(typ)
        return typ.kind in ("int", "float")


# ============================================================================
# Babbage Type Mapping
# ============================================================================


class BabbageTypeMapper:
    """Maps Haskell types to Babbage IR types"""

    TYPE_MAP = {
        "int": "i64",
        "float": "f64",
        "string": "i64",  # String as memory pointer
        "char": "i64",  # Char as integer
        "bool": "i64",  # Bool as 0/1
        "List": "i64",  # List as pointer
        "Tuple": "i64",  # Tuple as pointer
        "->": "i64",  # Function pointer
    }

    @staticmethod
    def to_babbage_type(typ: HaskellType) -> str:
        """Map Haskell type to Babbage IR type"""
        if typ.is_var():
            return "i64"  # Default to i64 for type variables
        return BabbageTypeMapper.TYPE_MAP.get(typ.kind, "i64")

    @staticmethod
    def default_value_for_type(typ: HaskellType) -> Any:
        """Get default value for Haskell type"""
        typ_deref = typ
        if typ.is_var():
            return 0

        if typ.kind == "int":
            return 0
        elif typ.kind == "float":
            return 0.0
        elif typ.kind == "bool":
            return False
        elif typ.kind == "char":
            return ""
        elif typ.kind == "string":
            return ""
        elif typ.kind == "List":
            return []
        elif typ.kind == "Tuple":
            return ()
        else:
            return 0


# ============================================================================
# Type Inference Environment
# ============================================================================


@dataclass
class TypeEnvironment:
    """Type inference environment (bindings from names to types)"""

    def __init__(self, parent: Optional[TypeEnvironment] = None) -> None:
        """Initialize environment"""
        self.bindings: Dict[str, HaskellType] = {}
        self.parent = parent

    def bind(self, name: str, typ: HaskellType) -> None:
        """Bind name to type in current scope"""
        self.bindings[name] = typ

    def lookup(self, name: str) -> Optional[HaskellType]:
        """Look up type, checking parent scopes"""
        if name in self.bindings:
            return self.bindings[name]
        if self.parent:
            return self.parent.lookup(name)
        return None

    def exists(self, name: str) -> bool:
        """Check if name is bound"""
        return self.lookup(name) is not None
