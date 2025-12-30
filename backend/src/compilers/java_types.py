"""
Java Type System - Type checking and inference for Java

Supports:
  - Primitive types (int, float, boolean, etc.)
  - Reference types (classes, interfaces)
  - Array types
  - Generic types with type parameters
  - Type inference
  - Type hierarchy and subtyping
"""

from __future__ import annotations
from typing import Dict, Optional, List, Tuple
from backend.src.compilers.java_ast import (
    Type, PrimitiveType, ReferenceType, ArrayType, TypeParameter,
    Expr, Literal, Variable, FieldAccess, MethodCall, NewExpression,
    BinaryOp, UnaryOp, InstanceofExpr, CastExpr
)


class JavaTypeError(Exception):
    """Type error in Java expression"""
    pass


class JavaTypeSystem:
    """Type system for Java"""

    # Primitive type hierarchy
    PRIMITIVE_TYPES = {
        'int', 'long', 'short', 'byte', 'float', 'double', 'boolean', 'char', 'void'
    }

    # Numeric type precedence for operations
    NUMERIC_PRECEDENCE = {
        'byte': 1, 'short': 2, 'int': 3, 'long': 4, 'float': 5, 'double': 6
    }

    def __init__(self) -> None:
        self.symbols: Dict[str, Type] = {}
        self._register_builtins()

    def _register_builtins(self) -> None:
        """Register built-in types and methods"""
        # Common types
        self.symbols['String'] = ReferenceType('String')
        self.symbols['Object'] = ReferenceType('Object')
        self.symbols['System'] = ReferenceType('System')
        self.symbols['Exception'] = ReferenceType('Exception')
        self.symbols['RuntimeException'] = ReferenceType('RuntimeException')

    def register_symbol(self, name: str, type_: Type) -> None:
        """Register symbol with type"""
        self.symbols[name] = type_

    def lookup_symbol(self, name: str) -> Optional[Type]:
        """Look up symbol type"""
        return self.symbols.get(name)

    def infer_type(self, expr: Expr) -> Type:
        """Infer type of expression"""
        if isinstance(expr, Literal):
            return self._infer_literal_type(expr.value)

        if isinstance(expr, Variable):
            sym = self.lookup_symbol(expr.name)
            if sym:
                return sym
            if expr.name == 'this' or expr.name == 'super':
                return ReferenceType('Object')
            raise JavaTypeError(f"Undefined variable: {expr.name}")

        if isinstance(expr, FieldAccess):
            obj_type = self.infer_type(expr.object_expr)
            # Field type depends on object type
            # For now, return Object type
            return ReferenceType('Object')

        if isinstance(expr, MethodCall):
            # Method return type depends on method signature
            # For now, return Object type
            return ReferenceType('Object')

        if isinstance(expr, NewExpression):
            return expr.type_

        if isinstance(expr, BinaryOp):
            return self._infer_binary_op_type(expr)

        if isinstance(expr, UnaryOp):
            return self._infer_unary_op_type(expr)

        if isinstance(expr, InstanceofExpr):
            return PrimitiveType('boolean')

        if isinstance(expr, CastExpr):
            return expr.target_type

        raise JavaTypeError(f"Cannot infer type of {type(expr).__name__}")

    def _infer_literal_type(self, value: object) -> Type:
        """Infer type from literal value"""
        if isinstance(value, bool):
            return PrimitiveType('boolean')
        elif isinstance(value, int):
            return PrimitiveType('int')
        elif isinstance(value, float):
            return PrimitiveType('double')
        elif isinstance(value, str):
            return ReferenceType('String')
        elif value is None:
            return ReferenceType('null')
        else:
            raise JavaTypeError(f"Unknown literal type: {type(value)}")

    def _infer_binary_op_type(self, expr: BinaryOp) -> Type:
        """Infer type of binary operation"""
        left_type = self.infer_type(expr.left)
        right_type = self.infer_type(expr.right)

        # Comparison operators return boolean
        if expr.operator in ['<', '>', '<=', '>=', '==', '!=']:
            return PrimitiveType('boolean')

        # Logical operators return boolean
        if expr.operator in ['&&', '||']:
            return PrimitiveType('boolean')

        # Arithmetic operators
        if expr.operator in ['+', '-', '*', '/', '%']:
            return self._promote_numeric_type(left_type, right_type)

        # Bitwise operators
        if expr.operator in ['&', '|', '^', '<<', '>>', '>>>']:
            return self._promote_numeric_type(left_type, right_type)

        # String concatenation
        if expr.operator == '+':
            if self._is_string_type(left_type) or self._is_string_type(right_type):
                return ReferenceType('String')

        raise JavaTypeError(f"Unknown binary operator: {expr.operator}")

    def _infer_unary_op_type(self, expr: UnaryOp) -> Type:
        """Infer type of unary operation"""
        operand_type = self.infer_type(expr.operand)

        # Logical NOT returns boolean
        if expr.operator == '!':
            return PrimitiveType('boolean')

        # Bitwise NOT
        if expr.operator == '~':
            return operand_type

        # Unary plus/minus
        if expr.operator in ['+', '-']:
            return operand_type

        # Increment/decrement
        if expr.operator in ['++', '--']:
            return operand_type

        raise JavaTypeError(f"Unknown unary operator: {expr.operator}")

    def _promote_numeric_type(self, type1: Type, type2: Type) -> Type:
        """Promote numeric types in binary operation"""
        # Handle non-numeric types
        if not self._is_numeric_type(type1) or not self._is_numeric_type(type2):
            raise JavaTypeError(f"Non-numeric types in arithmetic: {type1} and {type2}")

        # Get primitive type names
        if isinstance(type1, PrimitiveType):
            t1_name = type1.name
        else:
            raise JavaTypeError(f"Expected primitive type, got {type1}")

        if isinstance(type2, PrimitiveType):
            t2_name = type2.name
        else:
            raise JavaTypeError(f"Expected primitive type, got {type2}")

        # Return the higher precedence type
        precedence1 = self.NUMERIC_PRECEDENCE.get(t1_name, 0)
        precedence2 = self.NUMERIC_PRECEDENCE.get(t2_name, 0)

        if precedence1 >= precedence2:
            return type1
        else:
            return type2

    def _is_numeric_type(self, type_: Type) -> bool:
        """Check if type is numeric"""
        if isinstance(type_, PrimitiveType):
            return type_.name in {
                'byte', 'short', 'int', 'long', 'float', 'double', 'char'
            }
        return False

    def _is_string_type(self, type_: Type) -> bool:
        """Check if type is String"""
        if isinstance(type_, ReferenceType):
            return type_.name == 'String'
        return False

    def is_assignable_to(self, from_type: Type, to_type: Type) -> bool:
        """Check if from_type can be assigned to to_type"""
        # Identical types
        if self._types_equal(from_type, to_type):
            return True

        # null can be assigned to any reference type
        if isinstance(from_type, ReferenceType) and from_type.name == 'null':
            if isinstance(to_type, ReferenceType) or isinstance(to_type, ArrayType):
                return True

        # Primitive type widening
        if isinstance(from_type, PrimitiveType) and isinstance(to_type, PrimitiveType):
            from_prec = self.NUMERIC_PRECEDENCE.get(from_type.name, -1)
            to_prec = self.NUMERIC_PRECEDENCE.get(to_type.name, -1)
            if from_prec >= 0 and to_prec >= 0:
                return from_prec <= to_prec

        # Array covariance
        if isinstance(from_type, ArrayType) and isinstance(to_type, ArrayType):
            if from_type.dimensions == to_type.dimensions:
                return self.is_assignable_to(from_type.element_type, to_type.element_type)

        # Reference type compatibility (simplified)
        if isinstance(from_type, ReferenceType) and isinstance(to_type, ReferenceType):
            # Check if from_type is subclass of to_type
            # Simplified: only check for exact match or Object supertype
            if to_type.name == 'Object':
                return True
            return from_type.name == to_type.name

        return False

    def _types_equal(self, t1: Type, t2: Type) -> bool:
        """Check if two types are equal"""
        if isinstance(t1, PrimitiveType) and isinstance(t2, PrimitiveType):
            return t1.name == t2.name

        if isinstance(t1, ReferenceType) and isinstance(t2, ReferenceType):
            if t1.name != t2.name:
                return False
            if len(t1.type_args) != len(t2.type_args):
                return False
            return all(self._types_equal(a1, a2) for a1, a2 in zip(t1.type_args, t2.type_args))

        if isinstance(t1, ArrayType) and isinstance(t2, ArrayType):
            return (t1.dimensions == t2.dimensions and
                    self._types_equal(t1.element_type, t2.element_type))

        return False

    def to_babbage_type(self, type_: Type) -> str:
        """Map Java type to Babbage IR type"""
        if isinstance(type_, PrimitiveType):
            if type_.name in ('int', 'short', 'byte', 'char'):
                return 'i64'
            elif type_.name in ('long',):
                return 'i64'
            elif type_.name in ('float', 'double'):
                return 'f64'
            elif type_.name == 'boolean':
                return 'i64'
            elif type_.name == 'void':
                return 'void'

        # Reference types are pointers
        if isinstance(type_, ReferenceType) or isinstance(type_, ArrayType):
            return 'ptr'

        return 'ptr'

    def check_method_compatibility(self, param_types: List[Type], arg_types: List[Type]) -> bool:
        """Check if arguments match parameter types"""
        if len(param_types) != len(arg_types):
            return False

        for param_type, arg_type in zip(param_types, arg_types):
            if not self.is_assignable_to(arg_type, param_type):
                return False

        return True

    def get_common_type(self, types: List[Type]) -> Optional[Type]:
        """Find common type among multiple types"""
        if not types:
            return None

        common = types[0]
        for type_ in types[1:]:
            if self._types_equal(common, type_):
                continue
            elif isinstance(common, PrimitiveType) and isinstance(type_, PrimitiveType):
                # Promote to higher numeric type
                common = self._promote_numeric_type(common, type_)
            else:
                # Find common superclass (simplified to Object)
                if isinstance(common, ReferenceType) and isinstance(type_, ReferenceType):
                    if type_.name == 'Object' or common.name == 'Object':
                        common = ReferenceType('Object')
                    else:
                        common = ReferenceType('Object')

        return common
