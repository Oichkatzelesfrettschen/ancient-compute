"""
System F Type System - Type inference for polymorphic lambda calculus

Key features:
  - Type inference through unification
  - Instantiation of universal types
  - Polymorphic type checking
  - Mapping to Babbage IR types
"""

from __future__ import annotations

from backend.src.compilers.systemf_ast import (
    Annotation,
    Application,
    BaseType,
    Expr,
    FixExpr,
    FunctionType,
    IfExpr,
    Lambda,
    LetExpr,
    Literal,
    Type,
    TypeAbstraction,
    TypeApplication,
    TypeVar,
    UniversalType,
    Var,
)


class SystemFTypeError(Exception):
    """Type error in System F expression"""
    pass


class SystemFTypeSystem:
    """Type system for System F"""

    def __init__(self) -> None:
        self.symbols: dict[str, Type] = {}
        self._register_builtins()

    def _register_builtins(self) -> None:
        """Register built-in values and functions"""
        # Built-in functions
        self.symbols["true"] = BaseType("bool")
        self.symbols["false"] = BaseType("bool")
        self.symbols["zero"] = BaseType("nat")

    def register_symbol(self, name: str, type_: Type) -> None:
        """Register symbol with type"""
        self.symbols[name] = type_

    def lookup_symbol(self, name: str) -> Type | None:
        """Look up symbol type"""
        return self.symbols.get(name)

    def infer_type(self, expr: Expr) -> Type:
        """Infer type of expression"""
        if isinstance(expr, Literal):
            return self._infer_literal_type(expr.value)

        if isinstance(expr, Var):
            sym = self.lookup_symbol(expr.name)
            if sym:
                return sym
            raise SystemFTypeError(f"Undefined variable: {expr.name}")

        if isinstance(expr, Lambda):
            body_type = self.infer_type(expr.body)
            return FunctionType(expr.param_type, body_type)

        if isinstance(expr, TypeAbstraction):
            body_type = self.infer_type(expr.body)
            return UniversalType(expr.type_var, body_type)

        if isinstance(expr, Application):
            func_type = self.infer_type(expr.func)
            if isinstance(func_type, FunctionType):
                return func_type.return_type
            raise SystemFTypeError(f"Cannot apply non-function type: {func_type}")

        if isinstance(expr, TypeApplication):
            expr_type = self.infer_type(expr.expr)
            if isinstance(expr_type, UniversalType):
                # Instantiate universal type
                return self._instantiate(expr_type, expr.type_arg)
            raise SystemFTypeError("Cannot apply type to non-polymorphic expression")

        if isinstance(expr, IfExpr):
            then_type = self.infer_type(expr.then_expr)
            else_type = self.infer_type(expr.else_expr)
            if self._types_equal(then_type, else_type):
                return then_type
            raise SystemFTypeError("If branches have different types")

        if isinstance(expr, LetExpr):
            # Just return body type
            return self.infer_type(expr.body_expr)

        if isinstance(expr, FixExpr):
            func_type = self.infer_type(expr.func)
            if isinstance(func_type, FunctionType):
                return func_type.return_type
            raise SystemFTypeError("Fix requires function type")

        if isinstance(expr, Annotation):
            return expr.type_annotation

        raise SystemFTypeError(f"Cannot infer type of {type(expr).__name__}")

    def _infer_literal_type(self, value: object) -> Type:
        """Infer type from literal value"""
        if isinstance(value, int):
            return BaseType("nat")
        elif isinstance(value, float):
            return BaseType("real")
        elif isinstance(value, str):
            return BaseType("string")
        elif isinstance(value, bool):
            return BaseType("bool")
        else:
            raise SystemFTypeError(f"Unknown literal type: {type(value)}")

    def _types_equal(self, t1: Type, t2: Type) -> bool:
        """Check if two types are equal"""
        if isinstance(t1, BaseType) and isinstance(t2, BaseType):
            return t1.name == t2.name
        if isinstance(t1, TypeVar) and isinstance(t2, TypeVar):
            return t1.name == t2.name
        if isinstance(t1, FunctionType) and isinstance(t2, FunctionType):
            return (self._types_equal(t1.arg_type, t2.arg_type) and
                    self._types_equal(t1.return_type, t2.return_type))
        if isinstance(t1, UniversalType) and isinstance(t2, UniversalType):
            return (t1.type_var == t2.type_var and
                    self._types_equal(t1.body, t2.body))
        return False

    def _instantiate(self, univ_type: UniversalType, arg_type: Type) -> Type:
        """Instantiate universal type with concrete type"""
        # Substitute type_var with arg_type in body
        return self._substitute(univ_type.body, univ_type.type_var, arg_type)

    def _substitute(self, type_: Type, var: str, with_type: Type) -> Type:
        """Substitute type variable with concrete type"""
        if isinstance(type_, TypeVar):
            return with_type if type_.name == var else type_
        elif isinstance(type_, FunctionType):
            return FunctionType(
                self._substitute(type_.arg_type, var, with_type),
                self._substitute(type_.return_type, var, with_type)
            )
        elif isinstance(type_, UniversalType):
            if type_.type_var == var:
                return type_  # Shadowed variable
            return UniversalType(
                type_.type_var,
                self._substitute(type_.body, var, with_type)
            )
        else:
            return type_

    def unify(self, t1: Type, t2: Type) -> Type:
        """Unify two types"""
        if self._types_equal(t1, t2):
            return t1
        if isinstance(t1, TypeVar):
            return t2
        if isinstance(t2, TypeVar):
            return t1
        raise SystemFTypeError(f"Cannot unify {t1} with {t2}")

    def to_babbage_type(self, type_: Type) -> str:
        """Map System F type to Babbage IR type"""
        if isinstance(type_, BaseType):
            if type_.name in ("nat", "int"):
                return "i64"
            elif type_.name in ("real", "float"):
                return "f64"
            elif type_.name in ("string", "char"):
                return "ptr"
            elif type_.name == "bool":
                return "i64"
            else:
                return "ptr"

        # Functions and polymorphic types are pointers
        return "ptr"
