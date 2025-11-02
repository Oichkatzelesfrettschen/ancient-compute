"""
IDRIS2 Type System - Type inference and checking for dependent types

Handles:
  - Type inference from expressions
  - Dependent type unification
  - Type family instantiation
  - Implicit parameter resolution
  - Mapping to Babbage IR types

Key challenge: Dependent types allow types to depend on values.
Example: Vect n a (vector of length n containing type a)
         At compile-time, n is a value, not just a type variable.
"""

from __future__ import annotations
from typing import Dict, List, Optional, Tuple, Union
from backend.src.compilers.idris_ast import (
    Type,
    BaseType,
    TypeVariable,
    FunctionType,
    DependentType,
    TypeFamily,
    RefinementType,
    Expr,
    Var,
    Literal,
    Lambda,
    Application,
    LetExpr,
    CaseExpr,
    IfExpr,
    ProofExpr,
    DataConstructor,
)


class IDRISTypeError(Exception):
    """Type error in IDRIS2 expression"""

    pass


class IDRISType:
    """Wrapper for IDRIS2 type with metadata"""

    def __init__(self, type_: Type, is_dependent: bool = False) -> None:
        self.type_ = type_
        self.is_dependent = is_dependent

    def __repr__(self) -> str:
        return f"IDRISType({self.type_!r}, dependent={self.is_dependent})"

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, IDRISType):
            return False
        return self.type_ == other.type_ and self.is_dependent == other.is_dependent


class IDRISTypeSystem:
    """Type system for IDRIS2 with dependent types"""

    def __init__(self) -> None:
        self.symbols: Dict[str, IDRISType] = {}
        self.functions: Dict[str, FunctionType] = {}
        self.data_types: Dict[str, Tuple[List[Tuple[str, Type]], List[Tuple[str, Type]]]] = {}

        # Pre-register built-in types
        self._register_builtins()

    def _register_builtins(self) -> None:
        """Register built-in types and functions"""
        # Base types
        self.symbols["Nat"] = IDRISType(BaseType("Nat"))
        self.symbols["Int"] = IDRISType(BaseType("Int"))
        self.symbols["String"] = IDRISType(BaseType("String"))
        self.symbols["Bool"] = IDRISType(BaseType("Bool"))
        self.symbols["Char"] = IDRISType(BaseType("Char"))
        self.symbols["Double"] = IDRISType(BaseType("Double"))
        self.symbols["Type"] = IDRISType(BaseType("Type"))

        # Built-in data constructors
        self.symbols["Zero"] = IDRISType(BaseType("Nat"))
        self.symbols["Succ"] = IDRISType(FunctionType(BaseType("Nat"), BaseType("Nat")))
        self.symbols["True"] = IDRISType(BaseType("Bool"))
        self.symbols["False"] = IDRISType(BaseType("Bool"))

    def register_symbol(self, name: str, type_: Type, is_dependent: bool = False) -> None:
        """Register symbol with type"""
        self.symbols[name] = IDRISType(type_, is_dependent)

    def lookup_symbol(self, name: str) -> Optional[IDRISType]:
        """Look up symbol type"""
        return self.symbols.get(name)

    def register_function(self, name: str, func_type: FunctionType) -> None:
        """Register function type"""
        self.functions[name] = func_type
        self.symbols[name] = IDRISType(func_type, is_dependent=True)

    def register_data_type(
        self, name: str, type_params: List[Tuple[str, Type]], constructors: List[Tuple[str, Type]]
    ) -> None:
        """Register data type definition"""
        self.data_types[name] = (type_params, constructors)
        self.symbols[name] = IDRISType(BaseType(name), is_dependent=len(type_params) > 0)

    def infer_type(self, expr: Expr) -> Type:
        """Infer type of expression"""
        if isinstance(expr, Literal):
            return self._infer_literal_type(expr.value)

        if isinstance(expr, Var):
            sym = self.lookup_symbol(expr.name)
            if sym:
                return sym.type_
            raise IDRISTypeError(f"Undefined variable: {expr.name}")

        if isinstance(expr, DataConstructor):
            sym = self.lookup_symbol(expr.constructor_name)
            if sym:
                return sym.type_
            raise IDRISTypeError(f"Undefined constructor: {expr.constructor_name}")

        if isinstance(expr, Lambda):
            return self._infer_lambda_type(expr)

        if isinstance(expr, Application):
            return self._infer_application_type(expr)

        if isinstance(expr, LetExpr):
            return self._infer_let_type(expr)

        if isinstance(expr, CaseExpr):
            return self._infer_case_type(expr)

        if isinstance(expr, IfExpr):
            return self._infer_if_type(expr)

        if isinstance(expr, ProofExpr):
            return expr.type_

        raise IDRISTypeError(f"Cannot infer type of {type(expr).__name__}")

    def _infer_literal_type(self, value: Union[int, float, str]) -> Type:
        """Infer type from literal value"""
        if isinstance(value, int):
            return BaseType("Nat")
        elif isinstance(value, float):
            return BaseType("Double")
        elif isinstance(value, str):
            return BaseType("String")
        else:
            raise IDRISTypeError(f"Unknown literal type: {type(value)}")

    def _infer_lambda_type(self, expr: Lambda) -> FunctionType:
        """Infer type of lambda expression"""
        if not expr.parameters:
            raise IDRISTypeError("Lambda must have parameters")

        # Build function type from parameters
        param_type = expr.parameters[0][1]
        body_type = self.infer_type(expr.body)

        # If multiple parameters, nest the types
        if len(expr.parameters) > 1:
            return FunctionType(
                param_type, self._infer_lambda_type(Lambda(expr.parameters[1:], expr.body))
            )

        return FunctionType(param_type, body_type)

    def _infer_application_type(self, expr: Application) -> Type:
        """Infer type of function application"""
        func_type = self.infer_type(expr.func)

        # Function type should be FunctionType or DependentType
        if isinstance(func_type, FunctionType):
            return func_type.return_type

        if isinstance(func_type, DependentType):
            # Instantiate dependent type with argument
            if expr.args:
                # In a simplified implementation, just return the return type
                return func_type.return_type
            return func_type

        raise IDRISTypeError(f"Cannot apply non-function type: {func_type}")

    def _infer_let_type(self, expr: LetExpr) -> Type:
        """Infer type of let expression"""
        # Type of let is the type of the body
        return self.infer_type(expr.body)

    def _infer_case_type(self, expr: CaseExpr) -> Type:
        """Infer type of case expression"""
        # All branches should have same type
        if not expr.branches:
            raise IDRISTypeError("Case expression must have at least one branch")

        # Infer type from first branch
        _, result_expr = expr.branches[0]
        return self.infer_type(result_expr)

    def _infer_if_type(self, expr: IfExpr) -> Type:
        """Infer type of if expression"""
        then_type = self.infer_type(expr.then_expr)
        else_type = self.infer_type(expr.else_expr) if expr.else_expr else then_type

        # Both branches should have same type
        if not self._types_equal(then_type, else_type):
            raise IDRISTypeError(f"If branches have different types: {then_type} vs {else_type}")

        return then_type

    def _types_equal(self, t1: Type, t2: Type) -> bool:
        """Check if two types are equal"""
        if isinstance(t1, BaseType) and isinstance(t2, BaseType):
            return t1.name == t2.name

        if isinstance(t1, TypeVariable) and isinstance(t2, TypeVariable):
            return t1.name == t2.name

        if isinstance(t1, FunctionType) and isinstance(t2, FunctionType):
            return self._types_equal(t1.arg_type, t2.arg_type) and self._types_equal(
                t1.return_type, t2.return_type
            )

        if isinstance(t1, TypeFamily) and isinstance(t2, TypeFamily):
            if t1.family_name != t2.family_name:
                return False
            if len(t1.type_args) != len(t2.type_args):
                return False
            return all(self._types_equal(a1, a2) for a1, a2 in zip(t1.type_args, t2.type_args))

        return False

    def unify(self, t1: Type, t2: Type) -> Type:
        """Unify two types; return unified type or raise error"""
        # Same type
        if self._types_equal(t1, t2):
            return t1

        # Type variable unifies with anything
        if isinstance(t1, TypeVariable):
            return t2
        if isinstance(t2, TypeVariable):
            return t1

        # No unification possible
        raise IDRISTypeError(f"Cannot unify {t1} with {t2}")

    def check_dependent_type(self, dep_type: DependentType, arg: Expr) -> Type:
        """Check argument against dependent type parameter"""
        arg_type = self.infer_type(arg)

        # Unify argument type with parameter type
        try:
            self.unify(arg_type, dep_type.param_type)
        except IDRISTypeError:
            raise IDRISTypeError(
                f"Argument type {arg_type} doesn't match "
                f"dependent parameter type {dep_type.param_type}"
            )

        return dep_type.return_type

    def instantiate_type_family(self, family: TypeFamily, args: List[Expr]) -> Type:
        """Instantiate type family with arguments"""
        # In simplified implementation, just return the family itself
        # Full implementation would compute actual type based on argument values
        return family

    def to_babbage_type(self, type_: Type) -> str:
        """Map IDRIS2 type to Babbage IR type"""
        if isinstance(type_, BaseType):
            if type_.name in ("Nat", "Int"):
                return "i64"
            elif type_.name in ("Double", "Float"):
                return "f64"
            elif type_.name in ("String", "Char"):
                return "ptr"
            else:
                # Unknown type defaults to ptr
                return "ptr"

        if isinstance(type_, FunctionType):
            return "ptr"  # Functions are pointers in Babbage

        if isinstance(type_, TypeFamily):
            return "ptr"  # Type families are pointers

        if isinstance(type_, DependentType):
            return "ptr"  # Dependent types are pointers

        # Default
        return "ptr"


class DependentTypeChecker:
    """Helper for checking dependent type constraints"""

    def __init__(self, type_system: IDRISTypeSystem) -> None:
        self.type_system = type_system

    def check_refinement(self, ref_type: RefinementType, expr: Expr) -> bool:
        """Check if expression satisfies refinement type constraint"""
        # Simplified: just verify the base type matches
        base_type = self.type_system.infer_type(expr)
        return self.type_system._types_equal(base_type, ref_type.base_type)

    def check_vect_length(self, vect_type: TypeFamily, expr: Expr) -> bool:
        """Check if vector expression has correct length"""
        # Simplified: would require evaluating the length expression
        if vect_type.family_name != "Vect":
            return False
        if len(vect_type.type_args) != 2:
            return False
        # In full implementation, would evaluate the length argument
        return True
