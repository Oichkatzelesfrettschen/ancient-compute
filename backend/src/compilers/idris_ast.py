# Ancient Compute - IDRIS AST



# ---------------------------------------------------------------------------
# Type nodes (used by idris_types.py type system)
# ---------------------------------------------------------------------------

class Type:
    """Base class for all Idris type representations."""
    pass


class BaseType(Type):
    """Named base type (Nat, Int, String, Bool, etc.)."""
    def __init__(self, name: str):
        self.name = name

    def __repr__(self) -> str:
        return f"BaseType({self.name!r})"

    def __eq__(self, other: object) -> bool:
        return isinstance(other, BaseType) and self.name == other.name

    def __hash__(self) -> int:
        return hash(self.name)


class TypeVariable(Type):
    """Type variable (e.g. 'a' in 'forall a. a -> a')."""
    def __init__(self, name: str):
        self.name = name

    def __repr__(self) -> str:
        return f"TypeVariable({self.name!r})"


class FunctionType(Type):
    """Arrow type: param_type -> return_type."""
    def __init__(self, param_type: Type, return_type: Type):
        self.param_type = param_type
        self.return_type = return_type

    def __repr__(self) -> str:
        return f"FunctionType({self.param_type!r} -> {self.return_type!r})"


class DependentType(Type):
    """Dependent type: (x : A) -> B x."""
    def __init__(self, param_name: str, param_type: Type, body_type: Type):
        self.param_name = param_name
        self.param_type = param_type
        self.body_type = body_type

    def __repr__(self) -> str:
        return f"DependentType(({self.param_name} : {self.param_type!r}) -> {self.body_type!r})"


class TypeFamily(Type):
    """Type family application: F a1 a2 ... an."""
    def __init__(self, name: str, args: list[Type]):
        self.name = name
        self.args = args

    def __repr__(self) -> str:
        return f"TypeFamily({self.name}, {self.args!r})"


class RefinementType(Type):
    """Refinement type: { x : A | P x }."""
    def __init__(self, base_type: Type, predicate: "Expr"):
        self.base_type = base_type
        self.predicate = predicate

    def __repr__(self) -> str:
        return f"RefinementType({self.base_type!r})"


# ---------------------------------------------------------------------------
# AST nodes
# ---------------------------------------------------------------------------

class ASTNode:
    pass


class Expr(ASTNode):
    """Base class for all expressions (used by idris_types.py)."""
    pass


class Module(ASTNode):
    def __init__(self, name: str, body: list[ASTNode]):
        self.name = name
        self.body = body


class Import(ASTNode):
    def __init__(self, name: str):
        self.name = name


class Declaration(ASTNode):
    pass


class TypeDeclaration(Declaration):
    def __init__(self, name: str, type_sig: ASTNode):
        self.name = name
        self.type_sig = type_sig


class FunctionDeclaration(Declaration):
    def __init__(self, name: str, args: list[str], body: ASTNode):
        self.name = name
        self.args = args
        self.body = body


class DataConstructor(Declaration):
    """Data constructor definition."""
    def __init__(self, name: str, type_sig: Type):
        self.name = name
        self.type_sig = type_sig


class Expression(Expr):
    pass


class Var(Expr):
    """Variable reference (used by type system)."""
    def __init__(self, name: str):
        self.name = name


class Identifier(Expression):
    def __init__(self, name: str):
        self.name = name


class Literal(Expression):
    def __init__(self, value):
        self.value = value


class FunctionApplication(Expression):
    def __init__(self, func: Expression, args: list[Expression]):
        self.func = func
        self.args = args


# Alias for type system compatibility
Application = FunctionApplication


class Lambda(Expr):
    """Lambda expression."""
    def __init__(self, param: str, param_type: Type | None, body: Expr):
        self.param = param
        self.param_type = param_type
        self.body = body


class Let(Expression):
    def __init__(self, bindings: list[Declaration], body: Expression):
        self.bindings = bindings
        self.body = body


# Alias for type system compatibility
LetExpr = Let


class Case(Expression):
    def __init__(self, expr: Expression, alternatives: list[ASTNode]):
        self.expr = expr
        self.alternatives = alternatives


# Alias for type system compatibility
CaseExpr = Case


class IfExpr(Expr):
    """If-then-else expression."""
    def __init__(self, condition: Expr, then_expr: Expr, else_expr: Expr):
        self.condition = condition
        self.then_expr = then_expr
        self.else_expr = else_expr


class ProofExpr(Expr):
    """Proof term (refl, cong, etc.)."""
    def __init__(self, name: str, args: list[Expr]):
        self.name = name
        self.args = args
