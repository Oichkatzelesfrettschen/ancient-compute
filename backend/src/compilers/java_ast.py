"""
Java AST - Abstract Syntax Tree nodes for Java

Represents:
  - Types: primitive, reference, generic, array
  - Expressions: literals, variables, operators, method calls
  - Statements: control flow, declarations
  - Declarations: classes, methods, fields
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Union

# ==============================================================================
# TYPES
# ==============================================================================

@dataclass
class Type:
    """Base class for all types"""
    pass


@dataclass
class PrimitiveType(Type):
    """Primitive type: int, float, boolean, etc."""
    name: str


@dataclass
class ReferenceType(Type):
    """Reference type: class name, interface"""
    name: str
    type_args: list[Type] = None

    def __post_init__(self):
        if self.type_args is None:
            self.type_args = []


@dataclass
class ArrayType(Type):
    """Array type: int[], String[][]"""
    element_type: Type
    dimensions: int = 1


@dataclass
class TypeParameter(Type):
    """Type parameter: T, U, K, V"""
    name: str
    bounds: list[Type] = None

    def __post_init__(self):
        if self.bounds is None:
            self.bounds = []


@dataclass
class WildcardType(Type):
    """Wildcard type: ?, ? extends T, ? super T"""
    upper_bound: Type | None = None
    lower_bound: Type | None = None


# ==============================================================================
# EXPRESSIONS
# ==============================================================================

@dataclass
class Expr:
    """Base class for all expressions"""
    pass


@dataclass
class Literal(Expr):
    """Literal value: 42, "hello", true"""
    value: Union[int, float, str, bool, None]


@dataclass
class Variable(Expr):
    """Variable reference: x, count"""
    name: str


@dataclass
class FieldAccess(Expr):
    """Field access: obj.field"""
    object_expr: Expr
    field_name: str


@dataclass
class ArrayAccess(Expr):
    """Array access: arr[i]"""
    array_expr: Expr
    index_expr: Expr


@dataclass
class MethodCall(Expr):
    """Method call: obj.method(args) or method(args)"""
    object_expr: Expr | None
    method_name: str
    type_args: list[Type]
    arguments: list[Expr]


@dataclass
class NewExpression(Expr):
    """Object creation: new ClassName(args)"""
    type_: Type
    type_args: list[Type]
    arguments: list[Expr]


@dataclass
class ArrayCreation(Expr):
    """Array creation: new int[10] or new String[]{...}"""
    element_type: Type
    dimensions: list[Expr]
    initializer: list[Expr] | None = None


@dataclass
class BinaryOp(Expr):
    """Binary operation: a + b, a < b"""
    operator: str
    left: Expr
    right: Expr


@dataclass
class UnaryOp(Expr):
    """Unary operation: -a, !b, ++c"""
    operator: str
    operand: Expr
    prefix: bool = True


@dataclass
class ConditionalExpr(Expr):
    """Conditional expression: cond ? true_expr : false_expr"""
    condition: Expr
    true_expr: Expr
    false_expr: Expr


@dataclass
class CastExpr(Expr):
    """Cast expression: (Type) expr"""
    target_type: Type
    operand: Expr


@dataclass
class InstanceofExpr(Expr):
    """instanceof expression: expr instanceof Type"""
    operand: Expr
    type_: Type


@dataclass
class LambdaExpr(Expr):
    """Lambda expression: (x, y) -> x + y"""
    parameters: list[Parameter]
    body: Union[Expr, list[Stmt]]


@dataclass
class MethodReference(Expr):
    """Method reference: Object::toString or System.out::println"""
    receiver: Expr
    method_name: str


# ==============================================================================
# STATEMENTS
# ==============================================================================

@dataclass
class Stmt:
    """Base class for all statements"""
    pass


@dataclass
class ExprStmt(Stmt):
    """Expression statement: expr;"""
    expr: Expr


@dataclass
class BlockStmt(Stmt):
    """Block statement: { statements }"""
    statements: list[Stmt]


@dataclass
class VarDeclStmt(Stmt):
    """Variable declaration: int x = 5;"""
    name: str
    type_: Type
    initializer: Expr | None = None


@dataclass
class IfStmt(Stmt):
    """If statement: if (cond) then_stmt else else_stmt"""
    condition: Expr
    then_stmt: Stmt
    else_stmt: Stmt | None = None


@dataclass
class WhileStmt(Stmt):
    """While statement: while (cond) body"""
    condition: Expr
    body: Stmt


@dataclass
class DoWhileStmt(Stmt):
    """Do-while statement: do body while (cond)"""
    body: Stmt
    condition: Expr


@dataclass
class ForStmt(Stmt):
    """For statement: for (init; cond; update) body"""
    initializer: Union[VarDeclStmt, Expr] | None
    condition: Expr | None
    update: Expr | None
    body: Stmt


@dataclass
class EnhancedForStmt(Stmt):
    """Enhanced for statement: for (Type var : expr) body"""
    variable_type: Type
    variable_name: str
    iterable: Expr
    body: Stmt


@dataclass
class SwitchStmt(Stmt):
    """Switch statement: switch (expr) { case ... }"""
    expr: Expr
    cases: list[SwitchCase]
    default_case: list[Stmt] | None = None


@dataclass
class SwitchCase:
    """Single case in switch statement"""
    pattern: Expr  # Usually a literal
    statements: list[Stmt]


@dataclass
class BreakStmt(Stmt):
    """Break statement"""
    label: str | None = None


@dataclass
class ContinueStmt(Stmt):
    """Continue statement"""
    label: str | None = None


@dataclass
class ReturnStmt(Stmt):
    """Return statement: return expr;"""
    expr: Expr | None = None


@dataclass
class ThrowStmt(Stmt):
    """Throw statement: throw expr;"""
    expr: Expr


@dataclass
class TryStmt(Stmt):
    """Try-catch-finally statement"""
    try_block: BlockStmt
    catch_blocks: list[CatchBlock]
    finally_block: BlockStmt | None = None


@dataclass
class CatchBlock:
    """Single catch block in try-catch"""
    exception_type: Type
    variable_name: str
    block: BlockStmt


@dataclass
class SynchronizedStmt(Stmt):
    """Synchronized statement: synchronized (obj) { ... }"""
    monitor: Expr
    body: BlockStmt


@dataclass
class LabeledStmt(Stmt):
    """Labeled statement: label: stmt"""
    label: str
    stmt: Stmt


# ==============================================================================
# DECLARATIONS
# ==============================================================================

@dataclass
class Declaration:
    """Base class for all declarations"""
    pass


@dataclass
class Parameter:
    """Method/lambda parameter"""
    name: str
    type_: Type
    is_varargs: bool = False


@dataclass
class MethodDecl(Declaration):
    """Method declaration"""
    name: str
    return_type: Type
    parameters: list[Parameter]
    body: BlockStmt | None
    type_parameters: list[TypeParameter]
    modifiers: list[str]
    exceptions: list[Type]


@dataclass
class FieldDecl(Declaration):
    """Field declaration"""
    name: str
    type_: Type
    initializer: Expr | None
    modifiers: list[str]


@dataclass
class ConstructorDecl(Declaration):
    """Constructor declaration"""
    name: str
    parameters: list[Parameter]
    body: BlockStmt
    type_parameters: list[TypeParameter]
    modifiers: list[str]
    exceptions: list[Type]


@dataclass
class ClassDecl(Declaration):
    """Class declaration"""
    name: str
    type_parameters: list[TypeParameter]
    superclass: Type | None
    interfaces: list[Type]
    modifiers: list[str]
    members: list[Union[MethodDecl, FieldDecl, ConstructorDecl, ClassDecl]]


@dataclass
class InterfaceDecl(Declaration):
    """Interface declaration"""
    name: str
    type_parameters: list[TypeParameter]
    extends: list[Type]
    modifiers: list[str]
    members: list[Union[MethodDecl, FieldDecl]]


@dataclass
class EnumDecl(Declaration):
    """Enum declaration"""
    name: str
    interfaces: list[Type]
    modifiers: list[str]
    constants: list[EnumConstant]
    members: list[Union[MethodDecl, FieldDecl]]


@dataclass
class EnumConstant:
    """Single enum constant"""
    name: str
    arguments: list[Expr]


@dataclass
class AnnotationDecl(Declaration):
    """Annotation declaration"""
    name: str
    modifiers: list[str]
    members: list[AnnotationMember]


@dataclass
class AnnotationMember:
    """Single annotation member"""
    name: str
    type_: Type
    default_value: Expr | None


@dataclass
class ImportDecl(Declaration):
    """Import declaration"""
    name: str
    is_static: bool = False
    is_on_demand: bool = False


@dataclass
class PackageDecl(Declaration):
    """Package declaration"""
    name: str


@dataclass
class CompilationUnit:
    """Top-level compilation unit (file)"""
    package_decl: PackageDecl | None
    import_decls: list[ImportDecl]
    type_decls: list[Union[ClassDecl, InterfaceDecl, EnumDecl, AnnotationDecl]]
