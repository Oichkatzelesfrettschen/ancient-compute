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
from typing import List, Optional, Union


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
    type_args: List[Type] = None

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
    bounds: List[Type] = None

    def __post_init__(self):
        if self.bounds is None:
            self.bounds = []


@dataclass
class WildcardType(Type):
    """Wildcard type: ?, ? extends T, ? super T"""

    upper_bound: Optional[Type] = None
    lower_bound: Optional[Type] = None


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

    object_expr: Optional[Expr]
    method_name: str
    type_args: List[Type]
    arguments: List[Expr]


@dataclass
class NewExpression(Expr):
    """Object creation: new ClassName(args)"""

    type_: Type
    type_args: List[Type]
    arguments: List[Expr]


@dataclass
class ArrayCreation(Expr):
    """Array creation: new int[10] or new String[]{...}"""

    element_type: Type
    dimensions: List[Expr]
    initializer: Optional[List[Expr]] = None


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

    parameters: List[Parameter]
    body: Union[Expr, List[Stmt]]


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

    statements: List[Stmt]


@dataclass
class VarDeclStmt(Stmt):
    """Variable declaration: int x = 5;"""

    name: str
    type_: Type
    initializer: Optional[Expr] = None


@dataclass
class IfStmt(Stmt):
    """If statement: if (cond) then_stmt else else_stmt"""

    condition: Expr
    then_stmt: Stmt
    else_stmt: Optional[Stmt] = None


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

    initializer: Optional[Union[VarDeclStmt, Expr]]
    condition: Optional[Expr]
    update: Optional[Expr]
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
    cases: List[SwitchCase]
    default_case: Optional[List[Stmt]] = None


@dataclass
class SwitchCase:
    """Single case in switch statement"""

    pattern: Expr  # Usually a literal
    statements: List[Stmt]


@dataclass
class BreakStmt(Stmt):
    """Break statement"""

    label: Optional[str] = None


@dataclass
class ContinueStmt(Stmt):
    """Continue statement"""

    label: Optional[str] = None


@dataclass
class ReturnStmt(Stmt):
    """Return statement: return expr;"""

    expr: Optional[Expr] = None


@dataclass
class ThrowStmt(Stmt):
    """Throw statement: throw expr;"""

    expr: Expr


@dataclass
class TryStmt(Stmt):
    """Try-catch-finally statement"""

    try_block: BlockStmt
    catch_blocks: List[CatchBlock]
    finally_block: Optional[BlockStmt] = None


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
    parameters: List[Parameter]
    body: Optional[BlockStmt]
    type_parameters: List[TypeParameter]
    modifiers: List[str]
    exceptions: List[Type]


@dataclass
class FieldDecl(Declaration):
    """Field declaration"""

    name: str
    type_: Type
    initializer: Optional[Expr]
    modifiers: List[str]


@dataclass
class ConstructorDecl(Declaration):
    """Constructor declaration"""

    name: str
    parameters: List[Parameter]
    body: BlockStmt
    type_parameters: List[TypeParameter]
    modifiers: List[str]
    exceptions: List[Type]


@dataclass
class ClassDecl(Declaration):
    """Class declaration"""

    name: str
    type_parameters: List[TypeParameter]
    superclass: Optional[Type]
    interfaces: List[Type]
    modifiers: List[str]
    members: List[Union[MethodDecl, FieldDecl, ConstructorDecl, ClassDecl]]


@dataclass
class InterfaceDecl(Declaration):
    """Interface declaration"""

    name: str
    type_parameters: List[TypeParameter]
    extends: List[Type]
    modifiers: List[str]
    members: List[Union[MethodDecl, FieldDecl]]


@dataclass
class EnumDecl(Declaration):
    """Enum declaration"""

    name: str
    interfaces: List[Type]
    modifiers: List[str]
    constants: List[EnumConstant]
    members: List[Union[MethodDecl, FieldDecl]]


@dataclass
class EnumConstant:
    """Single enum constant"""

    name: str
    arguments: List[Expr]


@dataclass
class AnnotationDecl(Declaration):
    """Annotation declaration"""

    name: str
    modifiers: List[str]
    members: List[AnnotationMember]


@dataclass
class AnnotationMember:
    """Single annotation member"""

    name: str
    type_: Type
    default_value: Optional[Expr]


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

    package_decl: Optional[PackageDecl]
    import_decls: List[ImportDecl]
    type_decls: List[Union[ClassDecl, InterfaceDecl, EnumDecl, AnnotationDecl]]
