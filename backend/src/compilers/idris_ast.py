# Ancient Compute - IDRIS AST

from typing import List, Optional, Union


class ASTNode:
    pass


class Module(ASTNode):
    def __init__(self, name: str, body: List[ASTNode]):
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
    def __init__(self, name: str, args: List[str], body: ASTNode):
        self.name = name
        self.args = args
        self.body = body


class Expression(ASTNode):
    pass


class Identifier(Expression):
    def __init__(self, name: str):
        self.name = name


class Literal(Expression):
    def __init__(self, value):
        self.value = value


class FunctionApplication(Expression):
    def __init__(self, func: Expression, args: List[Expression]):
        self.func = func
        self.args = args


class Let(Expression):
    def __init__(self, bindings: List[Declaration], body: Expression):
        self.bindings = bindings
        self.body = body


class Case(Expression):
    def __init__(self, expr: Expression, alternatives: List[ASTNode]):
        self.expr = expr
        self.alternatives = alternatives
