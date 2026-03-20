# Ancient Compute - LISP AST


class ASTNode:
    pass


class Symbol(ASTNode):
    def __init__(self, value: str) -> None:
        self.value = value

    def __repr__(self) -> str:
        return f"Symbol({self.value})"


class Number(ASTNode):
    def __init__(self, value: int | float) -> None:
        self.value = value

    def __repr__(self) -> str:
        return f"Number({self.value})"


class String(ASTNode):
    def __init__(self, value: str) -> None:
        self.value = value

    def __repr__(self) -> str:
        return f"String({self.value})"


class SExpression(ASTNode):
    def __init__(self, children: list[ASTNode]) -> None:
        self.children = children

    def __repr__(self) -> str:
        return f"SExpression({self.children})"


Atom = Symbol | Number | String
Expression = Atom | SExpression
