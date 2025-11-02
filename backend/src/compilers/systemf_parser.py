"""
System F Parser - Builds Abstract Syntax Tree from tokens

Uses recursive descent parsing for System F expressions.
Handles type abstractions, universal types, and polymorphic expressions.

Grammar:
  expr := lambda_expr | type_abstraction | primary_expr
  lambda_expr := '\\' identifier ':' type '=>' expr
  type_abstraction := '/\\' identifier '=>' expr
  primary_expr := application | atom
  application := primary_expr (primary_expr | '[' type ']')*
  atom := var | literal | '(' expr ')'
  type := universal_type | function_type | primary_type
  universal_type := 'forall' identifier '.' type
  function_type := primary_type '->' type
  primary_type := type_var | base_type | '(' type ')'
"""

from __future__ import annotations
from typing import List, Optional
from backend.src.compilers.systemf_lexer import Token, TokenType, SystemFLexer
from backend.src.compilers.systemf_ast import (
    Type,
    TypeVar,
    BaseType,
    FunctionType,
    UniversalType,
    Expr,
    Var,
    Literal,
    Lambda,
    TypeAbstraction,
    Application,
    TypeApplication,
    IfExpr,
    LetExpr,
    FixExpr,
    Annotation,
)


class SystemFParser:
    """Recursive descent parser for System F"""

    def __init__(self, tokens: List[Token]) -> None:
        """Initialize parser with token list"""
        self.tokens = tokens
        self.pos = 0
        self.current_token = tokens[0] if tokens else Token(TokenType.EOF, "", 0, 0)

    def parse(self) -> List[Expr]:
        """Parse entire source and return list of expressions"""
        expressions = []

        while self.current_token.type != TokenType.EOF:
            expr = self._parse_expr()
            if expr:
                expressions.append(expr)

        return expressions

    def _advance(self) -> None:
        """Move to next token"""
        if self.pos < len(self.tokens) - 1:
            self.pos += 1
            self.current_token = self.tokens[self.pos]

    def _peek(self, offset: int = 1) -> Token:
        """Peek at future token"""
        pos = self.pos + offset
        if pos < len(self.tokens):
            return self.tokens[pos]
        return Token(TokenType.EOF, "", 0, 0)

    def _expect(self, token_type: TokenType) -> Token:
        """Consume token of expected type or raise error"""
        if self.current_token.type != token_type:
            raise SyntaxError(
                f"Expected {token_type}, got {self.current_token.type} "
                f"at {self.current_token.line}:{self.current_token.column}"
            )
        token = self.current_token
        self._advance()
        return token

    def _match(self, *token_types: TokenType) -> bool:
        """Check if current token matches any of the given types"""
        return self.current_token.type in token_types

    def _consume(self, token_type: TokenType) -> bool:
        """Consume token if it matches, return True if matched"""
        if self.current_token.type == token_type:
            self._advance()
            return True
        return False

    def _parse_expr(self) -> Optional[Expr]:
        """Parse an expression"""
        # Lambda abstraction
        if self._match(TokenType.BACKSLASH):
            return self._parse_lambda()

        # Type abstraction
        if self._match(TokenType.TYPE_BACKSLASH):
            return self._parse_type_abstraction()

        # If expression
        if self._match(TokenType.IF):
            return self._parse_if()

        # Let expression
        if self._match(TokenType.LET):
            return self._parse_let()

        # Fix expression
        if self._match(TokenType.FIX):
            return self._parse_fix()

        # Application or primary expression
        return self._parse_application()

    def _parse_lambda(self) -> Lambda:
        """Parse lambda expression: \\x : T => expr"""
        self._expect(TokenType.BACKSLASH)
        param_name = self._expect(TokenType.IDENTIFIER).value
        self._expect(TokenType.COLON)
        param_type = self._parse_type()
        self._expect(TokenType.FAT_ARROW)
        body = self._parse_expr()

        return Lambda(param_name, param_type, body)

    def _parse_type_abstraction(self) -> TypeAbstraction:
        """Parse type abstraction: /\\a => expr"""
        self._expect(TokenType.TYPE_BACKSLASH)
        type_var = self._expect(TokenType.TYPE_VAR).value
        self._expect(TokenType.FAT_ARROW)
        body = self._parse_expr()

        return TypeAbstraction(type_var, body)

    def _parse_if(self) -> IfExpr:
        """Parse if expression: if cond then e1 else e2"""
        self._expect(TokenType.IF)
        condition = self._parse_expr()
        self._expect(TokenType.THEN)
        then_expr = self._parse_expr()
        self._expect(TokenType.ELSE)
        else_expr = self._parse_expr()

        return IfExpr(condition, then_expr, else_expr)

    def _parse_let(self) -> LetExpr:
        """Parse let expression: let x : T = e1 in e2"""
        self._expect(TokenType.LET)
        var_name = self._expect(TokenType.IDENTIFIER).value
        self._expect(TokenType.COLON)
        var_type = self._parse_type()
        self._expect(TokenType.EQUALS)
        value_expr = self._parse_expr()
        self._expect(TokenType.IN)
        body_expr = self._parse_expr()

        return LetExpr(var_name, var_type, value_expr, body_expr)

    def _parse_fix(self) -> FixExpr:
        """Parse fix expression: fix f"""
        self._expect(TokenType.FIX)
        func = self._parse_primary_expr()

        return FixExpr(func)

    def _parse_application(self) -> Expr:
        """Parse application (f x) or type application (f [T])"""
        expr = self._parse_primary_expr()

        while True:
            # Type application: f [T]
            if self._match(TokenType.LBRACKET):
                self._advance()
                type_arg = self._parse_type()
                self._expect(TokenType.RBRACKET)
                expr = TypeApplication(expr, type_arg)
            # Regular application: f x
            elif self._match(
                TokenType.IDENTIFIER, TokenType.NUMBER, TokenType.STRING_LIT, TokenType.LPAREN
            ):
                arg = self._parse_primary_expr()
                expr = Application(expr, arg)
            else:
                break

        return expr

    def _parse_primary_expr(self) -> Expr:
        """Parse primary expression (atom)"""
        token = self.current_token

        # Number literal
        if token.type == TokenType.NUMBER:
            self._advance()
            try:
                if "." in token.value:
                    return Literal(float(token.value))
                else:
                    return Literal(int(token.value))
            except ValueError:
                raise SyntaxError(f"Invalid number: {token.value}")

        # String literal
        if token.type == TokenType.STRING_LIT:
            self._advance()
            return Literal(token.value)

        # Variable
        if token.type == TokenType.IDENTIFIER:
            self._advance()
            return Var(token.value)

        # Parenthesized expression
        if token.type == TokenType.LPAREN:
            self._advance()
            expr = self._parse_expr()
            self._expect(TokenType.RPAREN)
            return expr

        raise SyntaxError(
            f"Expected expression, got {self.current_token.type} "
            f"at {self.current_token.line}:{self.current_token.column}"
        )

    def _parse_type(self) -> Type:
        """Parse a type expression"""
        return self._parse_universal_type()

    def _parse_universal_type(self) -> Type:
        """Parse universal type: forall a. T"""
        if self._consume(TokenType.FORALL):
            type_var = self._expect(TokenType.TYPE_VAR).value
            self._expect(TokenType.DOT)
            body = self._parse_universal_type()
            return UniversalType(type_var, body)

        return self._parse_function_type()

    def _parse_function_type(self) -> Type:
        """Parse function type: a -> b"""
        left = self._parse_primary_type()

        if self._consume(TokenType.ARROW):
            right = self._parse_function_type()
            return FunctionType(left, right)

        return left

    def _parse_primary_type(self) -> Type:
        """Parse primary type (type variable or base type)"""
        token = self.current_token

        # Type variable (single letter)
        if token.type == TokenType.TYPE_VAR:
            self._advance()
            return TypeVar(token.value)

        # Base types
        if token.type in (TokenType.NAT, TokenType.INT, TokenType.BOOL, TokenType.STRING):
            self._advance()
            return BaseType(token.value.lower())

        # Parenthesized type
        if token.type == TokenType.LPAREN:
            self._advance()
            type_expr = self._parse_type()
            self._expect(TokenType.RPAREN)
            return type_expr

        raise SyntaxError(
            f"Expected type, got {self.current_token.type} "
            f"at {self.current_token.line}:{self.current_token.column}"
        )
