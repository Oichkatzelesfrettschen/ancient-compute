"""
LISP Parser - Builds Abstract Syntax Tree from tokens

Uses recursive descent parsing for S-expressions.
Handles nested lists, special forms, and quoted expressions.

Grammar (simplified):
  expr := atom | symbol | list | quote | lambda | let | if
  atom := number | string
  list := '(' expr* ')'
  quote := ''' expr
  lambda := '(lambda' '(' symbol* ')' expr ')'
  let := '(let' '(' binding* ')' expr ')'
  binding := '(' symbol expr ')'
  if := '(if' expr expr expr ')'
"""

from __future__ import annotations
from typing import List, Optional
from backend.src.compilers.lisp_lexer import Token, TokenType, LISPLexer
from backend.src.compilers.lisp_ast import (
    Expr, Atom, Symbol, List as ListExpr, Quote, Quasiquote, Unquote,
    UnquoteSplicing, Lambda, LetBinding, IfExpr, CondExpr, CaseExpr,
    DefunExpr, BuiltinFunctionCall, FunctionCall, BUILTIN_FUNCTIONS
)


class LISPParser:
    """Recursive descent parser for LISP"""

    def __init__(self, tokens: List[Token]) -> None:
        """Initialize parser with token list"""
        self.tokens = tokens
        self.pos = 0
        self.current_token = tokens[0] if tokens else Token(TokenType.EOF, '', 0, 0)

    def parse(self) -> List[Expr]:
        """Parse entire source and return list of top-level expressions"""
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
        return Token(TokenType.EOF, '', 0, 0)

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

    def _parse_expr(self) -> Optional[Expr]:
        """Parse a single expression"""
        token = self.current_token

        # Number literal
        if token.type == TokenType.NUMBER:
            self._advance()
            try:
                if '.' in token.value:
                    return Atom(float(token.value))
                else:
                    return Atom(int(token.value))
            except ValueError:
                raise SyntaxError(f"Invalid number: {token.value}")

        # String literal
        if token.type == TokenType.STRING:
            self._advance()
            return Atom(token.value)

        # Symbol
        if token.type == TokenType.SYMBOL:
            self._advance()
            return Symbol(token.value)

        # Quote shorthand
        if token.type == TokenType.QUOTE:
            self._advance()
            expr = self._parse_expr()
            if expr is None:
                raise SyntaxError("Quote requires an expression")
            return Quote(expr)

        # Quasiquote shorthand
        if token.type == TokenType.QUASIQUOTE:
            self._advance()
            expr = self._parse_expr()
            if expr is None:
                raise SyntaxError("Quasiquote requires an expression")
            return Quasiquote(expr)

        # Unquote shorthand
        if token.type == TokenType.UNQUOTE:
            self._advance()
            expr = self._parse_expr()
            if expr is None:
                raise SyntaxError("Unquote requires an expression")
            return Unquote(expr)

        # Unquote-splicing shorthand
        if token.type == TokenType.UNQUOTE_SPLICING:
            self._advance()
            expr = self._parse_expr()
            if expr is None:
                raise SyntaxError("Unquote-splicing requires an expression")
            return UnquoteSplicing(expr)

        # List (function call, special form, or literal)
        if token.type == TokenType.LPAREN:
            return self._parse_list()

        # Keyword (special form as standalone)
        if token.type in (TokenType.IF, TokenType.LET, TokenType.DEFUN,
                         TokenType.LAMBDA, TokenType.COND, TokenType.CASE):
            raise SyntaxError(f"Unexpected keyword {token.value} at {token.line}:{token.column}")

        if token.type == TokenType.EOF:
            return None

        raise SyntaxError(f"Unexpected token {token.type} at {token.line}:{token.column}")

    def _parse_list(self) -> Expr:
        """Parse a list (function call, special form, or literal)"""
        self._expect(TokenType.LPAREN)

        # Empty list
        if self.current_token.type == TokenType.RPAREN:
            self._advance()
            return ListExpr([])

        # Check for special forms
        if self.current_token.type == TokenType.IF:
            return self._parse_if()

        if self.current_token.type == TokenType.LET:
            return self._parse_let()

        if self.current_token.type == TokenType.LAMBDA:
            return self._parse_lambda()

        if self.current_token.type == TokenType.DEFUN:
            return self._parse_defun()

        if self.current_token.type == TokenType.COND:
            return self._parse_cond()

        if self.current_token.type == TokenType.CASE:
            return self._parse_case()

        # Regular function call or list
        elements = []
        while self.current_token.type != TokenType.RPAREN:
            expr = self._parse_expr()
            if expr is None:
                raise SyntaxError(f"Unexpected EOF in list at {self.current_token.line}")
            elements.append(expr)

        self._expect(TokenType.RPAREN)

        # If first element is a symbol/builtin, it's a function call
        if elements:
            return ListExpr(elements)

        return ListExpr([])

    def _parse_if(self) -> IfExpr:
        """Parse if expression: (if condition then [else])"""
        self._expect(TokenType.IF)

        condition = self._parse_expr()
        if condition is None:
            raise SyntaxError("If requires a condition")

        then_expr = self._parse_expr()
        if then_expr is None:
            raise SyntaxError("If requires a then expression")

        else_expr = None
        if self.current_token.type != TokenType.RPAREN:
            else_expr = self._parse_expr()

        self._expect(TokenType.RPAREN)
        return IfExpr(condition, then_expr, else_expr)

    def _parse_let(self) -> LetBinding:
        """Parse let expression: (let ((var1 val1) (var2 val2)) body)"""
        self._expect(TokenType.LET)
        self._expect(TokenType.LPAREN)

        bindings = []
        while self.current_token.type != TokenType.RPAREN:
            self._expect(TokenType.LPAREN)

            # Binding name
            if self.current_token.type != TokenType.SYMBOL:
                raise SyntaxError(f"Expected symbol in let binding, got {self.current_token.type}")
            var_name = self.current_token.value
            self._advance()

            # Binding value
            value = self._parse_expr()
            if value is None:
                raise SyntaxError("Let binding requires a value expression")

            self._expect(TokenType.RPAREN)
            bindings.append((var_name, value))

        self._expect(TokenType.RPAREN)

        # Body expression
        body = self._parse_expr()
        if body is None:
            raise SyntaxError("Let requires a body expression")

        self._expect(TokenType.RPAREN)
        return LetBinding(bindings, body)

    def _parse_lambda(self) -> Lambda:
        """Parse lambda expression: (lambda (params...) body)"""
        self._expect(TokenType.LAMBDA)
        self._expect(TokenType.LPAREN)

        # Parameter list
        parameters = []
        while self.current_token.type != TokenType.RPAREN:
            if self.current_token.type != TokenType.SYMBOL:
                raise SyntaxError(f"Expected parameter name, got {self.current_token.type}")
            parameters.append(self.current_token.value)
            self._advance()

        self._expect(TokenType.RPAREN)

        # Body
        body = self._parse_expr()
        if body is None:
            raise SyntaxError("Lambda requires a body expression")

        self._expect(TokenType.RPAREN)
        return Lambda(parameters, body)

    def _parse_defun(self) -> DefunExpr:
        """Parse function definition: (defun name (params...) body)"""
        self._expect(TokenType.DEFUN)

        # Function name
        if self.current_token.type != TokenType.SYMBOL:
            raise SyntaxError(f"Expected function name, got {self.current_token.type}")
        func_name = self.current_token.value
        self._advance()

        self._expect(TokenType.LPAREN)

        # Parameter list
        parameters = []
        while self.current_token.type != TokenType.RPAREN:
            if self.current_token.type != TokenType.SYMBOL:
                raise SyntaxError(f"Expected parameter name, got {self.current_token.type}")
            parameters.append(self.current_token.value)
            self._advance()

        self._expect(TokenType.RPAREN)

        # Body
        body = self._parse_expr()
        if body is None:
            raise SyntaxError("Defun requires a body expression")

        self._expect(TokenType.RPAREN)
        return DefunExpr(func_name, parameters, body)

    def _parse_cond(self) -> CondExpr:
        """Parse cond expression: (cond (test1 result1) (test2 result2) ...)"""
        self._expect(TokenType.COND)

        branches = []
        while self.current_token.type != TokenType.RPAREN:
            self._expect(TokenType.LPAREN)

            # Condition
            condition = self._parse_expr()
            if condition is None:
                raise SyntaxError("Cond branch requires a condition")

            # Result
            result = self._parse_expr()
            if result is None:
                raise SyntaxError("Cond branch requires a result expression")

            self._expect(TokenType.RPAREN)
            branches.append((condition, result))

        self._expect(TokenType.RPAREN)
        return CondExpr(branches)

    def _parse_case(self) -> CaseExpr:
        """Parse case expression: (case expr (pattern1 result1) (pattern2 result2) ...)"""
        self._expect(TokenType.CASE)

        # Scrutinee (value being matched)
        scrutinee = self._parse_expr()
        if scrutinee is None:
            raise SyntaxError("Case requires a scrutinee expression")

        branches = []
        while self.current_token.type != TokenType.RPAREN:
            self._expect(TokenType.LPAREN)

            # Pattern
            pattern = self._parse_expr()
            if pattern is None:
                raise SyntaxError("Case branch requires a pattern")

            # Result
            result = self._parse_expr()
            if result is None:
                raise SyntaxError("Case branch requires a result expression")

            self._expect(TokenType.RPAREN)
            branches.append((pattern, result))

        self._expect(TokenType.RPAREN)
        return CaseExpr(scrutinee, branches)
