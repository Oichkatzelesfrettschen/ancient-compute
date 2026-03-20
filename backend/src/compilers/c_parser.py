"""C Parser -- recursive descent parser for the C freestanding subset.

Builds a C AST from a token stream produced by CLexer.
"""

from __future__ import annotations

from typing import Union

from .c_ast import (
    ArrayAccess,
    Assignment,
    BinaryOp,
    Block,
    Expression,
    ExpressionStatement,
    FloatLiteral,
    ForStatement,
    Function,
    FunctionCall,
    FunctionParameter,
    GlobalDeclaration,
    IfStatement,
    IntLiteral,
    Program,
    ReturnStatement,
    Statement,
    StringLiteral,
    Type,
    UnaryOp,
    Variable,
    VariableDeclaration,
    VariableRef,
    WhileStatement,
)
from .c_lexer import Token, TokenType


class CParser:
    """Recursive descent parser for C."""

    def __init__(self, tokens: list[Token]) -> None:
        """Initialize parser with token list."""
        self.tokens = tokens
        self.pos = 0

    def parse(self) -> Program:
        """Parse a C program."""
        program = Program()

        while not self._is_at_end():
            if (
                self._check(TokenType.INT)
                or self._check(TokenType.FLOAT)
                or self._check(TokenType.VOID)
            ):
                # Could be function or global variable
                decl = self._parse_declaration()
                if decl:
                    program.declarations.append(decl)
            else:
                self._advance()

        return program

    def _parse_declaration(self) -> Union[GlobalDeclaration, Function] | None:
        """Parse a declaration (function or global variable)."""
        ret_type = self._parse_type()
        if not ret_type:
            return None

        name = self._consume(TokenType.IDENT, "Expected identifier").value

        # Check if function or variable
        if self._check(TokenType.LPAREN):
            return self._parse_function(ret_type, name)
        else:
            return self._parse_global_variables(ret_type, name)

    def _parse_function(self, ret_type: Type, name: str) -> Function:
        """Parse a function declaration."""
        self._consume(TokenType.LPAREN, "Expected '('")

        params: list[FunctionParameter] = []
        if not self._check(TokenType.RPAREN):
            while True:
                param_type = self._parse_type()
                assert param_type is not None, "Expected parameter type"
                param_name = self._consume(TokenType.IDENT, "Expected parameter name").value
                params.append(FunctionParameter(param_name, param_type))

                if not self._check(TokenType.COMMA):
                    break
                self._advance()  # consume comma

        self._consume(TokenType.RPAREN, "Expected ')'")
        self._consume(TokenType.LBRACE, "Expected '{'")

        body = self._parse_block()

        return Function(name, ret_type, params, body)

    def _parse_global_variables(self, var_type: Type, first_name: str) -> GlobalDeclaration:
        """Parse global variable declarations."""
        variables: list[Variable] = []

        variables.append(Variable(first_name, var_type, is_global=True))

        while self._check(TokenType.COMMA):
            self._advance()  # consume comma
            name = self._consume(TokenType.IDENT, "Expected variable name").value
            variables.append(Variable(name, var_type, is_global=True))

        self._consume(TokenType.SEMICOLON, "Expected ';'")

        return GlobalDeclaration(variables)

    def _parse_block(self) -> Block:
        """Parse a block of statements."""
        statements: list[Statement] = []

        while not self._check(TokenType.RBRACE) and not self._is_at_end():
            stmt = self._parse_statement()
            if stmt:
                statements.append(stmt)

        self._consume(TokenType.RBRACE, "Expected '}'")

        return Block(statements)

    def _parse_statement(self) -> Statement | None:
        """Parse a single statement."""
        if self._check(TokenType.LBRACE):
            self._advance()
            return self._parse_block()

        if self._check(TokenType.IF):
            return self._parse_if()

        if self._check(TokenType.WHILE):
            return self._parse_while()

        if self._check(TokenType.FOR):
            return self._parse_for()

        if self._check(TokenType.RETURN):
            return self._parse_return()

        if (
            self._check(TokenType.INT)
            or self._check(TokenType.FLOAT)
            or self._check(TokenType.VOID)
        ):
            return self._parse_variable_declaration()

        # Expression statement
        expr = self._parse_expression()
        self._consume(TokenType.SEMICOLON, "Expected ';'")
        return ExpressionStatement(expr)

    def _parse_if(self) -> IfStatement:
        """Parse an if statement."""
        self._consume(TokenType.IF, "Expected 'if'")
        self._consume(TokenType.LPAREN, "Expected '('")
        condition = self._parse_expression()
        self._consume(TokenType.RPAREN, "Expected ')'")

        then_stmt = self._parse_statement()
        assert then_stmt is not None, "Expected then branch statement"
        else_stmt = None

        if self._check(TokenType.ELSE):
            self._advance()
            else_stmt = self._parse_statement()

        return IfStatement(condition, then_stmt, else_stmt)

    def _parse_while(self) -> WhileStatement:
        """Parse a while loop."""
        self._consume(TokenType.WHILE, "Expected 'while'")
        self._consume(TokenType.LPAREN, "Expected '('")
        condition = self._parse_expression()
        self._consume(TokenType.RPAREN, "Expected ')'")

        body = self._parse_statement()
        assert body is not None, "Expected while body statement"

        return WhileStatement(condition, body)

    def _parse_for_init_decl(self) -> Statement:
        """Parse a for-loop init declaration: type name [= expr] [, name [= expr]]*.

        Does NOT consume the trailing semicolon (that belongs to the for syntax).
        Returns a Block of VariableDeclaration + optional Assignment statements.
        """
        var_type = self._parse_type()
        assert var_type is not None, "Expected type in for-init declaration"

        stmts: list[Statement] = []

        while True:
            name = self._consume(TokenType.IDENT, "Expected variable name").value
            stmts.append(VariableDeclaration([Variable(name, var_type, is_global=False)]))
            if self._check(TokenType.ASSIGN):
                self._advance()  # consume '='
                init_expr = self._parse_expression()
                stmts.append(ExpressionStatement(Assignment(name, init_expr)))
            if not self._check(TokenType.COMMA):
                break
            self._advance()  # consume ','

        return Block(stmts) if len(stmts) > 1 else stmts[0]

    def _parse_for(self) -> ForStatement:
        """Parse a for loop."""
        self._consume(TokenType.FOR, "Expected 'for'")
        self._consume(TokenType.LPAREN, "Expected '('")

        init: Union[Expression, Statement, None] = None
        if not self._check(TokenType.SEMICOLON):
            if (
                self._check(TokenType.INT)
                or self._check(TokenType.FLOAT)
                or self._check(TokenType.VOID)
            ):
                init = self._parse_for_init_decl()
            else:
                init = self._parse_expression()
        self._consume(TokenType.SEMICOLON, "Expected ';'")

        condition = None
        if not self._check(TokenType.SEMICOLON):
            condition = self._parse_expression()
        self._consume(TokenType.SEMICOLON, "Expected ';'")

        increment = None
        if not self._check(TokenType.RPAREN):
            increment = self._parse_expression()
        self._consume(TokenType.RPAREN, "Expected ')'")

        body = self._parse_statement()
        assert body is not None, "Expected for body statement"

        return ForStatement(init, condition, increment, body)

    def _parse_return(self) -> ReturnStatement:
        """Parse a return statement."""
        self._consume(TokenType.RETURN, "Expected 'return'")

        value = None
        if not self._check(TokenType.SEMICOLON):
            value = self._parse_expression()

        self._consume(TokenType.SEMICOLON, "Expected ';'")

        return ReturnStatement(value)

    def _parse_variable_declaration(self) -> VariableDeclaration:
        """Parse a variable declaration."""
        var_type = self._parse_type()

        variables: list[Variable] = []
        name = self._consume(TokenType.IDENT, "Expected variable name").value
        assert var_type is not None, "Expected variable type"
        variables.append(Variable(name, var_type, is_global=False))

        while self._check(TokenType.COMMA):
            self._advance()
            name = self._consume(TokenType.IDENT, "Expected variable name").value
            variables.append(Variable(name, var_type, is_global=False))

        self._consume(TokenType.SEMICOLON, "Expected ';'")

        return VariableDeclaration(variables)

    def _parse_expression(self) -> Expression:
        """Parse an expression."""
        return self._parse_assignment()

    def _parse_assignment(self) -> Expression:
        """Parse assignment expression."""
        expr = self._parse_logical_or()

        if self._check(TokenType.ASSIGN):
            self._advance()
            if isinstance(expr, VariableRef):
                value = self._parse_assignment()
                return Assignment(expr.name, value)

        return expr

    def _parse_logical_or(self) -> Expression:
        """Parse logical OR expression."""
        expr = self._parse_logical_and()

        while self._check(TokenType.OR):
            op = self._advance().value
            right = self._parse_logical_and()
            expr = BinaryOp(op, expr, right)

        return expr

    def _parse_logical_and(self) -> Expression:
        """Parse logical AND expression."""
        expr = self._parse_equality()

        while self._check(TokenType.AND):
            op = self._advance().value
            right = self._parse_equality()
            expr = BinaryOp(op, expr, right)

        return expr

    def _parse_equality(self) -> Expression:
        """Parse equality expression."""
        expr = self._parse_comparison()

        while self._check(TokenType.EQ) or self._check(TokenType.NE):
            op = self._advance().value
            right = self._parse_comparison()
            expr = BinaryOp(op, expr, right)

        return expr

    def _parse_comparison(self) -> Expression:
        """Parse comparison expression."""
        expr = self._parse_additive()

        while (
            self._check(TokenType.LT)
            or self._check(TokenType.LE)
            or self._check(TokenType.GT)
            or self._check(TokenType.GE)
        ):
            op = self._advance().value
            right = self._parse_additive()
            expr = BinaryOp(op, expr, right)

        return expr

    def _parse_additive(self) -> Expression:
        """Parse additive expression."""
        expr = self._parse_multiplicative()

        while self._check(TokenType.PLUS) or self._check(TokenType.MINUS):
            op = self._advance().value
            right = self._parse_multiplicative()
            expr = BinaryOp(op, expr, right)

        return expr

    def _parse_multiplicative(self) -> Expression:
        """Parse multiplicative expression."""
        expr = self._parse_unary()

        while (
            self._check(TokenType.STAR)
            or self._check(TokenType.SLASH)
            or self._check(TokenType.PERCENT)
        ):
            op = self._advance().value
            right = self._parse_unary()
            expr = BinaryOp(op, expr, right)

        return expr

    def _parse_unary(self) -> Expression:
        """Parse unary expression."""
        if (
            self._check(TokenType.MINUS)
            or self._check(TokenType.NOT)
            or self._check(TokenType.AMPERSAND)
        ):
            op = self._advance().value
            expr = self._parse_unary()
            return UnaryOp(op, expr)

        return self._parse_postfix()

    def _parse_postfix(self) -> Expression:
        """Parse postfix expression."""
        expr = self._parse_primary()

        while True:
            if self._check(TokenType.LPAREN):
                # Function call
                self._advance()
                args: list[Expression] = []
                if not self._check(TokenType.RPAREN):
                    while True:
                        args.append(self._parse_expression())
                        if not self._check(TokenType.COMMA):
                            break
                        self._advance()
                self._consume(TokenType.RPAREN, "Expected ')'")

                if isinstance(expr, VariableRef):
                    expr = FunctionCall(expr.name, args)

            elif self._check(TokenType.LBRACKET):
                # Array access
                self._advance()
                index = self._parse_expression()
                self._consume(TokenType.RBRACKET, "Expected ']'")

                if isinstance(expr, VariableRef):
                    expr = ArrayAccess(expr.name, index)
            else:
                break

        return expr

    def _parse_primary(self) -> Expression:
        """Parse primary expression."""
        if self._check(TokenType.INT_LIT):
            value = int(self._advance().value)
            return IntLiteral(value)

        if self._check(TokenType.FLOAT_LIT):
            return FloatLiteral(float(self._advance().value))

        if self._check(TokenType.STR_LIT):
            return StringLiteral(self._advance().value)

        if self._check(TokenType.IDENT):
            name = self._advance().value
            return VariableRef(name)

        if self._check(TokenType.LPAREN):
            self._advance()
            expr = self._parse_expression()
            self._consume(TokenType.RPAREN, "Expected ')'")
            return expr

        raise Exception(f"Unexpected token: {self._peek()}")

    def _parse_type(self) -> Type | None:
        """Parse a type."""
        if self._check(TokenType.INT):
            self._advance()
            return Type("int")
        elif self._check(TokenType.FLOAT):
            self._advance()
            return Type("float")
        elif self._check(TokenType.VOID):
            self._advance()
            return Type("void")

        return None

    def _check(self, token_type: TokenType) -> bool:
        """Check if current token matches type."""
        if self._is_at_end():
            return False
        return self._peek().type == token_type

    def _advance(self) -> Token:
        """Consume and return current token."""
        if not self._is_at_end():
            self.pos += 1
        return self._previous()

    def _is_at_end(self) -> bool:
        """Check if at end of tokens."""
        return self._peek().type == TokenType.EOF

    def _peek(self) -> Token:
        """Return current token without consuming."""
        return self.tokens[self.pos]

    def _previous(self) -> Token:
        """Return previous token."""
        return self.tokens[self.pos - 1]

    def _consume(self, token_type: TokenType, message: str) -> Token:
        """Consume a token or raise error."""
        if self._check(token_type):
            return self._advance()

        current = self._peek()
        raise Exception(f"{message} at line {current.line}, col {current.col}")
