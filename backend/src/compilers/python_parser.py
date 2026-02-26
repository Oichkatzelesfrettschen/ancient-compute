"""
Python Parser - Builds Abstract Syntax Tree from tokens

Uses recursive descent parsing with proper precedence handling.
Indentation-sensitive: uses INDENT/DEDENT tokens to track block structure.

Operator precedence (lowest to highest):
  1. or
  2. and
  3. not
  4. ==, !=, <, <=, >, >=
  5. +, -
  6. *, /, //, %
  7. ** (power)
  8. unary -, +
  9. [subscript], .attribute, (call)
"""

from __future__ import annotations

from backend.src.compilers.python_ast import (
    Assign,
    Attribute,
    BinOp,
    Break,
    Call,
    Constant,
    Continue,
    Expr,
    ExprStmt,
    For,
    FunctionDef,
    If,
    Module,
    Name,
    Pass,
    Return,
    Stmt,
    Subscript,
    UnaryOp,
    While,
)
from backend.src.compilers.python_lexer import Token, TokenType


class PythonParser:
    """Recursive descent parser for Python"""

    def __init__(self, tokens: list[Token]) -> None:
        """Initialize parser with token list"""
        self.tokens = tokens
        self.pos = 0
        self.current_token = tokens[0] if tokens else Token(TokenType.EOF, '', 0, 0)

    def parse(self) -> Module:
        """Parse module (top-level program)"""
        statements = []

        while self.current_token.type != TokenType.EOF:
            # Skip newlines at top level
            while self.current_token.type == TokenType.NEWLINE:
                self._advance()

            if self.current_token.type == TokenType.EOF:
                break

            stmt = self._parse_statement()
            if stmt:
                statements.append(stmt)

        return Module(body=statements)

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

    def _skip_newlines(self) -> None:
        """Skip NEWLINE tokens"""
        while self.current_token.type == TokenType.NEWLINE:
            self._advance()

    def _skip_indent_dedent(self) -> None:
        """Skip INDENT/DEDENT tokens"""
        while self.current_token.type in (TokenType.INDENT, TokenType.DEDENT):
            self._advance()

    def _parse_statement(self) -> Stmt | None:
        """Parse a single statement"""
        self._skip_newlines()

        if self.current_token.type == TokenType.DEF:
            return self._parse_function_def()
        elif self.current_token.type == TokenType.RETURN:
            return self._parse_return()
        elif self.current_token.type == TokenType.IF:
            return self._parse_if()
        elif self.current_token.type == TokenType.WHILE:
            return self._parse_while()
        elif self.current_token.type == TokenType.FOR:
            return self._parse_for()
        elif self.current_token.type == TokenType.PASS:
            self._advance()
            self._skip_to_end_of_line()
            return Pass()
        elif self.current_token.type == TokenType.BREAK:
            self._advance()
            self._skip_to_end_of_line()
            return Break()
        elif self.current_token.type == TokenType.CONTINUE:
            self._advance()
            self._skip_to_end_of_line()
            return Continue()
        else:
            return self._parse_simple_statement()

    def _parse_function_def(self) -> FunctionDef:
        """Parse function definition: def name(args): body"""
        self._expect(TokenType.DEF)

        name_token = self._expect(TokenType.IDENTIFIER)
        name = name_token.value

        self._expect(TokenType.LPAREN)

        # Parse parameters
        args = []
        while self.current_token.type != TokenType.RPAREN:
            arg_token = self._expect(TokenType.IDENTIFIER)
            args.append(arg_token.value)

            if self.current_token.type == TokenType.COMMA:
                self._advance()

        self._expect(TokenType.RPAREN)
        self._expect(TokenType.COLON)
        self._skip_newlines()
        self._expect(TokenType.INDENT)

        # Parse body
        body = self._parse_block()

        self._expect(TokenType.DEDENT)

        return FunctionDef(name=name, args=args, body=body)

    def _parse_return(self) -> Return:
        """Parse return statement"""
        self._expect(TokenType.RETURN)

        value = None
        if self.current_token.type not in (TokenType.NEWLINE, TokenType.EOF, TokenType.DEDENT):
            value = self._parse_expression()

        self._skip_to_end_of_line()
        return Return(value=value)

    def _parse_if(self) -> If:
        """Parse if/elif/else statement"""
        self._expect(TokenType.IF)

        test = self._parse_expression()
        self._expect(TokenType.COLON)
        self._skip_newlines()
        self._expect(TokenType.INDENT)

        body = self._parse_block()
        self._expect(TokenType.DEDENT)

        # Handle elif/else
        orelse = []
        while self.current_token.type in (TokenType.ELIF, TokenType.ELSE):
            if self.current_token.type == TokenType.ELIF:
                self._advance()
                elif_test = self._parse_expression()
                self._expect(TokenType.COLON)
                self._skip_newlines()
                self._expect(TokenType.INDENT)

                elif_body = self._parse_block()
                self._expect(TokenType.DEDENT)

                # Create nested If for elif
                orelse = [If(test=elif_test, body=elif_body, orelse=[])]
            else:  # else
                self._advance()
                self._expect(TokenType.COLON)
                self._skip_newlines()
                self._expect(TokenType.INDENT)

                orelse = self._parse_block()
                self._expect(TokenType.DEDENT)
                break

        return If(test=test, body=body, orelse=orelse)

    def _parse_while(self) -> While:
        """Parse while loop"""
        self._expect(TokenType.WHILE)

        test = self._parse_expression()
        self._expect(TokenType.COLON)
        self._skip_newlines()
        self._expect(TokenType.INDENT)

        body = self._parse_block()
        self._expect(TokenType.DEDENT)

        return While(test=test, body=body)

    def _parse_for(self) -> For:
        """Parse for loop: for target in iterable: body"""
        self._expect(TokenType.FOR)

        target_token = self._expect(TokenType.IDENTIFIER)
        target = target_token.value

        self._expect(TokenType.IN)
        iter_expr = self._parse_expression()

        self._expect(TokenType.COLON)
        self._skip_newlines()
        self._expect(TokenType.INDENT)

        body = self._parse_block()
        self._expect(TokenType.DEDENT)

        return For(target=target, iter=iter_expr, body=body)

    def _parse_block(self) -> list[Stmt]:
        """Parse a block of statements (after INDENT)"""
        statements = []

        while self.current_token.type != TokenType.DEDENT:
            self._skip_newlines()

            if self.current_token.type == TokenType.DEDENT:
                break

            stmt = self._parse_statement()
            if stmt:
                statements.append(stmt)

        return statements

    def _parse_simple_statement(self) -> Stmt | None:
        """Parse simple statement (assignment or expression)"""
        expr = self._parse_expression()

        if self.current_token.type == TokenType.EQUAL:
            # Assignment
            if not isinstance(expr, Name):
                raise SyntaxError(f"Can only assign to names, not {type(expr).__name__}")

            self._advance()
            value = self._parse_expression()
            self._skip_to_end_of_line()
            return Assign(target=expr.id, value=value)
        else:
            # Expression statement
            self._skip_to_end_of_line()
            return ExprStmt(value=expr)

    def _parse_expression(self) -> Expr:
        """Parse expression (lowest precedence)"""
        return self._parse_or()

    def _parse_or(self) -> Expr:
        """Parse logical or: expr or expr"""
        left = self._parse_and()

        while self.current_token.type == TokenType.OR:
            self._advance()
            right = self._parse_and()
            left = BinOp(left=left, op='or', right=right)

        return left

    def _parse_and(self) -> Expr:
        """Parse logical and: expr and expr"""
        left = self._parse_not()

        while self.current_token.type == TokenType.AND:
            self._advance()
            right = self._parse_not()
            left = BinOp(left=left, op='and', right=right)

        return left

    def _parse_not(self) -> Expr:
        """Parse logical not: not expr"""
        if self.current_token.type == TokenType.NOT:
            self._advance()
            operand = self._parse_not()
            return UnaryOp(op='not', operand=operand)

        return self._parse_comparison()

    def _parse_comparison(self) -> Expr:
        """Parse comparison: expr (==|!=|<|<=|>|>=) expr"""
        left = self._parse_additive()

        while self.current_token.type in (TokenType.EQUAL_EQUAL, TokenType.NOT_EQUAL,
                                          TokenType.LESS, TokenType.LESS_EQUAL,
                                          TokenType.GREATER, TokenType.GREATER_EQUAL):
            op_map = {
                TokenType.EQUAL_EQUAL: '==',
                TokenType.NOT_EQUAL: '!=',
                TokenType.LESS: '<',
                TokenType.LESS_EQUAL: '<=',
                TokenType.GREATER: '>',
                TokenType.GREATER_EQUAL: '>=',
            }
            op = op_map[self.current_token.type]
            self._advance()
            right = self._parse_additive()
            left = BinOp(left=left, op=op, right=right)

        return left

    def _parse_additive(self) -> Expr:
        """Parse addition/subtraction: expr (+|-) expr"""
        left = self._parse_multiplicative()

        while self.current_token.type in (TokenType.PLUS, TokenType.MINUS):
            op = self.current_token.value
            self._advance()
            right = self._parse_multiplicative()
            left = BinOp(left=left, op=op, right=right)

        return left

    def _parse_multiplicative(self) -> Expr:
        """Parse multiplication/division: expr (*|/|//|%) expr"""
        left = self._parse_power()

        while self.current_token.type in (TokenType.STAR, TokenType.SLASH,
                                          TokenType.DOUBLE_SLASH, TokenType.PERCENT):
            op_map = {
                TokenType.STAR: '*',
                TokenType.SLASH: '/',
                TokenType.DOUBLE_SLASH: '//',
                TokenType.PERCENT: '%',
            }
            op = op_map[self.current_token.type]
            self._advance()
            right = self._parse_power()
            left = BinOp(left=left, op=op, right=right)

        return left

    def _parse_power(self) -> Expr:
        """Parse exponentiation: expr ** expr (right-associative)"""
        left = self._parse_unary()

        if self.current_token.type == TokenType.POWER:
            self._advance()
            right = self._parse_power()  # Right-associative
            return BinOp(left=left, op='**', right=right)

        return left

    def _parse_unary(self) -> Expr:
        """Parse unary operations: -expr, +expr"""
        if self.current_token.type in (TokenType.MINUS, TokenType.PLUS):
            op = self.current_token.value
            self._advance()
            operand = self._parse_unary()
            return UnaryOp(op=op, operand=operand)

        return self._parse_postfix()

    def _parse_postfix(self) -> Expr:
        """Parse postfix operations: func(), obj[idx], obj.attr"""
        expr = self._parse_primary()

        while True:
            if self.current_token.type == TokenType.LPAREN:
                # Function call
                self._advance()
                args = []

                while self.current_token.type != TokenType.RPAREN:
                    args.append(self._parse_expression())
                    if self.current_token.type == TokenType.COMMA:
                        self._advance()

                self._expect(TokenType.RPAREN)

                if not isinstance(expr, Name):
                    raise SyntaxError("Can only call functions by name (for now)")

                expr = Call(func=expr.id, args=args)

            elif self.current_token.type == TokenType.LBRACKET:
                # Subscript
                self._advance()
                index = self._parse_expression()
                self._expect(TokenType.RBRACKET)
                expr = Subscript(value=expr, index=index)

            elif self.current_token.type == TokenType.DOT:
                # Attribute access
                self._advance()
                attr_token = self._expect(TokenType.IDENTIFIER)
                expr = Attribute(value=expr, attr=attr_token.value)

            else:
                break

        return expr

    def _parse_primary(self) -> Expr:
        """Parse primary expression: literal, name, (expr)"""
        if self.current_token.type == TokenType.NUMBER:
            value_str = self.current_token.value
            self._advance()

            if '.' in value_str:
                value = float(value_str)
            else:
                value = int(value_str)

            return Constant(value=value)

        elif self.current_token.type == TokenType.STRING:
            value = self.current_token.value
            self._advance()
            return Constant(value=value)

        elif self.current_token.type == TokenType.TRUE:
            self._advance()
            return Constant(value=True)

        elif self.current_token.type == TokenType.FALSE:
            self._advance()
            return Constant(value=False)

        elif self.current_token.type == TokenType.NONE:
            self._advance()
            return Constant(value=None)

        elif self.current_token.type == TokenType.IDENTIFIER:
            name = self.current_token.value
            self._advance()
            return Name(id=name)

        elif self.current_token.type == TokenType.LPAREN:
            self._advance()
            expr = self._parse_expression()
            self._expect(TokenType.RPAREN)
            return expr

        else:
            raise SyntaxError(
                f"Unexpected token {self.current_token.type} "
                f"at {self.current_token.line}:{self.current_token.column}"
            )

    def _skip_to_end_of_line(self) -> None:
        """Skip to end of line"""
        while self.current_token.type not in (TokenType.NEWLINE, TokenType.DEDENT,
                                               TokenType.EOF):
            self._advance()

        if self.current_token.type == TokenType.NEWLINE:
            self._advance()
