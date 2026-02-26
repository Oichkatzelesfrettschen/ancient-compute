"""
Haskell Parser - Builds Abstract Syntax Tree from tokens

Uses recursive descent parsing with proper operator precedence.

Operator precedence (lowest to highest):
  1. let/in
  2. case/if
  3. ||  (logical or)
  4. &&  (logical and)
  5. ==, /=, <, <=, >, >=
  6. +, -
  7. *, /, %, ^
  8. unary -
  9. function application
  10. primary (literals, variables, parens)
"""

from __future__ import annotations

from backend.src.compilers.haskell_ast import (
    Application,
    BinOp,
    Case,
    CaseBranch,
    ClassDecl,
    DataConstructor,
    DataDecl,
    Expr,
    FunctionDef,
    FunctionEquation,
    IfThenElse,
    InstanceDecl,
    Lambda,
    Let,
    Literal,
    Module,
    Pattern,
    PatternConstructor,
    PatternList,
    PatternLiteral,
    PatternTuple,
    PatternVariable,
    Stmt,
    Tuple,
    TypeDecl,
    UnaryOp,
    Variable,
)
from backend.src.compilers.haskell_ast import List as HList
from backend.src.compilers.haskell_lexer import Token, TokenType


class HaskellParser:
    """Recursive descent parser for Haskell"""

    def __init__(self, tokens: list[Token]) -> None:
        """Initialize parser with token list"""
        self.tokens = tokens
        self.pos = 0
        self.current_token = tokens[0] if tokens else Token(TokenType.EOF, '', 0, 0)

    def parse(self) -> Module:
        """Parse module (top-level program)"""
        declarations = []

        while self.current_token.type != TokenType.EOF:
            # Skip newlines at top level
            while self.current_token.type == TokenType.NEWLINE:
                self._advance()

            if self.current_token.type == TokenType.EOF:
                break

            # Parse module declaration (optional)
            if self.current_token.type == TokenType.MODULE:
                self._advance()
                module_name_token = self._expect(TokenType.IDENTIFIER)
                module_name = module_name_token.value
                self._expect(TokenType.WHERE)
                self._skip_newlines()
            else:
                module_name = None

            # Parse declarations
            stmt = self._parse_declaration()
            if stmt:
                declarations.append(stmt)

        return Module(name=module_name if 'module_name' in locals() else None, declarations=declarations)

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

    def _parse_declaration(self) -> Stmt | None:
        """Parse top-level declaration"""
        self._skip_newlines()

        if self.current_token.type == TokenType.EOF:
            return None

        # Type declaration: name :: type
        if self.current_token.type == TokenType.IDENTIFIER and \
           self._peek().type == TokenType.DOUBLE_COLON:
            name_token = self._expect(TokenType.IDENTIFIER)
            self._expect(TokenType.DOUBLE_COLON)
            type_sig = self._parse_type_signature()
            self._skip_to_end_of_line()
            return TypeDecl(name=name_token.value, type_sig=type_sig)

        # Data declaration
        if self.current_token.type == TokenType.DATA:
            return self._parse_data_decl()

        # Class declaration
        if self.current_token.type == TokenType.CLASS:
            return self._parse_class_decl()

        # Instance declaration
        if self.current_token.type == TokenType.INSTANCE:
            return self._parse_instance_decl()

        # Function definition: name pattern ... = expr
        if self.current_token.type == TokenType.IDENTIFIER:
            return self._parse_function_def()

        self._skip_to_end_of_line()
        return None

    def _parse_data_decl(self) -> DataDecl:
        """Parse data type declaration"""
        self._expect(TokenType.DATA)
        name_token = self._expect(TokenType.IDENTIFIER)
        self._expect(TokenType.EQUALS)

        constructors = []
        while self.current_token.type == TokenType.IDENTIFIER:
            const_name_token = self._expect(TokenType.IDENTIFIER)
            fields = []

            # Parse constructor fields
            while self.current_token.type == TokenType.IDENTIFIER:
                field_token = self._expect(TokenType.IDENTIFIER)
                fields.append(field_token.value)

            constructors.append(DataConstructor(name=const_name_token.value, fields=fields))

            if self.current_token.type == TokenType.PIPE:
                self._advance()

        self._skip_to_end_of_line()
        return DataDecl(name=name_token.value, constructors=constructors)

    def _parse_class_decl(self) -> ClassDecl:
        """Parse type class declaration (simplified)"""
        self._expect(TokenType.CLASS)
        name_token = self._expect(TokenType.IDENTIFIER)
        self._expect(TokenType.WHERE)
        self._skip_newlines()
        self._expect(TokenType.INDENT)

        methods = []
        while self.current_token.type != TokenType.DEDENT:
            self._skip_newlines()
            if self.current_token.type == TokenType.DEDENT:
                break

            if self.current_token.type == TokenType.IDENTIFIER:
                method_name = self._expect(TokenType.IDENTIFIER).value
                self._expect(TokenType.DOUBLE_COLON)
                type_sig = self._parse_type_signature()
                methods.append((method_name, type_sig))

        self._expect(TokenType.DEDENT)
        return ClassDecl(name=name_token.value, methods=methods)

    def _parse_instance_decl(self) -> InstanceDecl:
        """Parse instance declaration (simplified)"""
        self._expect(TokenType.INSTANCE)
        class_name = self._expect(TokenType.IDENTIFIER).value
        type_name = self._expect(TokenType.IDENTIFIER).value
        self._expect(TokenType.WHERE)
        self._skip_newlines()

        # For now, skip the definitions
        self._skip_to_end_of_line()
        return InstanceDecl(class_name=class_name, type_name=type_name, definitions=[])

    def _parse_function_def(self) -> FunctionDef:
        """Parse function definition with multiple equations"""
        name_token = self._expect(TokenType.IDENTIFIER)
        name = name_token.value

        equations = []

        # Parse first equation
        patterns = self._parse_patterns()
        guard = None
        if self.current_token.type == TokenType.PIPE:
            self._advance()
            guard = self._parse_expression()
        self._expect(TokenType.EQUALS)
        body = self._parse_expression()
        equations.append(FunctionEquation(patterns=patterns, guard=guard, body=body))

        self._skip_to_end_of_line()

        # Parse additional equations (same function name)
        while self.current_token.type == TokenType.IDENTIFIER and \
              self.current_token.value == name:
            self._advance()
            patterns = self._parse_patterns()
            guard = None
            if self.current_token.type == TokenType.PIPE:
                self._advance()
                guard = self._parse_expression()
            self._expect(TokenType.EQUALS)
            body = self._parse_expression()
            equations.append(FunctionEquation(patterns=patterns, guard=guard, body=body))
            self._skip_to_end_of_line()

        return FunctionDef(name=name, equations=equations)

    def _parse_patterns(self) -> list[Pattern]:
        """Parse function argument patterns"""
        patterns = []

        while self.current_token.type not in (TokenType.PIPE, TokenType.EQUALS):
            patterns.append(self._parse_pattern())

        return patterns

    def _parse_pattern(self) -> Pattern:
        """Parse a single pattern"""
        if self.current_token.type == TokenType.NUMBER:
            value_token = self._expect(TokenType.NUMBER)
            return PatternLiteral(value=int(value_token.value) if '.' not in value_token.value else float(value_token.value))

        elif self.current_token.type == TokenType.STRING:
            value_token = self._expect(TokenType.STRING)
            return PatternLiteral(value=value_token.value)

        elif self.current_token.type == TokenType.CHAR:
            value_token = self._expect(TokenType.CHAR)
            return PatternLiteral(value=value_token.value)

        elif self.current_token.type == TokenType.LBRACKET:
            return self._parse_list_pattern()

        elif self.current_token.type == TokenType.LPAREN:
            return self._parse_tuple_pattern()

        elif self.current_token.type == TokenType.IDENTIFIER:
            # Could be variable or constructor
            name_token = self._expect(TokenType.IDENTIFIER)

            # Check if followed by patterns (constructor application)
            if self.current_token.type in (TokenType.IDENTIFIER, TokenType.NUMBER, TokenType.LBRACKET, TokenType.LPAREN):
                patterns = []
                while self.current_token.type not in (TokenType.PIPE, TokenType.EQUALS, TokenType.COMMA, TokenType.RPAREN, TokenType.RBRACKET):
                    patterns.append(self._parse_pattern())
                return PatternConstructor(name=name_token.value, patterns=patterns)
            else:
                return PatternVariable(name=name_token.value)

        else:
            raise SyntaxError(f"Unexpected token in pattern: {self.current_token.type}")

    def _parse_list_pattern(self) -> Pattern:
        """Parse list pattern: [p1, p2, ...] or [h|t]"""
        self._expect(TokenType.LBRACKET)
        patterns = []

        if self.current_token.type == TokenType.RBRACKET:
            self._expect(TokenType.RBRACKET)
            return PatternList(patterns=[])

        patterns.append(self._parse_pattern())

        tail = None
        while self.current_token.type == TokenType.COMMA:
            self._advance()
            if self.current_token.type != TokenType.RBRACKET:
                patterns.append(self._parse_pattern())

        if self.current_token.type == TokenType.CONS:
            self._advance()
            tail = self._parse_pattern()

        self._expect(TokenType.RBRACKET)
        return PatternList(patterns=patterns, tail=tail)

    def _parse_tuple_pattern(self) -> Pattern:
        """Parse tuple pattern: (p1, p2, ...)"""
        self._expect(TokenType.LPAREN)
        patterns = []

        if self.current_token.type != TokenType.RPAREN:
            patterns.append(self._parse_pattern())
            while self.current_token.type == TokenType.COMMA:
                self._advance()
                patterns.append(self._parse_pattern())

        self._expect(TokenType.RPAREN)
        return PatternTuple(patterns=patterns)

    def _parse_type_signature(self) -> str:
        """Parse type signature (simplified - returns as string)"""
        sig = ''
        while self.current_token.type not in (TokenType.NEWLINE, TokenType.EOF, TokenType.EQUALS):
            sig += self.current_token.value + ' '
            self._advance()
        return sig.strip()

    def _parse_expression(self) -> Expr:
        """Parse expression (lowest precedence)"""
        return self._parse_let()

    def _parse_let(self) -> Expr:
        """Parse let expression: let bindings in body"""
        if self.current_token.type == TokenType.LET:
            self._advance()

            bindings = []
            while True:
                name_token = self._expect(TokenType.IDENTIFIER)
                self._expect(TokenType.EQUALS)
                expr = self._parse_expression()
                bindings.append((name_token.value, expr))

                if self.current_token.type == TokenType.IN:
                    break
                elif self.current_token.type == TokenType.SEMICOLON:
                    self._advance()
                else:
                    break

            self._expect(TokenType.IN)
            body = self._parse_expression()
            return Let(bindings=bindings, body=body)

        return self._parse_case()

    def _parse_case(self) -> Expr:
        """Parse case expression: case expr of branches"""
        if self.current_token.type == TokenType.CASE:
            self._advance()
            expr = self._parse_expression()
            self._expect(TokenType.OF)
            self._skip_newlines()

            branches = []
            if self.current_token.type == TokenType.INDENT:
                self._advance()
                while self.current_token.type != TokenType.DEDENT:
                    self._skip_newlines()
                    if self.current_token.type == TokenType.DEDENT:
                        break

                    pattern = self._parse_pattern()
                    guard = None
                    if self.current_token.type == TokenType.PIPE:
                        self._advance()
                        guard = self._parse_expression()
                    self._expect(TokenType.ARROW)
                    body = self._parse_expression()
                    branches.append(CaseBranch(pattern=pattern, guard=guard, body=body))
                    self._skip_to_end_of_line()

                self._expect(TokenType.DEDENT)
            else:
                # Single-line case
                pattern = self._parse_pattern()
                self._expect(TokenType.ARROW)
                body = self._parse_expression()
                branches.append(CaseBranch(pattern=pattern, guard=None, body=body))

            return Case(expr=expr, branches=branches)

        return self._parse_if()

    def _parse_if(self) -> Expr:
        """Parse if-then-else expression"""
        if self.current_token.type == TokenType.IF:
            self._advance()
            condition = self._parse_expression()
            self._expect(TokenType.THEN)
            then_expr = self._parse_expression()
            self._expect(TokenType.ELSE)
            else_expr = self._parse_expression()
            return IfThenElse(condition=condition, then_expr=then_expr, else_expr=else_expr)

        return self._parse_lambda()

    def _parse_lambda(self) -> Expr:
        r"""Parse lambda expression: \x -> body"""
        if self.current_token.type == TokenType.BACKSLASH:
            self._advance()

            params = []
            while self.current_token.type == TokenType.IDENTIFIER:
                params.append(self._expect(TokenType.IDENTIFIER).value)

            self._expect(TokenType.ARROW)
            body = self._parse_expression()
            return Lambda(params=params, body=body)

        return self._parse_or()

    def _parse_or(self) -> Expr:
        """Parse logical or: expr || expr"""
        left = self._parse_and()

        # Haskell doesn't have || operator in same way, but keep for compatibility
        return left

    def _parse_and(self) -> Expr:
        """Parse logical and: expr && expr"""
        left = self._parse_comparison()

        return left

    def _parse_comparison(self) -> Expr:
        """Parse comparison: expr (==|/=|<|<=|>|>=) expr"""
        left = self._parse_additive()

        while self.current_token.type in (TokenType.EQUAL_EQUAL, TokenType.NOT_EQUAL,
                                          TokenType.LESS, TokenType.LESS_EQUAL,
                                          TokenType.GREATER, TokenType.GREATER_EQUAL):
            op_map = {
                TokenType.EQUAL_EQUAL: '==',
                TokenType.NOT_EQUAL: '/=',
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
        """Parse multiplication/division: expr (*|/|%) expr"""
        left = self._parse_power()

        while self.current_token.type in (TokenType.STAR, TokenType.SLASH, TokenType.PERCENT):
            op_map = {
                TokenType.STAR: '*',
                TokenType.SLASH: '/',
                TokenType.PERCENT: '%',
            }
            op = op_map[self.current_token.type]
            self._advance()
            right = self._parse_power()
            left = BinOp(left=left, op=op, right=right)

        return left

    def _parse_power(self) -> Expr:
        """Parse exponentiation: expr ^ expr (right-associative)"""
        left = self._parse_unary()

        if self.current_token.type == TokenType.POWER:
            self._advance()
            right = self._parse_power()  # Right-associative
            return BinOp(left=left, op='^', right=right)

        return left

    def _parse_unary(self) -> Expr:
        """Parse unary operations: -expr, +expr"""
        if self.current_token.type in (TokenType.MINUS, TokenType.PLUS):
            op = self.current_token.value
            self._advance()
            operand = self._parse_unary()
            return UnaryOp(op=op, operand=operand)

        return self._parse_application()

    def _parse_application(self) -> Expr:
        """Parse function application: func arg1 arg2 ..."""
        func = self._parse_primary()

        args = []
        while self.current_token.type not in (TokenType.NEWLINE, TokenType.EOF, TokenType.EQUALS,
                                               TokenType.THEN, TokenType.ELSE, TokenType.IN,
                                               TokenType.ARROW, TokenType.OF, TokenType.COMMA,
                                               TokenType.RPAREN, TokenType.RBRACKET,
                                               TokenType.PIPE, TokenType.SEMICOLON,
                                               TokenType.DEDENT):
            # Check if this looks like an argument
            if self.current_token.type in (TokenType.IDENTIFIER, TokenType.NUMBER, TokenType.STRING,
                                           TokenType.LPAREN, TokenType.LBRACKET):
                args.append(self._parse_primary())
            else:
                break

        if args:
            return Application(func=func, args=args)
        return func

    def _parse_primary(self) -> Expr:
        """Parse primary expression: literal, name, (expr), [list], etc."""
        if self.current_token.type == TokenType.NUMBER:
            value_str = self.current_token.value
            self._advance()

            if '.' in value_str:
                return Literal(value=float(value_str), type_name='float')
            else:
                return Literal(value=int(value_str), type_name='int')

        elif self.current_token.type == TokenType.STRING:
            value = self.current_token.value
            self._advance()
            return Literal(value=value, type_name='string')

        elif self.current_token.type == TokenType.CHAR:
            value = self.current_token.value
            self._advance()
            return Literal(value=value, type_name='char')

        elif self.current_token.type == TokenType.IDENTIFIER:
            name = self.current_token.value
            self._advance()
            return Variable(name=name)

        elif self.current_token.type == TokenType.LPAREN:
            self._advance()

            # Check for empty tuple ()
            if self.current_token.type == TokenType.RPAREN:
                self._advance()
                return Tuple(elements=[])

            expr = self._parse_expression()

            # Check for tuple
            if self.current_token.type == TokenType.COMMA:
                elements = [expr]
                while self.current_token.type == TokenType.COMMA:
                    self._advance()
                    elements.append(self._parse_expression())
                self._expect(TokenType.RPAREN)
                return Tuple(elements=elements)

            self._expect(TokenType.RPAREN)
            return expr

        elif self.current_token.type == TokenType.LBRACKET:
            self._advance()

            # Check for empty list []
            if self.current_token.type == TokenType.RBRACKET:
                self._advance()
                return HList(elements=[])

            elements = []
            elements.append(self._parse_expression())

            is_range = False
            while self.current_token.type in (TokenType.COMMA, TokenType.DOT):
                if self.current_token.type == TokenType.COMMA:
                    self._advance()
                    if self.current_token.type != TokenType.RBRACKET:
                        elements.append(self._parse_expression())
                elif self.current_token.type == TokenType.DOT:
                    # Check for range [a .. b]
                    if self._peek().type == TokenType.DOT:
                        self._advance()
                        self._advance()
                        elements.append(self._parse_expression())
                        is_range = True
                        break

            self._expect(TokenType.RBRACKET)
            return HList(elements=elements, is_range=is_range)

        else:
            raise SyntaxError(
                f"Unexpected token {self.current_token.type} "
                f"at {self.current_token.line}:{self.current_token.column}"
            )

    def _skip_to_end_of_line(self) -> None:
        """Skip to end of line"""
        while self.current_token.type not in (TokenType.NEWLINE, TokenType.DEDENT, TokenType.EOF):
            self._advance()

        if self.current_token.type == TokenType.NEWLINE:
            self._advance()
