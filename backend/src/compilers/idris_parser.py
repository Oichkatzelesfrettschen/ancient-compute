"""
IDRIS2 Parser - Builds Abstract Syntax Tree from tokens

Uses recursive descent parsing for IDRIS2 source code.
Handles dependent types, data declarations, function definitions, and expressions.

Grammar (simplified):
  module := 'module' name imports* declaration*
  declaration := type_decl | function_def | data_def | type_def
  type_decl := name ':' type
  function_def := name params ':' type '=' expr
  data_def := 'data' name params ':' type 'where' constructor*
  expr := atom | var | lambda | application | case | if | let | proof
  type := base | function_type | dependent_type | type_family
  pattern := var_pattern | literal_pattern | constructor_pattern | wildcard
"""

from __future__ import annotations
from typing import List, Optional, Tuple
from backend.src.compilers.idris_lexer import Token, TokenType, IDRIS2Lexer
from backend.src.compilers.idris_ast import (
    Type, BaseType, TypeVariable, FunctionType, DependentType, TypeFamily, RefinementType,
    Expr, Var, Literal, Lambda, Application, LetExpr, CaseExpr, IfExpr, ProofExpr,
    DataConstructor,
    Pattern, VarPattern, LiteralPattern, ConstructorPattern, WildcardPattern,
    Declaration, TypeDeclaration, FunctionDef, DataDef, TypeDef,
    Module
)


class IDRIS2Parser:
    """Recursive descent parser for IDRIS2"""

    def __init__(self, tokens: List[Token]) -> None:
        """Initialize parser with token list"""
        self.tokens = tokens
        self.pos = 0
        self.current_token = tokens[0] if tokens else Token(TokenType.EOF, '', 0, 0)

    def parse(self) -> Module:
        """Parse entire source and return Module"""
        module_name = "main"
        imports = []
        declarations = []

        # Parse module declaration if present
        if self.current_token.type == TokenType.MODULE:
            self._advance()
            module_name = self._expect(TokenType.IDENTIFIER).value

        # Parse imports
        while self.current_token.type == TokenType.IMPORT:
            self._advance()
            import_name = self._parse_qualified_name()
            imports.append(import_name)

        # Parse declarations
        while self.current_token.type != TokenType.EOF:
            decl = self._parse_declaration()
            if decl:
                declarations.append(decl)

        return Module(module_name, imports, declarations)

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

    def _match(self, *token_types: TokenType) -> bool:
        """Check if current token matches any of the given types"""
        return self.current_token.type in token_types

    def _consume(self, token_type: TokenType) -> bool:
        """Consume token if it matches, return True if matched"""
        if self.current_token.type == token_type:
            self._advance()
            return True
        return False

    def _parse_qualified_name(self) -> str:
        """Parse qualified name (e.g., Data.List.head)"""
        parts = []
        parts.append(self._expect(TokenType.IDENTIFIER).value)

        while self._consume(TokenType.DOT):
            parts.append(self._expect(TokenType.IDENTIFIER).value)

        return ".".join(parts)

    def _parse_declaration(self) -> Optional[Declaration]:
        """Parse a top-level declaration"""
        if self.current_token.type == TokenType.DATA:
            return self._parse_data_def()

        if self.current_token.type == TokenType.TYPE:
            return self._parse_type_def()

        if self.current_token.type == TokenType.EXPORT:
            self._advance()
            decl = self._parse_declaration()
            if decl:
                decl.is_public = True
            return decl

        if self.current_token.type == TokenType.IDENTIFIER:
            return self._parse_function_or_type_decl()

        return None

    def _parse_function_or_type_decl(self) -> Optional[Declaration]:
        """Parse function definition or type declaration"""
        name = self._expect(TokenType.IDENTIFIER).value

        # Check for type declaration (name : type)
        if self._consume(TokenType.COLON):
            type_annotation = self._parse_type()

            # Check for function definition body (name : type = expr)
            if self._consume(TokenType.EQUALS):
                parameters = []
                body = self._parse_expr()
                return FunctionDef(name, type_annotation, parameters, body)

            # Just type declaration
            return TypeDeclaration(name, type_annotation)

        return None

    def _parse_data_def(self) -> DataDef:
        """Parse data type definition"""
        self._expect(TokenType.DATA)
        name = self._expect(TokenType.IDENTIFIER).value

        # Parse type parameters
        type_parameters = []
        while self._match(TokenType.LPAREN):
            type_parameters.extend(self._parse_type_parameters())

        # Parse return type (: Type, : Type -> Type, etc.)
        self._expect(TokenType.COLON)
        _return_type = self._parse_type()

        # Parse constructors
        constructors = []
        if self._consume(TokenType.WHERE):
            while self._match(TokenType.IDENTIFIER):
                constructor_name = self._expect(TokenType.IDENTIFIER).value
                self._expect(TokenType.COLON)
                constructor_type = self._parse_type()
                constructors.append((constructor_name, constructor_type))

        return DataDef(name, type_parameters, constructors)

    def _parse_type_def(self) -> TypeDef:
        """Parse type synonym definition"""
        self._expect(TokenType.TYPE)
        name = self._expect(TokenType.IDENTIFIER).value

        # Parse type parameters
        type_params = []
        while self._match(TokenType.IDENTIFIER):
            param = self._expect(TokenType.IDENTIFIER).value
            type_params.append(param)

        self._expect(TokenType.EQUALS)
        body = self._parse_type()

        return TypeDef(name, type_params, body)

    def _parse_type_parameters(self) -> List[Tuple[str, Type]]:
        """Parse type parameters in parentheses"""
        self._expect(TokenType.LPAREN)
        params = []

        while not self._match(TokenType.RPAREN):
            param_name = self._expect(TokenType.IDENTIFIER).value
            self._expect(TokenType.COLON)
            param_type = self._parse_type()
            params.append((param_name, param_type))

            if not self._consume(TokenType.COMMA):
                break

        self._expect(TokenType.RPAREN)
        return params

    def _parse_type(self) -> Type:
        """Parse a type expression"""
        return self._parse_function_type()

    def _parse_function_type(self) -> Type:
        """Parse function type (a -> b or (x : a) -> b)"""
        left = self._parse_dependent_type()

        if self._consume(TokenType.ARROW):
            right = self._parse_function_type()
            return FunctionType(left, right)

        return left

    def _parse_dependent_type(self) -> Type:
        """Parse dependent type ((x : a) -> b) or simple type"""
        if self._match(TokenType.LPAREN):
            self._advance()

            # Check for dependent type syntax (x : type) or implicit {x : type}
            is_implicit = False
            if self._match(TokenType.LBRACE):
                is_implicit = True
                self._advance()

            if self._match(TokenType.IDENTIFIER) and self._peek().type == TokenType.COLON:
                param_name = self._expect(TokenType.IDENTIFIER).value
                self._expect(TokenType.COLON)
                param_type = self._parse_type()

                if is_implicit:
                    self._expect(TokenType.RBRACE)
                else:
                    self._expect(TokenType.RPAREN)

                # Parse return type
                self._expect(TokenType.ARROW)
                return_type = self._parse_dependent_type()

                return DependentType(param_name, param_type, return_type, is_implicit)

            # Not a dependent type; parse as expression in parens
            self.pos -= 1  # Back up
            self.current_token = self.tokens[self.pos]
            return self._parse_primary_type()

        return self._parse_primary_type()

    def _parse_primary_type(self) -> Type:
        """Parse primary type (base, variable, family)"""
        if self._match(TokenType.LPAREN):
            self._advance()
            type_expr = self._parse_type()
            self._expect(TokenType.RPAREN)
            return type_expr

        if self._match(TokenType.IDENTIFIER):
            name = self._expect(TokenType.IDENTIFIER).value

            # Check if it's a type family (e.g., Vect n a)
            if self._match(TokenType.IDENTIFIER, TokenType.NUMBER, TokenType.LPAREN):
                type_args = []
                while self._match(TokenType.IDENTIFIER, TokenType.NUMBER, TokenType.LPAREN):
                    type_args.append(self._parse_primary_type())
                    if not self._match(TokenType.IDENTIFIER, TokenType.NUMBER, TokenType.LPAREN):
                        break

                return TypeFamily(name, type_args)

            # Check for built-in base types
            if name in ('Nat', 'Int', 'String', 'Bool', 'Char', 'Double', 'Float', 'Type'):
                return BaseType(name)

            # Otherwise it's a type variable
            return TypeVariable(name)

        raise SyntaxError(
            f"Expected type, got {self.current_token.type} "
            f"at {self.current_token.line}:{self.current_token.column}"
        )

    def _parse_expr(self) -> Expr:
        """Parse an expression"""
        return self._parse_application()

    def _parse_application(self) -> Expr:
        """Parse function application (f x y z)"""
        func = self._parse_primary_expr()

        # Collect arguments
        args = []
        while self._match(TokenType.IDENTIFIER, TokenType.NUMBER, TokenType.STRING,
                           TokenType.LPAREN, TokenType.BACKSLASH, TokenType.IF,
                           TokenType.LET, TokenType.CASE):
            args.append(self._parse_primary_expr())

        if args:
            return Application(func, args)

        return func

    def _parse_primary_expr(self) -> Expr:
        """Parse primary expression"""
        token = self.current_token

        # Number literal
        if token.type == TokenType.NUMBER:
            self._advance()
            try:
                if '.' in token.value:
                    return Literal(float(token.value))
                else:
                    return Literal(int(token.value))
            except ValueError:
                raise SyntaxError(f"Invalid number: {token.value}")

        # String literal
        if token.type == TokenType.STRING:
            self._advance()
            return Literal(token.value)

        # Character literal
        if token.type == TokenType.CHAR:
            self._advance()
            return Literal(token.value)

        # Variable or constructor
        if token.type == TokenType.IDENTIFIER:
            name = self._expect(TokenType.IDENTIFIER).value
            # Capitalized = constructor, lowercase = variable
            if name and name[0].isupper():
                # Could be constructor with arguments
                return DataConstructor(name, [])
            else:
                return Var(name)

        # Type name (uppercase identifier)
        if token.type == TokenType.TYPE_NAME:
            name = self._expect(TokenType.TYPE_NAME).value
            return DataConstructor(name, [])

        # Lambda abstraction
        if token.type == TokenType.BACKSLASH:
            return self._parse_lambda()

        # If expression
        if token.type == TokenType.IF:
            return self._parse_if()

        # Let expression
        if token.type == TokenType.LET:
            return self._parse_let()

        # Case expression
        if token.type == TokenType.CASE:
            return self._parse_case()

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

    def _parse_lambda(self) -> Lambda:
        """Parse lambda expression: \\x => expr or \\x : type => expr"""
        self._expect(TokenType.BACKSLASH)

        parameters = []

        # Parse parameter list (until =>)
        while not self._match(TokenType.DOUBLE_ARROW):
            param_name = self._expect(TokenType.IDENTIFIER).value
            param_type = TypeVariable("?")  # Implicit type

            # Optional type annotation
            if self._consume(TokenType.COLON):
                param_type = self._parse_type()

            parameters.append((param_name, param_type))

        self._expect(TokenType.DOUBLE_ARROW)
        body = self._parse_expr()

        return Lambda(parameters, body)

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
        """Parse let expression: let x = e1 in e2"""
        self._expect(TokenType.LET)

        bindings = []
        while not self._match(TokenType.IN):
            var_name = self._expect(TokenType.IDENTIFIER).value
            self._expect(TokenType.EQUALS)
            value_expr = self._parse_expr()
            var_type = TypeVariable("?")  # Inferred

            bindings.append((var_name, var_type, value_expr))

            if not self._match(TokenType.IN):
                # Optional semicolon between bindings
                self._consume(TokenType.SEMICOLON)

        self._expect(TokenType.IN)
        body = self._parse_expr()

        return LetExpr(bindings, body)

    def _parse_case(self) -> CaseExpr:
        """Parse case expression: case e of pattern1 => expr1 | pattern2 => expr2"""
        self._expect(TokenType.CASE)
        scrutinee = self._parse_expr()
        self._expect(TokenType.OF)

        branches = []
        while not self._match(TokenType.EOF):
            pattern = self._parse_pattern()
            self._expect(TokenType.FAT_ARROW)
            result_expr = self._parse_expr()
            branches.append((pattern, result_expr))

            if not self._consume(TokenType.PIPE):
                break

        return CaseExpr(scrutinee, branches)

    def _parse_pattern(self) -> Pattern:
        """Parse a pattern"""
        token = self.current_token

        # Wildcard pattern
        if token.type == TokenType.UNDERSCORE:
            self._advance()
            return WildcardPattern()

        # Number literal pattern
        if token.type == TokenType.NUMBER:
            self._advance()
            try:
                if '.' in token.value:
                    value = float(token.value)
                else:
                    value = int(token.value)
                return LiteralPattern(value)
            except ValueError:
                raise SyntaxError(f"Invalid number in pattern: {token.value}")

        # String literal pattern
        if token.type == TokenType.STRING:
            self._advance()
            return LiteralPattern(token.value)

        # Variable or constructor pattern
        if token.type == TokenType.IDENTIFIER:
            name = self._expect(TokenType.IDENTIFIER).value

            # Check if constructor pattern (with arguments)
            if self._match(TokenType.LPAREN):
                self._advance()
                arg_patterns = []

                while not self._match(TokenType.RPAREN):
                    arg_patterns.append(self._parse_pattern())
                    if not self._consume(TokenType.COMMA):
                        break

                self._expect(TokenType.RPAREN)
                return ConstructorPattern(name, arg_patterns)

            # Just variable pattern
            return VarPattern(name)

        # Type name pattern (uppercase)
        if token.type == TokenType.TYPE_NAME:
            name = self._expect(TokenType.TYPE_NAME).value
            arg_patterns = []

            if self._match(TokenType.LPAREN):
                self._advance()
                while not self._match(TokenType.RPAREN):
                    arg_patterns.append(self._parse_pattern())
                    if not self._consume(TokenType.COMMA):
                        break
                self._expect(TokenType.RPAREN)

            return ConstructorPattern(name, arg_patterns)

        # Parenthesized pattern
        if token.type == TokenType.LPAREN:
            self._advance()
            pattern = self._parse_pattern()
            self._expect(TokenType.RPAREN)
            return pattern

        raise SyntaxError(
            f"Expected pattern, got {self.current_token.type} "
            f"at {self.current_token.line}:{self.current_token.column}"
        )
