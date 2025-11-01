"""
Java Parser - Builds Abstract Syntax Tree from tokens

Uses recursive descent parsing for Java syntax.
Handles classes, methods, expressions, statements.
"""

from __future__ import annotations
from typing import List, Optional
from backend.src.compilers.java_lexer import Token, TokenType, JavaLexer
from backend.src.compilers.java_ast import (
    Type, PrimitiveType, ReferenceType, ArrayType, TypeParameter, WildcardType,
    Expr, Literal, Variable, FieldAccess, ArrayAccess, MethodCall, NewExpression,
    ArrayCreation, BinaryOp, UnaryOp, ConditionalExpr, CastExpr, InstanceofExpr,
    LambdaExpr, MethodReference,
    Stmt, ExprStmt, BlockStmt, VarDeclStmt, IfStmt, WhileStmt, DoWhileStmt,
    ForStmt, EnhancedForStmt, SwitchStmt, SwitchCase, BreakStmt, ContinueStmt,
    ReturnStmt, ThrowStmt, TryStmt, CatchBlock, SynchronizedStmt, LabeledStmt,
    Parameter, MethodDecl, FieldDecl, ConstructorDecl, ClassDecl, InterfaceDecl,
    EnumDecl, EnumConstant, AnnotationDecl, AnnotationMember,
    ImportDecl, PackageDecl, CompilationUnit
)


class JavaParser:
    """Recursive descent parser for Java"""

    def __init__(self, tokens: List[Token]) -> None:
        self.tokens = tokens
        self.pos = 0
        self.current_token = tokens[0] if tokens else Token(TokenType.EOF, '', 0, 0)

    def parse(self) -> CompilationUnit:
        """Parse entire Java source file"""
        package_decl = None
        import_decls = []
        type_decls = []

        # Parse package declaration
        if self._match(TokenType.PACKAGE):
            package_decl = self._parse_package_decl()

        # Parse import declarations
        while self._match(TokenType.IMPORT):
            import_decls.append(self._parse_import_decl())

        # Parse type declarations (classes, interfaces, enums)
        while self.current_token.type != TokenType.EOF:
            if self._match(TokenType.PUBLIC, TokenType.PRIVATE, TokenType.PROTECTED,
                          TokenType.STATIC, TokenType.FINAL, TokenType.ABSTRACT,
                          TokenType.CLASS, TokenType.INTERFACE, TokenType.ENUM, TokenType.AT):
                type_decls.append(self._parse_type_decl())
            else:
                self._advance()

        return CompilationUnit(package_decl, import_decls, type_decls)

    def _advance(self) -> None:
        if self.pos < len(self.tokens) - 1:
            self.pos += 1
            self.current_token = self.tokens[self.pos]

    def _peek(self, offset: int = 1) -> Token:
        pos = self.pos + offset
        if pos < len(self.tokens):
            return self.tokens[pos]
        return Token(TokenType.EOF, '', 0, 0)

    def _match(self, *token_types: TokenType) -> bool:
        return self.current_token.type in token_types

    def _expect(self, token_type: TokenType) -> Token:
        if self.current_token.type != token_type:
            raise SyntaxError(f"Expected {token_type}, got {self.current_token.type} "
                            f"at line {self.current_token.line}")
        token = self.current_token
        self._advance()
        return token

    def _consume(self, token_type: TokenType) -> bool:
        if self.current_token.type == token_type:
            self._advance()
            return True
        return False

    # Declarations

    def _parse_package_decl(self) -> PackageDecl:
        self._expect(TokenType.PACKAGE)
        name = self._parse_name()
        self._expect(TokenType.SEMICOLON)
        return PackageDecl(name)

    def _parse_import_decl(self) -> ImportDecl:
        self._expect(TokenType.IMPORT)
        is_static = self._consume(TokenType.STATIC)
        name = self._parse_name()
        is_on_demand = self._consume(TokenType.STAR) if self.current_token.type == TokenType.DOT else False
        self._expect(TokenType.SEMICOLON)
        return ImportDecl(name, is_static, is_on_demand)

    def _parse_type_decl(self) -> object:
        """Parse class, interface, enum, or annotation"""
        modifiers = self._parse_modifiers()

        if self._match(TokenType.CLASS):
            return self._parse_class_decl(modifiers)
        elif self._match(TokenType.INTERFACE):
            return self._parse_interface_decl(modifiers)
        elif self._match(TokenType.ENUM):
            return self._parse_enum_decl(modifiers)
        elif self._match(TokenType.AT):
            return self._parse_annotation_decl(modifiers)
        else:
            raise SyntaxError(f"Expected type declaration, got {self.current_token.type}")

    def _parse_class_decl(self, modifiers: List[str]) -> ClassDecl:
        self._expect(TokenType.CLASS)
        # Accept both IDENTIFIER and TYPE_NAME (capitalized)
        if self._match(TokenType.IDENTIFIER, TokenType.TYPE_NAME):
            name = self.current_token.value
            self._advance()
        else:
            raise SyntaxError(f"Expected class name, got {self.current_token.type}")
        type_parameters = self._parse_type_parameters()

        superclass = None
        if self._consume(TokenType.EXTENDS):
            superclass = self._parse_type()

        interfaces = []
        if self._consume(TokenType.IMPLEMENTS):
            interfaces.append(self._parse_type())
            while self._consume(TokenType.COMMA):
                interfaces.append(self._parse_type())

        self._expect(TokenType.LBRACE)
        members = []
        while self.current_token.type != TokenType.RBRACE:
            members.append(self._parse_class_member())
        self._expect(TokenType.RBRACE)

        return ClassDecl(name, type_parameters, superclass, interfaces, modifiers, members)

    def _parse_interface_decl(self, modifiers: List[str]) -> InterfaceDecl:
        self._expect(TokenType.INTERFACE)
        # Accept both IDENTIFIER and TYPE_NAME (capitalized)
        if self._match(TokenType.IDENTIFIER, TokenType.TYPE_NAME):
            name = self.current_token.value
            self._advance()
        else:
            raise SyntaxError(f"Expected interface name, got {self.current_token.type}")
        type_parameters = self._parse_type_parameters()

        extends = []
        if self._consume(TokenType.EXTENDS):
            extends.append(self._parse_type())
            while self._consume(TokenType.COMMA):
                extends.append(self._parse_type())

        self._expect(TokenType.LBRACE)
        members = []
        while self.current_token.type != TokenType.RBRACE:
            members.append(self._parse_interface_member())
        self._expect(TokenType.RBRACE)

        return InterfaceDecl(name, type_parameters, extends, modifiers, members)

    def _parse_enum_decl(self, modifiers: List[str]) -> EnumDecl:
        self._expect(TokenType.ENUM)
        # Accept both IDENTIFIER and TYPE_NAME (capitalized)
        if self._match(TokenType.IDENTIFIER, TokenType.TYPE_NAME):
            name = self.current_token.value
            self._advance()
        else:
            raise SyntaxError(f"Expected enum name, got {self.current_token.type}")

        interfaces = []
        if self._consume(TokenType.IMPLEMENTS):
            interfaces.append(self._parse_type())
            while self._consume(TokenType.COMMA):
                interfaces.append(self._parse_type())

        self._expect(TokenType.LBRACE)
        constants = []
        while self.current_token.type == TokenType.IDENTIFIER:
            const_name = self._expect(TokenType.IDENTIFIER).value
            arguments = []
            if self._match(TokenType.LPAREN):
                arguments = self._parse_arguments()
            constants.append(EnumConstant(const_name, arguments))
            if not self._consume(TokenType.COMMA):
                break

        self._consume(TokenType.SEMICOLON)
        members = []
        while self.current_token.type != TokenType.RBRACE:
            members.append(self._parse_class_member())
        self._expect(TokenType.RBRACE)

        return EnumDecl(name, interfaces, modifiers, constants, members)

    def _parse_annotation_decl(self, modifiers: List[str]) -> AnnotationDecl:
        self._expect(TokenType.AT)
        self._expect(TokenType.INTERFACE)
        name = self._expect(TokenType.IDENTIFIER).value

        self._expect(TokenType.LBRACE)
        members = []
        while self.current_token.type != TokenType.RBRACE:
            member_modifiers = self._parse_modifiers()
            member_type = self._parse_type()
            member_name = self._expect(TokenType.IDENTIFIER).value
            self._expect(TokenType.LPAREN)
            self._expect(TokenType.RPAREN)
            default_value = None
            if self._consume(TokenType.DEFAULT):
                default_value = self._parse_expr()
            self._expect(TokenType.SEMICOLON)
            members.append(AnnotationMember(member_name, member_type, default_value))
        self._expect(TokenType.RBRACE)

        return AnnotationDecl(name, modifiers, members)

    def _parse_class_member(self) -> object:
        """Parse class member: field, method, or constructor"""
        modifiers = self._parse_modifiers()

        # Check for nested class/interface
        if self._match(TokenType.CLASS, TokenType.INTERFACE, TokenType.ENUM):
            return self._parse_type_decl()

        # Check for constructor (name matches class name)
        if self._match(TokenType.IDENTIFIER):
            # Could be field, method, or constructor
            # Peek ahead to distinguish
            if self._peek(1).type == TokenType.LPAREN:
                # Could be method or constructor
                name = self._expect(TokenType.IDENTIFIER).value
                type_parameters = self._parse_type_parameters()
                self._expect(TokenType.LPAREN)
                parameters = self._parse_parameters()
                self._expect(TokenType.RPAREN)
                exceptions = self._parse_throws()
                if self._match(TokenType.LBRACE):
                    body = self._parse_block_stmt()
                    return ConstructorDecl(name, parameters, body, type_parameters, modifiers, exceptions)
                else:
                    return MethodDecl(name, PrimitiveType("void"), parameters, None, type_parameters, modifiers, exceptions)
            else:
                # Field
                return self._parse_field_decl(modifiers)
        else:
            # Method with explicit return type
            return_type = self._parse_type()
            name = self._expect(TokenType.IDENTIFIER).value
            type_parameters = self._parse_type_parameters()
            self._expect(TokenType.LPAREN)
            parameters = self._parse_parameters()
            self._expect(TokenType.RPAREN)
            exceptions = self._parse_throws()
            if self._match(TokenType.LBRACE):
                body = self._parse_block_stmt()
            else:
                body = None
                self._expect(TokenType.SEMICOLON)
            return MethodDecl(name, return_type, parameters, body, type_parameters, modifiers, exceptions)

    def _parse_interface_member(self) -> object:
        """Parse interface member: method or field"""
        modifiers = self._parse_modifiers()
        return_type = self._parse_type()
        name = self._expect(TokenType.IDENTIFIER).value

        if self._match(TokenType.LPAREN):
            self._expect(TokenType.LPAREN)
            parameters = self._parse_parameters()
            self._expect(TokenType.RPAREN)
            exceptions = self._parse_throws()
            body = None
            if self._match(TokenType.LBRACE):
                body = self._parse_block_stmt()
            else:
                self._expect(TokenType.SEMICOLON)
            return MethodDecl(name, return_type, parameters, body, [], modifiers, exceptions)
        else:
            # Field
            initializer = None
            if self._consume(TokenType.EQUAL):
                initializer = self._parse_expr()
            self._expect(TokenType.SEMICOLON)
            return FieldDecl(name, return_type, initializer, modifiers)

    def _parse_field_decl(self, modifiers: List[str]) -> FieldDecl:
        field_type = self._parse_type()
        name = self._expect(TokenType.IDENTIFIER).value
        initializer = None
        if self._consume(TokenType.EQUAL):
            initializer = self._parse_expr()
        self._expect(TokenType.SEMICOLON)
        return FieldDecl(name, field_type, initializer, modifiers)

    # Statements

    def _parse_stmt(self) -> Stmt:
        """Parse any statement"""
        if self._match(TokenType.LBRACE):
            return self._parse_block_stmt()
        elif self._match(TokenType.VAR, TokenType.INT, TokenType.LONG, TokenType.FLOAT,
                         TokenType.DOUBLE, TokenType.BOOLEAN_TYPE, TokenType.BYTE,
                         TokenType.SHORT, TokenType.CHAR_TYPE, TokenType.STRING,
                         TokenType.IDENTIFIER, TokenType.TYPE_NAME):
            return self._parse_var_decl_stmt()
        elif self._match(TokenType.IF):
            return self._parse_if_stmt()
        elif self._match(TokenType.WHILE):
            return self._parse_while_stmt()
        elif self._match(TokenType.DO):
            return self._parse_do_while_stmt()
        elif self._match(TokenType.FOR):
            return self._parse_for_stmt()
        elif self._match(TokenType.SWITCH):
            return self._parse_switch_stmt()
        elif self._match(TokenType.BREAK):
            self._advance()
            self._expect(TokenType.SEMICOLON)
            return BreakStmt()
        elif self._match(TokenType.CONTINUE):
            self._advance()
            self._expect(TokenType.SEMICOLON)
            return ContinueStmt()
        elif self._match(TokenType.RETURN):
            return self._parse_return_stmt()
        elif self._match(TokenType.THROW):
            return self._parse_throw_stmt()
        elif self._match(TokenType.TRY):
            return self._parse_try_stmt()
        elif self._match(TokenType.SYNCHRONIZED):
            return self._parse_synchronized_stmt()
        else:
            expr = self._parse_expr()
            self._expect(TokenType.SEMICOLON)
            return ExprStmt(expr)

    def _parse_block_stmt(self) -> BlockStmt:
        self._expect(TokenType.LBRACE)
        statements = []
        while self.current_token.type != TokenType.RBRACE:
            statements.append(self._parse_stmt())
        self._expect(TokenType.RBRACE)
        return BlockStmt(statements)

    def _parse_var_decl_stmt(self) -> Stmt:
        var_type = self._parse_type()
        name = self._expect(TokenType.IDENTIFIER).value
        initializer = None
        if self._consume(TokenType.EQUAL):
            initializer = self._parse_expr()
        self._expect(TokenType.SEMICOLON)
        return VarDeclStmt(name, var_type, initializer)

    def _parse_if_stmt(self) -> IfStmt:
        self._expect(TokenType.IF)
        self._expect(TokenType.LPAREN)
        condition = self._parse_expr()
        self._expect(TokenType.RPAREN)
        then_stmt = self._parse_stmt()
        else_stmt = None
        if self._consume(TokenType.ELSE):
            else_stmt = self._parse_stmt()
        return IfStmt(condition, then_stmt, else_stmt)

    def _parse_while_stmt(self) -> WhileStmt:
        self._expect(TokenType.WHILE)
        self._expect(TokenType.LPAREN)
        condition = self._parse_expr()
        self._expect(TokenType.RPAREN)
        body = self._parse_stmt()
        return WhileStmt(condition, body)

    def _parse_do_while_stmt(self) -> DoWhileStmt:
        self._expect(TokenType.DO)
        body = self._parse_stmt()
        self._expect(TokenType.WHILE)
        self._expect(TokenType.LPAREN)
        condition = self._parse_expr()
        self._expect(TokenType.RPAREN)
        self._expect(TokenType.SEMICOLON)
        return DoWhileStmt(body, condition)

    def _parse_for_stmt(self) -> Stmt:
        self._expect(TokenType.FOR)
        self._expect(TokenType.LPAREN)

        # Check for enhanced for: for (Type var : expr)
        if self._match(TokenType.VAR, TokenType.INT, TokenType.LONG, TokenType.FLOAT,
                       TokenType.DOUBLE, TokenType.BOOLEAN_TYPE, TokenType.BYTE,
                       TokenType.SHORT, TokenType.CHAR_TYPE, TokenType.STRING,
                       TokenType.IDENTIFIER, TokenType.TYPE_NAME):
            checkpoint = self.pos
            var_type = self._parse_type()
            var_name = self._expect(TokenType.IDENTIFIER).value
            if self._match(TokenType.COLON):
                # Enhanced for
                self._advance()
                iterable = self._parse_expr()
                self._expect(TokenType.RPAREN)
                body = self._parse_stmt()
                return EnhancedForStmt(var_type, var_name, iterable, body)
            else:
                # Regular for with initializer; backtrack
                self.pos = checkpoint
                self.current_token = self.tokens[self.pos]

        initializer = None
        if not self._match(TokenType.SEMICOLON):
            if self._match(TokenType.VAR, TokenType.INT, TokenType.LONG, TokenType.FLOAT,
                           TokenType.DOUBLE, TokenType.BOOLEAN_TYPE, TokenType.BYTE,
                           TokenType.SHORT, TokenType.CHAR_TYPE, TokenType.STRING):
                initializer = self._parse_var_decl_stmt()
                # Remove the semicolon that was consumed
                # Already consumed in var decl
            else:
                initializer = self._parse_expr()
                self._expect(TokenType.SEMICOLON)
        else:
            self._expect(TokenType.SEMICOLON)

        condition = None
        if not self._match(TokenType.SEMICOLON):
            condition = self._parse_expr()
        self._expect(TokenType.SEMICOLON)

        update = None
        if not self._match(TokenType.RPAREN):
            update = self._parse_expr()
        self._expect(TokenType.RPAREN)

        body = self._parse_stmt()
        return ForStmt(initializer, condition, update, body)

    def _parse_switch_stmt(self) -> SwitchStmt:
        self._expect(TokenType.SWITCH)
        self._expect(TokenType.LPAREN)
        expr = self._parse_expr()
        self._expect(TokenType.RPAREN)
        self._expect(TokenType.LBRACE)

        cases = []
        default_case = None
        while self.current_token.type != TokenType.RBRACE:
            if self._match(TokenType.CASE):
                self._advance()
                pattern = self._parse_expr()
                self._expect(TokenType.COLON)
                statements = []
                while not self._match(TokenType.CASE, TokenType.DEFAULT, TokenType.RBRACE):
                    statements.append(self._parse_stmt())
                cases.append(SwitchCase(pattern, statements))
            elif self._match(TokenType.DEFAULT):
                self._advance()
                self._expect(TokenType.COLON)
                default_case = []
                while not self._match(TokenType.CASE, TokenType.DEFAULT, TokenType.RBRACE):
                    default_case.append(self._parse_stmt())
            else:
                break

        self._expect(TokenType.RBRACE)
        return SwitchStmt(expr, cases, default_case)

    def _parse_return_stmt(self) -> ReturnStmt:
        self._expect(TokenType.RETURN)
        expr = None
        if not self._match(TokenType.SEMICOLON):
            expr = self._parse_expr()
        self._expect(TokenType.SEMICOLON)
        return ReturnStmt(expr)

    def _parse_throw_stmt(self) -> ThrowStmt:
        self._expect(TokenType.THROW)
        expr = self._parse_expr()
        self._expect(TokenType.SEMICOLON)
        return ThrowStmt(expr)

    def _parse_try_stmt(self) -> TryStmt:
        self._expect(TokenType.TRY)
        try_block = self._parse_block_stmt()
        catch_blocks = []
        while self._match(TokenType.CATCH):
            self._advance()
            self._expect(TokenType.LPAREN)
            exc_type = self._parse_type()
            exc_var = self._expect(TokenType.IDENTIFIER).value
            self._expect(TokenType.RPAREN)
            block = self._parse_block_stmt()
            catch_blocks.append(CatchBlock(exc_type, exc_var, block))

        finally_block = None
        if self._consume(TokenType.FINALLY):
            finally_block = self._parse_block_stmt()

        return TryStmt(try_block, catch_blocks, finally_block)

    def _parse_synchronized_stmt(self) -> SynchronizedStmt:
        self._expect(TokenType.SYNCHRONIZED)
        self._expect(TokenType.LPAREN)
        monitor = self._parse_expr()
        self._expect(TokenType.RPAREN)
        body = self._parse_block_stmt()
        return SynchronizedStmt(monitor, body)

    # Expressions

    def _parse_expr(self) -> Expr:
        """Parse expression with operator precedence"""
        return self._parse_conditional()

    def _parse_conditional(self) -> Expr:
        expr = self._parse_logical_or()
        if self._consume(TokenType.QUESTION):
            true_expr = self._parse_expr()
            self._expect(TokenType.COLON)
            false_expr = self._parse_expr()
            return ConditionalExpr(expr, true_expr, false_expr)
        return expr

    def _parse_logical_or(self) -> Expr:
        expr = self._parse_logical_and()
        while self._match(TokenType.OR):
            op = self.current_token.value
            self._advance()
            right = self._parse_logical_and()
            expr = BinaryOp(op, expr, right)
        return expr

    def _parse_logical_and(self) -> Expr:
        expr = self._parse_bitwise_or()
        while self._match(TokenType.AND):
            op = self.current_token.value
            self._advance()
            right = self._parse_bitwise_or()
            expr = BinaryOp(op, expr, right)
        return expr

    def _parse_bitwise_or(self) -> Expr:
        expr = self._parse_bitwise_xor()
        while self._match(TokenType.BIT_OR):
            op = self.current_token.value
            self._advance()
            right = self._parse_bitwise_xor()
            expr = BinaryOp(op, expr, right)
        return expr

    def _parse_bitwise_xor(self) -> Expr:
        expr = self._parse_bitwise_and()
        while self._match(TokenType.BIT_XOR):
            op = self.current_token.value
            self._advance()
            right = self._parse_bitwise_and()
            expr = BinaryOp(op, expr, right)
        return expr

    def _parse_bitwise_and(self) -> Expr:
        expr = self._parse_equality()
        while self._match(TokenType.BIT_AND):
            op = self.current_token.value
            self._advance()
            right = self._parse_equality()
            expr = BinaryOp(op, expr, right)
        return expr

    def _parse_equality(self) -> Expr:
        expr = self._parse_relational()
        while self._match(TokenType.EQUAL_EQUAL, TokenType.NOT_EQUAL):
            op = self.current_token.value
            self._advance()
            right = self._parse_relational()
            expr = BinaryOp(op, expr, right)
        return expr

    def _parse_relational(self) -> Expr:
        expr = self._parse_shift()
        while self._match(TokenType.LESS, TokenType.GREATER, TokenType.LESS_EQUAL,
                          TokenType.GREATER_EQUAL, TokenType.INSTANCEOF):
            if self._match(TokenType.INSTANCEOF):
                self._advance()
                type_ = self._parse_type()
                expr = InstanceofExpr(expr, type_)
            else:
                op = self.current_token.value
                self._advance()
                right = self._parse_shift()
                expr = BinaryOp(op, expr, right)
        return expr

    def _parse_shift(self) -> Expr:
        expr = self._parse_additive()
        while self._match(TokenType.LSHIFT, TokenType.RSHIFT, TokenType.URSHIFT):
            op = self.current_token.value
            self._advance()
            right = self._parse_additive()
            expr = BinaryOp(op, expr, right)
        return expr

    def _parse_additive(self) -> Expr:
        expr = self._parse_multiplicative()
        while self._match(TokenType.PLUS, TokenType.MINUS):
            op = self.current_token.value
            self._advance()
            right = self._parse_multiplicative()
            expr = BinaryOp(op, expr, right)
        return expr

    def _parse_multiplicative(self) -> Expr:
        expr = self._parse_unary()
        while self._match(TokenType.STAR, TokenType.SLASH, TokenType.PERCENT):
            op = self.current_token.value
            self._advance()
            right = self._parse_unary()
            expr = BinaryOp(op, expr, right)
        return expr

    def _parse_unary(self) -> Expr:
        if self._match(TokenType.NOT, TokenType.BIT_NOT, TokenType.PLUS, TokenType.MINUS):
            op = self.current_token.value
            self._advance()
            expr = self._parse_unary()
            return UnaryOp(op, expr, prefix=True)
        elif self._match(TokenType.INCREMENT, TokenType.DECREMENT):
            op = self.current_token.value
            self._advance()
            expr = self._parse_postfix()
            return UnaryOp(op, expr, prefix=True)
        return self._parse_postfix()

    def _parse_postfix(self) -> Expr:
        expr = self._parse_primary()
        while True:
            if self._match(TokenType.DOT):
                self._advance()
                if self._match(TokenType.IDENTIFIER):
                    field_name = self._expect(TokenType.IDENTIFIER).value
                    if self._match(TokenType.LPAREN):
                        # Method call
                        type_args = self._parse_type_arguments()
                        args = self._parse_arguments()
                        expr = MethodCall(expr, field_name, type_args, args)
                    else:
                        expr = FieldAccess(expr, field_name)
                elif self._match(TokenType.NEW):
                    self._advance()
                    type_ = ReferenceType(self._expect(TokenType.TYPE_NAME).value)
                    args = self._parse_arguments()
                    expr = NewExpression(type_, [], args)
            elif self._match(TokenType.LBRACKET):
                self._advance()
                index = self._parse_expr()
                self._expect(TokenType.RBRACKET)
                expr = ArrayAccess(expr, index)
            elif self._match(TokenType.INCREMENT, TokenType.DECREMENT):
                op = self.current_token.value
                self._advance()
                expr = UnaryOp(op, expr, prefix=False)
            elif self._match(TokenType.DOUBLE_COLON):
                self._advance()
                method_name = self._expect(TokenType.IDENTIFIER).value
                expr = MethodReference(expr, method_name)
            else:
                break
        return expr

    def _parse_primary(self) -> Expr:
        """Parse primary expression"""
        if self._match(TokenType.NUMBER):
            value = self.current_token.value
            self._advance()
            try:
                if '.' in value:
                    return Literal(float(value))
                else:
                    return Literal(int(value))
            except ValueError:
                return Literal(value)

        elif self._match(TokenType.STRING_LIT):
            value = self._expect(TokenType.STRING_LIT).value
            return Literal(value)

        elif self._match(TokenType.CHAR_LIT):
            value = self._expect(TokenType.CHAR_LIT).value
            return Literal(value)

        elif self._match(TokenType.BOOLEAN):
            value = self._expect(TokenType.BOOLEAN).value == 'true'
            return Literal(value)

        elif self._match(TokenType.NULL):
            self._advance()
            return Literal(None)

        elif self._match(TokenType.IDENTIFIER):
            name = self._expect(TokenType.IDENTIFIER).value
            if self._match(TokenType.LPAREN):
                type_args = self._parse_type_arguments()
                args = self._parse_arguments()
                return MethodCall(None, name, type_args, args)
            return Variable(name)

        elif self._match(TokenType.THIS):
            self._advance()
            return Variable('this')

        elif self._match(TokenType.SUPER):
            self._advance()
            return Variable('super')

        elif self._match(TokenType.NEW):
            return self._parse_new_expr()

        elif self._match(TokenType.LPAREN):
            self._advance()
            # Could be cast or grouped expression
            checkpoint = self.pos
            # Try to parse as cast
            if self._match(TokenType.INT, TokenType.LONG, TokenType.FLOAT,
                           TokenType.DOUBLE, TokenType.BOOLEAN_TYPE, TokenType.BYTE,
                           TokenType.SHORT, TokenType.CHAR_TYPE, TokenType.STRING,
                           TokenType.IDENTIFIER, TokenType.TYPE_NAME):
                type_ = self._parse_type()
                if self._match(TokenType.RPAREN):
                    self._advance()
                    # Check if this looks like a cast
                    if self._match(TokenType.NUMBER, TokenType.IDENTIFIER, TokenType.LPAREN,
                                   TokenType.NEW, TokenType.THIS, TokenType.SUPER,
                                   TokenType.NOT, TokenType.MINUS, TokenType.PLUS):
                        expr = self._parse_unary()
                        return CastExpr(type_, expr)
            # Not a cast, parse as grouped expression
            self.pos = checkpoint
            self.current_token = self.tokens[self.pos]
            expr = self._parse_expr()
            self._expect(TokenType.RPAREN)
            return expr

        else:
            raise SyntaxError(f"Unexpected token: {self.current_token.type} "
                            f"at line {self.current_token.line}")

    def _parse_new_expr(self) -> NewExpression:
        self._expect(TokenType.NEW)
        type_args = self._parse_type_arguments()
        type_ = self._parse_type()
        args = self._parse_arguments()
        return NewExpression(type_, type_args, args)

    # Types

    def _parse_type(self) -> Type:
        """Parse any type"""
        base_type = self._parse_primary_type()
        while self._consume(TokenType.LBRACKET):
            self._expect(TokenType.RBRACKET)
            base_type = ArrayType(base_type)
        return base_type

    def _parse_primary_type(self) -> Type:
        """Parse base type (not array)"""
        if self._match(TokenType.INT):
            self._advance()
            return PrimitiveType("int")
        elif self._match(TokenType.LONG):
            self._advance()
            return PrimitiveType("long")
        elif self._match(TokenType.FLOAT):
            self._advance()
            return PrimitiveType("float")
        elif self._match(TokenType.DOUBLE):
            self._advance()
            return PrimitiveType("double")
        elif self._match(TokenType.BOOLEAN_TYPE):
            self._advance()
            return PrimitiveType("boolean")
        elif self._match(TokenType.BYTE):
            self._advance()
            return PrimitiveType("byte")
        elif self._match(TokenType.SHORT):
            self._advance()
            return PrimitiveType("short")
        elif self._match(TokenType.CHAR_TYPE):
            self._advance()
            return PrimitiveType("char")
        elif self._match(TokenType.VOID):
            self._advance()
            return PrimitiveType("void")
        elif self._match(TokenType.STRING):
            self._advance()
            return ReferenceType("String")
        elif self._match(TokenType.IDENTIFIER, TokenType.TYPE_NAME):
            name = self.current_token.value
            self._advance()
            type_args = []
            if self._match(TokenType.LESS):
                type_args = self._parse_type_arguments_inline()
            return ReferenceType(name, type_args)
        else:
            raise SyntaxError(f"Expected type, got {self.current_token.type}")

    def _parse_type_parameters(self) -> List[TypeParameter]:
        """Parse <T, U, V>"""
        parameters = []
        if self._match(TokenType.LESS):
            self._advance()
            while self.current_token.type != TokenType.GREATER:
                name = self._expect(TokenType.IDENTIFIER).value
                bounds = []
                if self._consume(TokenType.EXTENDS):
                    bounds.append(self._parse_type())
                    while self._consume(TokenType.BIT_AND):
                        bounds.append(self._parse_type())
                parameters.append(TypeParameter(name, bounds))
                if not self._consume(TokenType.COMMA):
                    break
            self._expect(TokenType.GREATER)
        return parameters

    def _parse_type_arguments(self) -> List[Type]:
        """Parse <T, U> from method call"""
        if self._match(TokenType.LESS):
            return self._parse_type_arguments_inline()
        return []

    def _parse_type_arguments_inline(self) -> List[Type]:
        """Parse <T, U> types"""
        args = []
        self._expect(TokenType.LESS)
        while self.current_token.type != TokenType.GREATER:
            if self._match(TokenType.QUESTION):
                self._advance()
                upper_bound = None
                lower_bound = None
                if self._consume(TokenType.EXTENDS):
                    upper_bound = self._parse_type()
                elif self._consume(TokenType.SUPER):
                    lower_bound = self._parse_type()
                args.append(WildcardType(upper_bound, lower_bound))
            else:
                args.append(self._parse_type())
            if not self._consume(TokenType.COMMA):
                break
        self._expect(TokenType.GREATER)
        return args

    # Helpers

    def _parse_modifiers(self) -> List[str]:
        """Parse access modifiers and other modifiers"""
        modifiers = []
        while self._match(TokenType.PUBLIC, TokenType.PRIVATE, TokenType.PROTECTED,
                          TokenType.STATIC, TokenType.FINAL, TokenType.ABSTRACT,
                          TokenType.NATIVE, TokenType.SYNCHRONIZED, TokenType.VOLATILE,
                          TokenType.TRANSIENT, TokenType.AT):
            if self._match(TokenType.AT):
                self._advance()
                # annotation
                modifiers.append('@' + self._expect(TokenType.IDENTIFIER).value)
            else:
                modifiers.append(self.current_token.value)
                self._advance()
        return modifiers

    def _parse_parameters(self) -> List[Parameter]:
        """Parse method parameters"""
        parameters = []
        while self.current_token.type != TokenType.RPAREN:
            param_type = self._parse_type()
            is_varargs = self._consume(TokenType.ELLIPSIS)
            param_name = self._expect(TokenType.IDENTIFIER).value
            parameters.append(Parameter(param_name, param_type, is_varargs))
            if not self._consume(TokenType.COMMA):
                break
        return parameters

    def _parse_arguments(self) -> List[Expr]:
        """Parse method/constructor arguments"""
        self._expect(TokenType.LPAREN)
        arguments = []
        while self.current_token.type != TokenType.RPAREN:
            arguments.append(self._parse_expr())
            if not self._consume(TokenType.COMMA):
                break
        self._expect(TokenType.RPAREN)
        return arguments

    def _parse_throws(self) -> List[Type]:
        """Parse throws clause"""
        exceptions = []
        if self._consume(TokenType.THROW):
            exceptions.append(self._parse_type())
            while self._consume(TokenType.COMMA):
                exceptions.append(self._parse_type())
        return exceptions

    def _parse_name(self) -> str:
        """Parse qualified name: package.Class"""
        name = self._expect(TokenType.IDENTIFIER).value
        while self._consume(TokenType.DOT):
            name += '.' + self._expect(TokenType.IDENTIFIER).value
        return name
