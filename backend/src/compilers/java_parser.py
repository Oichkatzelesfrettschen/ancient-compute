"""
Java Parser - Builds Abstract Syntax Tree from tokens

Uses recursive descent parsing for Java syntax.
Handles classes, methods, expressions, statements.
"""

from __future__ import annotations

from backend.src.compilers.java_ast import (
    AnnotationDecl,
    AnnotationMember,
    ArrayAccess,
    ArrayCreation,
    ArrayType,
    BinaryOp,
    BlockStmt,
    BreakStmt,
    CatchBlock,
    ClassDecl,
    CompilationUnit,
    ConditionalExpr,
    ConstructorDecl,
    ContinueStmt,
    DoWhileStmt,
    EnhancedForStmt,
    EnumConstant,
    EnumDecl,
    Expr,
    ExprStmt,
    FieldAccess,
    FieldDecl,
    ForStmt,
    IfStmt,
    ImportDecl,
    InstanceofExpr,
    InterfaceDecl,
    Literal,
    MethodCall,
    MethodDecl,
    NewExpression,
    PackageDecl,
    Parameter,
    PrimitiveType,
    ReferenceType,
    ReturnStmt,
    Stmt,
    SwitchCase,
    SwitchStmt,
    SynchronizedStmt,
    ThrowStmt,
    TryStmt,
    Type,
    TypeParameter,
    UnaryOp,
    VarDeclStmt,
    Variable,
    WhileStmt,
)
from backend.src.compilers.java_lexer import Token, TokenType


class JavaParser:
    """Recursive descent parser for Java"""

    def __init__(self, tokens: list[Token]) -> None:
        self.tokens = tokens
        self.pos = 0
        self.current_token = tokens[0] if tokens else Token(TokenType.EOF, '', 0, 0)

    def parse(self) -> CompilationUnit:
        """Parse entire Java source file"""
        package_decl = None
        import_decls = []
        type_decls = []

        if self._match(TokenType.PACKAGE):
            package_decl = self._parse_package_decl()

        while self._match(TokenType.IMPORT):
            import_decls.append(self._parse_import_decl())

        while self.current_token.type != TokenType.EOF:
            if self._match(TokenType.PUBLIC, TokenType.PRIVATE, TokenType.PROTECTED,
                          TokenType.STATIC, TokenType.FINAL, TokenType.ABSTRACT,
                          TokenType.CLASS, TokenType.INTERFACE, TokenType.ENUM, TokenType.AT):
                type_decls.append(self._parse_type_decl())
            else:
                if self._match(TokenType.CLASS, TokenType.INTERFACE, TokenType.ENUM):
                    type_decls.append(self._parse_type_decl())
                else:
                    self._advance()

        return CompilationUnit(package_decl, import_decls, type_decls)

    def _advance(self) -> None:
        if self.pos < len(self.tokens) - 1:
            self.pos += 1
            self.current_token = self.tokens[self.pos]
        else:
            self.pos = len(self.tokens)
            self.current_token = Token(TokenType.EOF, '', 0, 0)

    def _peek(self, offset: int = 1) -> Token:
        pos = self.pos + offset
        if pos < len(self.tokens):
            return self.tokens[pos]
        return Token(TokenType.EOF, '', 0, 0)

    def _match(self, *token_types: TokenType) -> bool:
        return self.current_token.type in token_types

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
        name = self._expect(TokenType.IDENTIFIER, TokenType.TYPE_NAME).value
        while self._match(TokenType.DOT) and self._peek(1).type != TokenType.STAR:
            self._advance()
            name += '.' + self._expect(TokenType.IDENTIFIER, TokenType.TYPE_NAME).value
        is_on_demand = False
        if self._consume(TokenType.DOT):
            if self._consume(TokenType.STAR):
                is_on_demand = True
        self._expect(TokenType.SEMICOLON)
        return ImportDecl(name, is_static, is_on_demand)

    def _parse_type_decl(self) -> object:
        modifiers = self._parse_modifiers()
        if self._match(TokenType.CLASS): return self._parse_class_decl(modifiers)
        elif self._match(TokenType.INTERFACE): return self._parse_interface_decl(modifiers)
        elif self._match(TokenType.ENUM): return self._parse_enum_decl(modifiers)
        elif self._match(TokenType.AT): return self._parse_annotation_decl(modifiers)
        else: raise SyntaxError(f"Expected type declaration, got {self.current_token.type}")

    def _parse_class_decl(self, modifiers: list[str]) -> ClassDecl:
        self._expect(TokenType.CLASS)
        name = self._expect(TokenType.IDENTIFIER, TokenType.TYPE_NAME).value
        type_parameters = self._parse_type_parameters()
        superclass = None
        if self._consume(TokenType.EXTENDS): superclass = self._parse_type()
        interfaces = []
        if self._consume(TokenType.IMPLEMENTS):
            interfaces.append(self._parse_type())
            while self._consume(TokenType.COMMA): interfaces.append(self._parse_type())
        self._expect(TokenType.LBRACE)
        members = []
        while self.current_token.type != TokenType.RBRACE and self.current_token.type != TokenType.EOF:
            members.append(self._parse_class_member())
        self._expect(TokenType.RBRACE)
        return ClassDecl(name, type_parameters, superclass, interfaces, modifiers, members)

    def _parse_interface_decl(self, modifiers: list[str]) -> InterfaceDecl:
        self._expect(TokenType.INTERFACE)
        name = self._expect(TokenType.IDENTIFIER, TokenType.TYPE_NAME).value
        type_parameters = self._parse_type_parameters()
        extends = []
        if self._consume(TokenType.EXTENDS):
            extends.append(self._parse_type())
            while self._consume(TokenType.COMMA): extends.append(self._parse_type())
        self._expect(TokenType.LBRACE)
        members = []
        while self.current_token.type != TokenType.RBRACE and self.current_token.type != TokenType.EOF:
            members.append(self._parse_interface_member())
        self._expect(TokenType.RBRACE)
        return InterfaceDecl(name, type_parameters, extends, modifiers, members)

    def _parse_enum_decl(self, modifiers: list[str]) -> EnumDecl:
        self._expect(TokenType.ENUM)
        name = self._expect(TokenType.IDENTIFIER, TokenType.TYPE_NAME).value
        interfaces = []
        if self._consume(TokenType.IMPLEMENTS):
            interfaces.append(self._parse_type())
            while self._consume(TokenType.COMMA): interfaces.append(self._parse_type())
        self._expect(TokenType.LBRACE)
        constants = []
        while self.current_token.type in (TokenType.IDENTIFIER, TokenType.TYPE_NAME):
            const_name = self.current_token.value; self._advance()
            arguments = []
            if self._match(TokenType.LPAREN): arguments = self._parse_arguments()
            constants.append(EnumConstant(const_name, arguments))
            if not self._consume(TokenType.COMMA): break
        self._consume(TokenType.SEMICOLON)
        members = []
        while self.current_token.type != TokenType.RBRACE and self.current_token.type != TokenType.EOF:
            members.append(self._parse_class_member())
        self._expect(TokenType.RBRACE)
        return EnumDecl(name, interfaces, modifiers, constants, members)

    def _parse_annotation_decl(self, modifiers: list[str]) -> AnnotationDecl:
        self._expect(TokenType.AT); self._expect(TokenType.INTERFACE)
        name = self._expect(TokenType.IDENTIFIER).value
        self._expect(TokenType.LBRACE)
        members = []
        while self.current_token.type != TokenType.RBRACE and self.current_token.type != TokenType.EOF:
            member_modifiers = self._parse_modifiers()
            member_type = self._parse_type()
            member_name = self._expect(TokenType.IDENTIFIER).value
            self._expect(TokenType.LPAREN); self._expect(TokenType.RPAREN)
            default_value = None
            if self._consume(TokenType.DEFAULT): default_value = self._parse_expr()
            self._expect(TokenType.SEMICOLON)
            members.append(AnnotationMember(member_name, member_type, default_value))
        self._expect(TokenType.RBRACE)
        return AnnotationDecl(name, modifiers, members)

    def _parse_class_member(self) -> object:
        modifiers = self._parse_modifiers()
        if self._match(TokenType.CLASS, TokenType.INTERFACE, TokenType.ENUM):
            return self._parse_type_decl()
        if self._match(TokenType.IDENTIFIER, TokenType.TYPE_NAME) and self._peek(1).type == TokenType.LPAREN:
            name = self.current_token.value; self._advance(); self._expect(TokenType.LPAREN)
            parameters = self._parse_parameters(); self._expect(TokenType.RPAREN)
            exceptions = self._parse_throws(); body = self._parse_block_stmt()
            return ConstructorDecl(name, parameters, body, [], modifiers, exceptions)
        return_type = self._parse_type()
        name = self._expect(TokenType.IDENTIFIER, TokenType.TYPE_NAME).value
        if self._match(TokenType.LPAREN):
            self._expect(TokenType.LPAREN); parameters = self._parse_parameters(); self._expect(TokenType.RPAREN)
            exceptions = self._parse_throws()
            if self._match(TokenType.LBRACE): body = self._parse_block_stmt()
            else: body = None; self._expect(TokenType.SEMICOLON)
            return MethodDecl(name, return_type, parameters, body, [], modifiers, exceptions)
        else:
            initializer = None
            if self._consume(TokenType.EQUAL): initializer = self._parse_expr()
            self._expect(TokenType.SEMICOLON)
            return FieldDecl(name, return_type, initializer, modifiers)

    def _parse_interface_member(self) -> object:
        modifiers = self._parse_modifiers()
        return_type = self._parse_type()
        name = self._expect(TokenType.IDENTIFIER, TokenType.TYPE_NAME).value
        if self._match(TokenType.LPAREN):
            self._expect(TokenType.LPAREN); parameters = self._parse_parameters(); self._expect(TokenType.RPAREN)
            exceptions = self._parse_throws(); body = None
            if self._match(TokenType.LBRACE): body = self._parse_block_stmt()
            else: self._expect(TokenType.SEMICOLON)
            return MethodDecl(name, return_type, parameters, body, [], modifiers, exceptions)
        else:
            initializer = None
            if self._consume(TokenType.EQUAL): initializer = self._parse_expr()
            self._expect(TokenType.SEMICOLON)
            return FieldDecl(name, return_type, initializer, modifiers)

    # Statements

    def _parse_stmt(self) -> Stmt:
        if self._match(TokenType.LBRACE): return self._parse_block_stmt()
        elif self._match(TokenType.VAR, TokenType.INT, TokenType.LONG, TokenType.FLOAT,
                         TokenType.DOUBLE, TokenType.BOOLEAN_TYPE, TokenType.BYTE,
                         TokenType.SHORT, TokenType.CHAR_TYPE, TokenType.STRING):
             return self._parse_var_decl_stmt()
        elif self._match(TokenType.IF): return self._parse_if_stmt()
        elif self._match(TokenType.WHILE): return self._parse_while_stmt()
        elif self._match(TokenType.DO): return self._parse_do_while_stmt()
        elif self._match(TokenType.FOR): return self._parse_for_stmt()
        elif self._match(TokenType.SWITCH): return self._parse_switch_stmt()
        elif self._match(TokenType.BREAK): self._advance(); self._expect(TokenType.SEMICOLON); return BreakStmt()
        elif self._match(TokenType.CONTINUE): self._advance(); self._expect(TokenType.SEMICOLON); return ContinueStmt()
        elif self._match(TokenType.RETURN): return self._parse_return_stmt()
        elif self._match(TokenType.THROW): return self._parse_throw_stmt()
        elif self._match(TokenType.TRY): return self._parse_try_stmt()
        elif self._match(TokenType.SYNCHRONIZED): return self._parse_synchronized_stmt()
        elif self._match(TokenType.IDENTIFIER, TokenType.TYPE_NAME):
            # Lookahead to distinguish decl from assignment
            if self._peek(1).type in (TokenType.IDENTIFIER, TokenType.TYPE_NAME) or self._peek(1).type == TokenType.LBRACKET and self._peek(2).type == TokenType.RBRACKET:
                 return self._parse_var_decl_stmt()
            else:
                expr = self._parse_expr(); self._expect(TokenType.SEMICOLON); return ExprStmt(expr)
        else:
            expr = self._parse_expr(); self._expect(TokenType.SEMICOLON); return ExprStmt(expr)

    def _parse_block_stmt(self) -> BlockStmt:
        self._expect(TokenType.LBRACE); statements = []
        while self.current_token.type != TokenType.RBRACE and self.current_token.type != TokenType.EOF:
            statements.append(self._parse_stmt())
        self._expect(TokenType.RBRACE); return BlockStmt(statements)

    def _parse_var_decl_stmt(self) -> Stmt:
        var_type = self._parse_type()
        name = self._expect(TokenType.IDENTIFIER, TokenType.TYPE_NAME).value
        initializer = None
        if self._consume(TokenType.EQUAL): initializer = self._parse_expr()
        self._expect(TokenType.SEMICOLON)
        return VarDeclStmt(name, var_type, initializer)

    def _parse_if_stmt(self) -> IfStmt:
        self._expect(TokenType.IF); self._expect(TokenType.LPAREN)
        condition = self._parse_expr(); self._expect(TokenType.RPAREN)
        then_stmt = self._parse_stmt(); else_stmt = None
        if self._consume(TokenType.ELSE): else_stmt = self._parse_stmt()
        return IfStmt(condition, then_stmt, else_stmt)

    def _parse_while_stmt(self) -> WhileStmt:
        self._expect(TokenType.WHILE); self._expect(TokenType.LPAREN)
        condition = self._parse_expr(); self._expect(TokenType.RPAREN)
        body = self._parse_stmt(); return WhileStmt(condition, body)

    def _parse_do_while_stmt(self) -> DoWhileStmt:
        self._expect(TokenType.DO); body = self._parse_stmt(); self._expect(TokenType.WHILE)
        self._expect(TokenType.LPAREN); condition = self._parse_expr(); self._expect(TokenType.RPAREN); self._expect(TokenType.SEMICOLON)
        return DoWhileStmt(body, condition)

    def _parse_for_stmt(self) -> Stmt:
        self._expect(TokenType.FOR); self._expect(TokenType.LPAREN)
        initializer = None
        if not self._match(TokenType.SEMICOLON):
             checkpoint = self.pos
             try:
                 var_type = self._parse_type()
                 var_name = self._expect(TokenType.IDENTIFIER, TokenType.TYPE_NAME).value
                 if self._match(TokenType.COLON):
                     self._advance(); iterable = self._parse_expr(); self._expect(TokenType.RPAREN)
                     body = self._parse_stmt(); return EnhancedForStmt(var_type, var_name, iterable, body)
                 else:
                     self.pos = checkpoint; self.current_token = self.tokens[self.pos]
                     var_type = self._parse_type(); var_name = self._expect(TokenType.IDENTIFIER, TokenType.TYPE_NAME).value
                     init_expr = None
                     if self._consume(TokenType.EQUAL): init_expr = self._parse_expr()
                     initializer = VarDeclStmt(var_name, var_type, init_expr)
             except:
                 self.pos = checkpoint; self.current_token = self.tokens[self.pos]; initializer = self._parse_expr()
        self._expect(TokenType.SEMICOLON)
        condition = None
        if not self._match(TokenType.SEMICOLON): condition = self._parse_expr()
        self._expect(TokenType.SEMICOLON)
        update = None
        if not self._match(TokenType.RPAREN): update = self._parse_expr()
        self._expect(TokenType.RPAREN); body = self._parse_stmt()
        return ForStmt(initializer, condition, update, body)

    def _parse_switch_stmt(self) -> SwitchStmt:
        self._expect(TokenType.SWITCH); self._expect(TokenType.LPAREN)
        expr = self._parse_expr(); self._expect(TokenType.RPAREN); self._expect(TokenType.LBRACE)
        cases = []; default_case = None
        while self.current_token.type != TokenType.RBRACE:
            if self._match(TokenType.CASE):
                self._advance(); pattern = self._parse_expr(); self._expect(TokenType.COLON)
                statements = []
                while not self._match(TokenType.CASE, TokenType.DEFAULT, TokenType.RBRACE):
                    statements.append(self._parse_stmt())
                cases.append(SwitchCase(pattern, statements))
            elif self._match(TokenType.DEFAULT):
                self._advance(); self._expect(TokenType.COLON); default_case = []
                while not self._match(TokenType.CASE, TokenType.DEFAULT, TokenType.RBRACE):
                    default_case.append(self._parse_stmt())
            else: break
        self._expect(TokenType.RBRACE); return SwitchStmt(expr, cases, default_case)

    def _parse_return_stmt(self) -> ReturnStmt:
        self._expect(TokenType.RETURN); expr = None
        if not self._match(TokenType.SEMICOLON): expr = self._parse_expr()
        self._expect(TokenType.SEMICOLON); return ReturnStmt(expr)

    def _parse_throw_stmt(self) -> ThrowStmt:
        self._expect(TokenType.THROW); expr = self._parse_expr(); self._expect(TokenType.SEMICOLON); return ThrowStmt(expr)

    def _parse_try_stmt(self) -> TryStmt:
        self._expect(TokenType.TRY); try_block = self._parse_block_stmt(); catch_blocks = []
        while self._match(TokenType.CATCH):
            self._advance(); self._expect(TokenType.LPAREN)
            exc_type = self._parse_type(); exc_var = self._expect(TokenType.IDENTIFIER, TokenType.TYPE_NAME).value
            self._expect(TokenType.RPAREN); block = self._parse_block_stmt(); catch_blocks.append(CatchBlock(exc_type, exc_var, block))
        finally_block = None
        if self._consume(TokenType.FINALLY): finally_block = self._parse_block_stmt()
        return TryStmt(try_block, catch_blocks, finally_block)

    def _parse_synchronized_stmt(self) -> SynchronizedStmt:
        self._expect(TokenType.SYNCHRONIZED); self._expect(TokenType.LPAREN); monitor = self._parse_expr(); self._expect(TokenType.RPAREN)
        body = self._parse_block_stmt(); return SynchronizedStmt(monitor, body)

    # Expressions

    def _parse_expr(self) -> Expr: return self._parse_assignment()

    def _parse_assignment(self) -> Expr:
        expr = self._parse_conditional()
        if self._match(TokenType.EQUAL, TokenType.PLUS_EQUAL, TokenType.MINUS_EQUAL,
                       TokenType.STAR_EQUAL, TokenType.SLASH_EQUAL, TokenType.PERCENT_EQUAL):
            op = self.current_token.value; self._advance(); right = self._parse_assignment()
            return BinaryOp(op, expr, right)
        return expr

    def _parse_conditional(self) -> Expr:
        expr = self._parse_logical_or()
        if self._consume(TokenType.QUESTION):
            true_expr = self._parse_expr(); self._expect(TokenType.COLON); false_expr = self._parse_expr()
            return ConditionalExpr(expr, true_expr, false_expr)
        return expr

    def _parse_logical_or(self) -> Expr:
        expr = self._parse_logical_and()
        while self._match(TokenType.OR):
            op = self.current_token.value; self._advance(); right = self._parse_logical_and()
            expr = BinaryOp(op, expr, right)
        return expr

    def _parse_logical_and(self) -> Expr:
        expr = self._parse_bitwise_or()
        while self._match(TokenType.AND):
            op = self.current_token.value; self._advance(); right = self._parse_bitwise_or()
            expr = BinaryOp(op, expr, right)
        return expr

    def _parse_bitwise_or(self) -> Expr:
        expr = self._parse_bitwise_xor()
        while self._match(TokenType.BIT_OR):
            op = self.current_token.value; self._advance(); right = self._parse_bitwise_xor()
            expr = BinaryOp(op, expr, right)
        return expr

    def _parse_bitwise_xor(self) -> Expr:
        expr = self._parse_bitwise_and()
        while self._match(TokenType.BIT_XOR):
            op = self.current_token.value; self._advance(); right = self._parse_bitwise_and()
            expr = BinaryOp(op, expr, right)
        return expr

    def _parse_bitwise_and(self) -> Expr:
        expr = self._parse_equality()
        while self._match(TokenType.BIT_AND):
            op = self.current_token.value; self._advance(); right = self._parse_equality()
            expr = BinaryOp(op, expr, right)
        return expr

    def _parse_equality(self) -> Expr:
        expr = self._parse_relational()
        while self._match(TokenType.EQUAL_EQUAL, TokenType.NOT_EQUAL):
            op = self.current_token.value; self._advance(); right = self._parse_relational()
            expr = BinaryOp(op, expr, right)
        return expr

    def _parse_relational(self) -> Expr:
        expr = self._parse_shift()
        while self._match(TokenType.LESS, TokenType.GREATER, TokenType.LESS_EQUAL,
                          TokenType.GREATER_EQUAL, TokenType.INSTANCEOF):
            if self._match(TokenType.INSTANCEOF):
                self._advance(); type_ = self._parse_type(); expr = InstanceofExpr(expr, type_)
            else:
                op = self.current_token.value; self._advance(); right = self._parse_shift()
                expr = BinaryOp(op, expr, right)
        return expr

    def _parse_shift(self) -> Expr:
        expr = self._parse_additive()
        while self._match(TokenType.LSHIFT, TokenType.RSHIFT, TokenType.URSHIFT):
            op = self.current_token.value; self._advance(); right = self._parse_additive()
            expr = BinaryOp(op, expr, right)
        return expr

    def _parse_additive(self) -> Expr:
        expr = self._parse_multiplicative()
        while self._match(TokenType.PLUS, TokenType.MINUS):
            op = self.current_token.value; self._advance(); right = self._parse_multiplicative()
            expr = BinaryOp(op, expr, right)
        return expr

    def _parse_multiplicative(self) -> Expr:
        expr = self._parse_unary()
        while self._match(TokenType.STAR, TokenType.SLASH, TokenType.PERCENT):
            op = self.current_token.value; self._advance(); right = self._parse_unary()
            expr = BinaryOp(op, expr, right)
        return expr

    def _parse_unary(self) -> Expr:
        if self._match(TokenType.NOT, TokenType.BIT_NOT, TokenType.PLUS, TokenType.MINUS):
            op = self.current_token.value; self._advance(); expr = self._parse_unary(); return UnaryOp(op, expr, True)
        elif self._match(TokenType.INCREMENT, TokenType.DECREMENT):
            op = self.current_token.value; self._advance(); expr = self._parse_postfix(); return UnaryOp(op, expr, True)
        return self._parse_postfix()

    def _parse_postfix(self) -> Expr:
        expr = self._parse_primary()
        while True:
            if self._match(TokenType.DOT):
                self._advance()
                if self._match(TokenType.IDENTIFIER, TokenType.TYPE_NAME):
                    field_name = self.current_token.value; self._advance()
                    if self._match(TokenType.LPAREN): args = self._parse_arguments(); expr = MethodCall(expr, field_name, [], args)
                    else: expr = FieldAccess(expr, field_name)
                elif self._match(TokenType.NEW):
                    self._advance(); type_ = ReferenceType(self._expect(TokenType.TYPE_NAME, TokenType.IDENTIFIER).value)
                    args = self._parse_arguments(); expr = NewExpression(type_, [], args)
            elif self._match(TokenType.LBRACKET):
                self._advance(); index = self._parse_expr(); self._expect(TokenType.RBRACKET)
                expr = ArrayAccess(expr, index)
            elif self._match(TokenType.INCREMENT, TokenType.DECREMENT):
                op = self.current_token.value; self._advance(); expr = UnaryOp(op, expr, False)
            else: break
        return expr

    def _parse_primary(self) -> Expr:
        if self._match(TokenType.NUMBER):
            val = self.current_token.value; self._advance()
            try: return Literal(float(val)) if '.' in val else Literal(int(val))
            except: return Literal(val)
        elif self._match(TokenType.STRING_LIT): return Literal(self._expect(TokenType.STRING_LIT).value)
        elif self._match(TokenType.CHAR_LIT): return Literal(self._expect(TokenType.CHAR_LIT).value)
        elif self._match(TokenType.BOOLEAN): return Literal(self._expect(TokenType.BOOLEAN).value == 'true')
        elif self._match(TokenType.NULL): self._advance(); return Literal(None)
        elif self._match(TokenType.IDENTIFIER, TokenType.TYPE_NAME):
            name = self.current_token.value; self._advance()
            if self._match(TokenType.LPAREN): args = self._parse_arguments(); return MethodCall(None, name, [], args)
            return Variable(name)
        elif self._match(TokenType.THIS): self._advance(); return Variable('this')
        elif self._match(TokenType.SUPER): self._advance(); return Variable('super')
        elif self._match(TokenType.NEW): return self._parse_new_expr()
        elif self._match(TokenType.LPAREN): self._advance(); expr = self._parse_expr(); self._expect(TokenType.RPAREN); return expr
        else: raise SyntaxError(f"Unexpected token: {self.current_token.type} at line {self.current_token.line}")

    def _parse_new_expr(self) -> Expr:
        self._expect(TokenType.NEW); checkpoint = self.pos
        try:
            type_ = self._parse_primary_type()
            if self._match(TokenType.LBRACKET):
                dims = []
                while self._consume(TokenType.LBRACKET):
                    if not self._match(TokenType.RBRACKET): dims.append(self._parse_expr())
                    self._expect(TokenType.RBRACKET)
                return ArrayCreation(type_, dims, None)
            else: args = self._parse_arguments(); return NewExpression(type_, [], args)
        except:
            self.pos = checkpoint; self.current_token = self.tokens[self.pos]
            type_ = self._parse_type(); args = self._parse_arguments(); return NewExpression(type_, [], args)

    def _parse_type(self) -> Type:
        base_type = self._parse_primary_type()
        while self._match(TokenType.LBRACKET) and self._peek(1).type == TokenType.RBRACKET:
            self._advance(); self._advance(); base_type = ArrayType(base_type)
        return base_type

    def _parse_primary_type(self) -> Type:
        if self._match(TokenType.INT): self._advance(); return PrimitiveType("int")
        elif self._match(TokenType.LONG): self._advance(); return PrimitiveType("long")
        elif self._match(TokenType.FLOAT): self._advance(); return PrimitiveType("float")
        elif self._match(TokenType.DOUBLE): self._advance(); return PrimitiveType("double")
        elif self._match(TokenType.BOOLEAN_TYPE): self._advance(); return PrimitiveType("boolean")
        elif self._match(TokenType.BYTE): self._advance(); return PrimitiveType("byte")
        elif self._match(TokenType.SHORT): self._advance(); return PrimitiveType("short")
        elif self._match(TokenType.CHAR_TYPE): self._advance(); return PrimitiveType("char")
        elif self._match(TokenType.VOID): self._advance(); return PrimitiveType("void")
        elif self._match(TokenType.STRING): self._advance(); return ReferenceType("String")
        elif self._match(TokenType.IDENTIFIER, TokenType.TYPE_NAME):
            name = self.current_token.value; self._advance()
            type_args = []
            if self._match(TokenType.LESS): type_args = self._parse_type_arguments_inline()
            return ReferenceType(name, type_args)
        else: raise SyntaxError(f"Expected type, got {self.current_token.type}")

    def _parse_type_parameters(self) -> list[TypeParameter]:
        params = []
        if self._match(TokenType.LESS):
            self._advance()
            while self.current_token.type != TokenType.GREATER:
                name = self._expect(TokenType.IDENTIFIER, TokenType.TYPE_NAME).value
                bounds = []
                if self._consume(TokenType.EXTENDS): bounds.append(self._parse_type())
                params.append(TypeParameter(name, bounds))
                if not self._consume(TokenType.COMMA): break
            self._expect(TokenType.GREATER)
        return params

    def _parse_type_arguments_inline(self) -> list[Type]:
        args = []; self._expect(TokenType.LESS)
        while self.current_token.type != TokenType.GREATER:
            args.append(self._parse_type())
            if not self._consume(TokenType.COMMA): break
        self._expect(TokenType.GREATER); return args

    def _parse_modifiers(self) -> list[str]:
        mods = []
        while self._match(TokenType.PUBLIC, TokenType.PRIVATE, TokenType.PROTECTED,
                          TokenType.STATIC, TokenType.FINAL, TokenType.ABSTRACT,
                          TokenType.NATIVE, TokenType.SYNCHRONIZED, TokenType.VOLATILE,
                          TokenType.TRANSIENT, TokenType.AT):
            mods.append(self.current_token.value); self._advance()
        return mods

    def _parse_parameters(self) -> list[Parameter]:
        params = []
        while self.current_token.type != TokenType.RPAREN:
            ptype = self._parse_type(); pname = self._expect(TokenType.IDENTIFIER, TokenType.TYPE_NAME).value
            params.append(Parameter(pname, ptype, False))
            if not self._consume(TokenType.COMMA): break
        return params

    def _parse_arguments(self) -> list[Expr]:
        self._expect(TokenType.LPAREN); args = []
        while self.current_token.type != TokenType.RPAREN:
            args.append(self._parse_expr())
            if not self._consume(TokenType.COMMA): break
        self._expect(TokenType.RPAREN); return args

    def _parse_throws(self) -> list[Type]:
        excs = []
        if self._consume(TokenType.THROWS):
            excs.append(self._parse_type())
            while self._consume(TokenType.COMMA): excs.append(self._parse_type())
        return excs

    def _parse_name(self) -> str:
        name = self._expect(TokenType.IDENTIFIER, TokenType.TYPE_NAME).value
        while self._consume(TokenType.DOT):
            name += '.' + self._expect(TokenType.IDENTIFIER, TokenType.TYPE_NAME).value
        return name

    def _expect(self, *types: TokenType) -> Token:
        if self.current_token.type not in types:
            raise SyntaxError(f"Expected {types}, got {self.current_token.type} at line {self.current_token.line}")
        token = self.current_token; self._advance(); return token
