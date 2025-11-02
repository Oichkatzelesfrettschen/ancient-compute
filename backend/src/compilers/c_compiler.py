"""C to Babbage Intermediate Representation (IR) compiler.

This module implements the C → IR compilation phase, translating C AST
to language-agnostic Babbage IR for downstream code generation.

Compilation phases:
  1. Parse C source to AST (via c_ast module)
  2. Build symbol table with type information
  3. Translate AST to IR (this module)
  4. Validate IR and type safety
"""

from __future__ import annotations
from typing import Dict, List, Optional, Any
import sys

from backend.src.ir_types import (
    IRType,
    Value,
    Constant,
    RegisterValue,
    MemoryValue,
    VariableValue,
    UndefValue,
    Instruction,
    Assignment,
    BinaryOp,
    Load,
    Store,
    Call,
    Jump,
    BasicBlock,
    Function,
    Program,
    IRBuilder,
    Operand,
)
from backend.src.compilers.c_ast import (
    CLexer,
    CParser,
    Token,
    Expression,
    Statement,
    Type,
    Variable,
    IntLiteral,
    FloatLiteral,
    StringLiteral,
    VariableRef,
    BinaryOp as AstBinaryOp,
    UnaryOp as AstUnaryOp,
    FunctionCall,
    ArrayAccess,
    Assignment as AstAssignment,
    ExpressionStatement,
    VariableDeclaration,
    Block,
    IfStatement,
    WhileStatement,
    ForStatement,
    ReturnStatement,
    Function as AstFunction,
    GlobalDeclaration,
    Program as AstProgram,
)
from backend.src.compilers.c_types import (
    CType,
    CTypeKind,
    CTypeSystem,
    BabbageTypeMapper,
    INT_TYPE,
    FLOAT_TYPE,
    VOID_TYPE,
)


class Symbol:
    """Symbol table entry for variable or function."""

    def __init__(
        self, name: str, c_type: CType, is_global: bool = False, is_function: bool = False
    ) -> None:
        """Initialize symbol."""
        self.name = name
        self.c_type = c_type
        self.is_global = is_global
        self.is_function = is_function


class SymbolTable:
    """Symbol table for managing variables and function scope."""

    def __init__(self, parent: Optional[SymbolTable] = None) -> None:
        """Initialize symbol table with optional parent scope."""
        self.parent = parent
        self.symbols: Dict[str, Symbol] = {}

    def define(
        self, name: str, c_type: CType, is_global: bool = False, is_function: bool = False
    ) -> None:
        """Define a symbol in current scope."""
        self.symbols[name] = Symbol(name, c_type, is_global, is_function)

    def lookup(self, name: str) -> Optional[Symbol]:
        """Look up symbol, checking parent scopes if necessary."""
        if name in self.symbols:
            return self.symbols[name]

        if self.parent:
            return self.parent.lookup(name)

        return None

    def push_scope(self) -> SymbolTable:
        """Push a new scope."""
        return SymbolTable(parent=self)


class CCompiler:
    """C to IR compiler."""

    def __init__(self, verbose: bool = False) -> None:
        """Initialize compiler."""
        self.verbose = verbose
        self.type_system = CTypeSystem()
        self.type_mapper = BabbageTypeMapper()
        self.global_symbols = SymbolTable()
        self.current_scope = self.global_symbols
        self.builder: Optional[IRBuilder] = None
        self.ir_program = Program()
        self.string_literals: Dict[str, int] = {}  # String value → address mapping
        self.next_string_addr = 1000  # Start string pool at address 1000
        self.temp_counter = 0  # For generating unique temp variable names

    def compile(self, c_source: str) -> Program:
        """Compile C source code to IR program."""
        # Lexical analysis
        if self.verbose:
            print("[C COMPILER] Phase 1: Lexical analysis...")

        lexer = CLexer(c_source)
        tokens = lexer.tokenize()

        if self.verbose:
            print(f"[C COMPILER]   Tokens: {len(tokens)}")

        # Parsing
        if self.verbose:
            print("[C COMPILER] Phase 2: Parsing...")

        parser = CParser(tokens)
        ast_program = parser.parse()

        if self.verbose:
            print(f"[C COMPILER]   Declarations: {len(ast_program.declarations)}")

        # Semantic analysis (symbol table building)
        if self.verbose:
            print("[C COMPILER] Phase 3: Semantic analysis...")

        self._build_symbol_table(ast_program)

        # Code generation
        if self.verbose:
            print("[C COMPILER] Phase 4: IR code generation...")

        self._compile_program(ast_program)

        if self.verbose:
            print(f"[C COMPILER]   IR functions: {len(self.ir_program.functions)}")
            print("[C COMPILER] Compilation COMPLETE")

        return self.ir_program

    def _build_symbol_table(self, program: AstProgram) -> None:
        """Build symbol table from AST."""
        # First pass: collect all global declarations
        for decl in program.declarations:
            if isinstance(decl, GlobalDeclaration):
                for var in decl.variables:
                    self.global_symbols.define(var.name, var.type, is_global=True)
            elif isinstance(decl, AstFunction):
                func_type = CType(CTypeKind.FUNCTION)
                self.global_symbols.define(decl.name, func_type, is_global=True, is_function=True)

    def _compile_program(self, program: AstProgram) -> None:
        """Compile entire program."""
        for decl in program.declarations:
            if isinstance(decl, GlobalDeclaration):
                # Skip global variables for now
                pass
            elif isinstance(decl, AstFunction):
                self._compile_function(decl)

    def _compile_function(self, func: AstFunction) -> None:
        """Compile a function declaration."""
        # Create IR builder for this function
        params = [param.name for param in func.parameters]
        self.builder = IRBuilder(func.name, params)

        # Create function scope
        func_scope = self.global_symbols.push_scope()
        self.current_scope = func_scope

        # Add parameters to symbol table
        for param in func.parameters:
            func_scope.define(param.name, param.type, is_global=False)

        # Create entry block
        entry_block = self.builder.new_block("entry")

        # Compile function body
        self._compile_statement(func.body)

        # Ensure function ends with return
        if not isinstance(func.body, Block) or not func.body.statements:
            self.builder.emit_return()

        # Get completed function
        ir_func = self.builder.finalize()

        # Add to program
        self.ir_program.add_function(ir_func)

        # Restore scope
        self.current_scope = self.global_symbols

    def _compile_statement(self, stmt: Statement) -> None:
        """Compile a statement."""
        if isinstance(stmt, Block):
            self._compile_block(stmt)
        elif isinstance(stmt, ExpressionStatement):
            self._compile_expression_statement(stmt)
        elif isinstance(stmt, VariableDeclaration):
            self._compile_variable_declaration(stmt)
        elif isinstance(stmt, IfStatement):
            self._compile_if_statement(stmt)
        elif isinstance(stmt, WhileStatement):
            self._compile_while_statement(stmt)
        elif isinstance(stmt, ForStatement):
            self._compile_for_statement(stmt)
        elif isinstance(stmt, ReturnStatement):
            self._compile_return_statement(stmt)

    def _compile_block(self, block: Block) -> None:
        """Compile a block of statements."""
        # Push new scope
        prev_scope = self.current_scope
        self.current_scope = self.current_scope.push_scope()

        # Compile statements
        for stmt in block.statements:
            self._compile_statement(stmt)

        # Pop scope
        self.current_scope = prev_scope

    def _compile_expression_statement(self, stmt: ExpressionStatement) -> None:
        """Compile expression statement."""
        self._compile_expression(stmt.expr)

    def _compile_variable_declaration(self, stmt: VariableDeclaration) -> None:
        """Compile variable declaration."""
        for var in stmt.variables:
            # Register variable in symbol table
            self.current_scope.define(var.name, var.type)

    def _compile_if_statement(self, stmt: IfStatement) -> None:
        """Compile if statement."""
        # Compile condition
        cond_val = self._compile_expression(stmt.condition)

        # Create then and else blocks
        then_label = self._gen_label("if_then")
        else_label = self._gen_label("if_else")
        end_label = self._gen_label("if_end")

        # Emit conditional branch
        # For now, simple comparison: if condition is non-zero, jump to then
        self.builder.emit_branch("ne", cond_val, Constant(0), then_label, else_label)

        # Compile then branch
        self.builder.new_block(then_label)
        self._compile_statement(stmt.then_stmt)
        self.builder.emit_jump(end_label)

        # Compile else branch if present
        self.builder.new_block(else_label)
        if stmt.else_stmt:
            self._compile_statement(stmt.else_stmt)
        self.builder.emit_jump(end_label)

        # Continue with end block
        self.builder.new_block(end_label)

    def _compile_while_statement(self, stmt: WhileStatement) -> None:
        """Compile while loop."""
        # Create loop structure
        loop_head_label = self._gen_label("while_head")
        loop_body_label = self._gen_label("while_body")
        loop_end_label = self._gen_label("while_end")

        # Jump to loop head
        self.builder.emit_jump(loop_head_label)

        # Loop header: evaluate condition
        self.builder.new_block(loop_head_label)
        cond_val = self._compile_expression(stmt.condition)
        self.builder.emit_branch("ne", cond_val, Constant(0), loop_body_label, loop_end_label)

        # Loop body
        self.builder.new_block(loop_body_label)
        self._compile_statement(stmt.body)
        self.builder.emit_jump(loop_head_label)

        # Continue with end block
        self.builder.new_block(loop_end_label)

    def _compile_for_statement(self, stmt: ForStatement) -> None:
        """Compile for loop."""
        # Create scope for loop variables
        prev_scope = self.current_scope
        self.current_scope = self.current_scope.push_scope()

        # Compile initialization
        if stmt.init:
            self._compile_expression(stmt.init)

        # Create loop structure
        loop_head_label = self._gen_label("for_head")
        loop_body_label = self._gen_label("for_body")
        loop_incr_label = self._gen_label("for_incr")
        loop_end_label = self._gen_label("for_end")

        # Jump to loop head
        self.builder.emit_jump(loop_head_label)

        # Loop header: evaluate condition
        self.builder.new_block(loop_head_label)
        if stmt.condition:
            cond_val = self._compile_expression(stmt.condition)
            self.builder.emit_branch("ne", cond_val, Constant(0), loop_body_label, loop_end_label)
        else:
            self.builder.emit_jump(loop_body_label)

        # Loop body
        self.builder.new_block(loop_body_label)
        self._compile_statement(stmt.body)
        self.builder.emit_jump(loop_incr_label)

        # Loop increment
        self.builder.new_block(loop_incr_label)
        if stmt.increment:
            self._compile_expression(stmt.increment)
        self.builder.emit_jump(loop_head_label)

        # Continue with end block
        self.builder.new_block(loop_end_label)

        # Restore scope
        self.current_scope = prev_scope

    def _compile_return_statement(self, stmt: ReturnStatement) -> None:
        """Compile return statement."""
        if stmt.value:
            return_value = self._compile_expression(stmt.value)
            self.builder.emit_return(return_value)
        else:
            self.builder.emit_return()

    def _compile_expression(self, expr: Expression) -> Operand:
        """Compile an expression to IR, returning an operand."""
        if isinstance(expr, IntLiteral):
            return Constant(float(expr.value), IRType.I64)

        elif isinstance(expr, FloatLiteral):
            return Constant(expr.value, IRType.F64)

        elif isinstance(expr, StringLiteral):
            # Allocate string in memory
            if expr.value not in self.string_literals:
                addr = self.next_string_addr
                self.string_literals[expr.value] = addr
                self.next_string_addr += len(expr.value) + 1

            addr = self.string_literals[expr.value]
            return Constant(float(addr), IRType.PTR)

        elif isinstance(expr, VariableRef):
            symbol = self.current_scope.lookup(expr.name)
            if not symbol:
                raise Exception(f"Undefined variable: {expr.name}")

            return VariableValue(expr.name, IRType.DEC50)

        elif isinstance(expr, AstBinaryOp):
            return self._compile_binary_op(expr)

        elif isinstance(expr, AstUnaryOp):
            return self._compile_unary_op(expr)

        elif isinstance(expr, FunctionCall):
            return self._compile_function_call(expr)

        elif isinstance(expr, ArrayAccess):
            return self._compile_array_access(expr)

        elif isinstance(expr, AstAssignment):
            return self._compile_assignment(expr)

        else:
            raise Exception(f"Unknown expression type: {type(expr)}")

    def _compile_binary_op(self, expr: AstBinaryOp) -> Operand:
        """Compile binary operation."""
        left = self._compile_expression(expr.left)
        right = self._compile_expression(expr.right)

        # Map C operator to IR operator
        op_map = {
            "+": "add",
            "-": "sub",
            "*": "mul",
            "/": "div",
            "%": "mod",
            "==": "eq",
            "!=": "ne",
            "<": "lt",
            "<=": "le",
            ">": "gt",
            ">=": "ge",
            "&&": "and",
            "||": "or",
        }

        ir_op = op_map.get(expr.op, expr.op)

        # Create temporary variable for result
        temp_name = self._gen_temp()
        self.builder.emit_binary_op(ir_op, temp_name, left, right)

        return VariableValue(temp_name, IRType.DEC50)

    def _compile_unary_op(self, expr: AstUnaryOp) -> Operand:
        """Compile unary operation."""
        operand = self._compile_expression(expr.operand)

        # Map C operator to IR operator
        op_map = {
            "-": "neg",
            "!": "not",
        }

        ir_op = op_map.get(expr.op, expr.op)

        # Create temporary variable for result
        temp_name = self._gen_temp()
        # Unary ops use BinaryOp with only operand1 (operand2 is None)
        # But since emit_binary_op requires operand2, we'll use the operand twice
        # for unary operations like negation
        self.builder.emit_binary_op(ir_op, temp_name, operand, operand)

        return VariableValue(temp_name, IRType.DEC50)

    def _compile_function_call(self, expr: FunctionCall) -> Operand:
        """Compile function call."""
        # Compile arguments
        args = []
        for arg in expr.args:
            args.append(self._compile_expression(arg))

        # Create temporary for result
        temp_name = self._gen_temp()

        # Emit function call
        self.builder.emit_call(expr.name, args, temp_name)

        return VariableValue(temp_name, IRType.DEC50)

    def _compile_array_access(self, expr: ArrayAccess) -> Operand:
        """Compile array access."""
        # Get base array
        symbol = self.current_scope.lookup(expr.name)
        if not symbol or not symbol.c_type.is_array():
            raise Exception(f"Not an array: {expr.name}")

        # Compile index
        index = self._compile_expression(expr.index)

        # Create temporary for result
        temp_name = self._gen_temp()

        # Emit load from array (simplified: just return variable for now)
        # Full implementation would calculate address and load from memory
        self.builder.emit_load(temp_name, VariableValue(expr.name, IRType.PTR))

        return VariableValue(temp_name, IRType.DEC50)

    def _compile_assignment(self, expr: AstAssignment) -> Operand:
        """Compile assignment expression."""
        # Compile right-hand side
        rhs = self._compile_expression(expr.value)

        # Get target variable
        symbol = self.current_scope.lookup(expr.target)
        if not symbol:
            raise Exception(f"Undefined variable: {expr.target}")

        # Emit assignment
        self.builder.emit_assignment(expr.target, rhs)

        return rhs

    def _gen_temp(self) -> str:
        """Generate a unique temporary variable name."""
        self.temp_counter += 1
        return f"__temp_{self.temp_counter}"

    def _gen_label(self, prefix: str) -> str:
        """Generate a unique label."""
        self.temp_counter += 1
        return f"{prefix}_{self.temp_counter}"
