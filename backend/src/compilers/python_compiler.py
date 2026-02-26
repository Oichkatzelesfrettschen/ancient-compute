"""
Python to Babbage IR Compiler

Compiles Python AST to Babbage IR through these phases:
1. Lexing: Source → Tokens (via PythonLexer)
2. Parsing: Tokens → AST (via PythonParser)
3. Semantic Analysis: Symbol table and type inference
4. IR Generation: AST → Babbage IR

Targets Babbage ISA constraints:
  - 4 registers (A, B, C, D)
  - 2000-word memory
  - 50-bit decimal arithmetic
"""

from __future__ import annotations

import time
from dataclasses import dataclass

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
from backend.src.compilers.python_lexer import PythonLexer
from backend.src.compilers.python_parser import PythonParser
from backend.src.compilers.python_types import PythonType, PythonTypeSystem
from backend.src.ir_types import (
    Assignment,
    BasicBlock,
    BinaryOp,
    BranchTerminator,
    Function,
    IRBuilder,
    Load,
    Program,
    ReturnTerminator,
    VariableValue,
)
from backend.src.ir_types import Call as IRCall
from backend.src.ir_types import Constant as IRConstant


@dataclass
class Symbol:
    """Symbol table entry"""
    name: str
    ptype: PythonType
    scope: str  # 'global', 'parameter', 'local'


class SymbolTable:
    """Symbol table with scope management"""

    def __init__(self, parent: SymbolTable | None = None) -> None:
        """Initialize symbol table"""
        self.symbols: dict[str, Symbol] = {}
        self.parent = parent

    def define(self, name: str, ptype: PythonType, scope: str = 'local') -> None:
        """Define symbol in current scope"""
        self.symbols[name] = Symbol(name=name, ptype=ptype, scope=scope)

    def lookup(self, name: str) -> Symbol | None:
        """Look up symbol, checking parent scopes"""
        if name in self.symbols:
            return self.symbols[name]
        if self.parent:
            return self.parent.lookup(name)
        return None

    def exists(self, name: str) -> bool:
        """Check if symbol exists"""
        return self.lookup(name) is not None


class PythonCompiler:
    """Python to Babbage IR compiler"""

    def __init__(self, verbose: bool = False) -> None:
        """Initialize compiler"""
        self.verbose = verbose
        self.type_system = PythonTypeSystem()
        self.symbol_table: SymbolTable | None = None
        self.builder: IRBuilder | None = None
        self.temp_counter = 0
        self.label_counter = 0
        self.break_labels: list[str] = []
        self.continue_labels: list[str] = []

    def compile(self, source: str) -> Program:
        """Compile Python source to Babbage IR"""
        start_time = time.time()

        try:
            # Phase 1: Lexing
            if self.verbose:
                print("[PYTHON COMPILER] Phase 1: Lexing...")
            lexer = PythonLexer(source)
            tokens = lexer.tokenize()

            # Phase 2: Parsing
            if self.verbose:
                print("[PYTHON COMPILER] Phase 2: Parsing...")
            parser = PythonParser(tokens)
            module = parser.parse()

            # Phase 3: Semantic Analysis
            if self.verbose:
                print("[PYTHON COMPILER] Phase 3: Semantic analysis...")
            self.symbol_table = SymbolTable()
            self._analyze_module(module)

            # Phase 4: IR Generation
            if self.verbose:
                print("[PYTHON COMPILER] Phase 4: IR generation...")
            program = self._generate_ir_module(module)

            if self.verbose:
                elapsed = time.time() - start_time
                print(f"[PYTHON COMPILER] Compilation complete in {elapsed:.3f}s")

            return program

        except Exception as e:
            raise RuntimeError(f"Compilation error: {str(e)}") from e

    def _analyze_module(self, module: Module) -> None:
        """First pass: collect all declarations and build symbol table"""
        for stmt in module.body:
            if isinstance(stmt, FunctionDef):
                self.symbol_table.define(
                    stmt.name,
                    PythonType.any(),  # Functions don't have simple types
                    scope='global'
                )

    def _generate_ir_module(self, module: Module) -> Program:
        """Generate IR for module"""
        program = Program(functions={}, global_variables={})

        for stmt in module.body:
            if isinstance(stmt, FunctionDef):
                func = self._compile_function(stmt)
                program.functions[func.name] = func

        return program

    def _compile_function(self, func_def: FunctionDef) -> Function:
        """Compile function definition"""
        if self.verbose:
            print(f"[PYTHON COMPILER] Compiling function '{func_def.name}'...")

        # Create new builder for this function
        self.builder = IRBuilder(func_def.name, func_def.args)

        # Create entry block
        entry_block = self.builder.new_block("entry")

        # Push new scope for function
        old_table = self.symbol_table
        self.symbol_table = SymbolTable(parent=old_table)

        # Define parameters
        for arg in func_def.args:
            self.symbol_table.define(arg, PythonType.any(), scope='parameter')

        # Compile function body
        for stmt in func_def.body:
            self._compile_statement(stmt, entry_block)

        # If no explicit return, add implicit return None
        if not entry_block.instructions or \
           not isinstance(entry_block.terminator, ReturnTerminator):
            self.builder.emit_return(IRConstant(0))

        # Restore previous scope
        self.symbol_table = old_table

        return self.builder.finalize()

    def _compile_statement(self, stmt: Stmt, block: BasicBlock) -> None:
        """Compile statement to IR"""
        if isinstance(stmt, Assign):
            self._compile_assign(stmt, block)

        elif isinstance(stmt, Return):
            self._compile_return(stmt, block)

        elif isinstance(stmt, If):
            self._compile_if(stmt, block)

        elif isinstance(stmt, While):
            self._compile_while(stmt, block)

        elif isinstance(stmt, For):
            self._compile_for(stmt, block)

        elif isinstance(stmt, ExprStmt):
            self._compile_expression(stmt.value, block)

        elif isinstance(stmt, Pass):
            pass  # No-op

        elif isinstance(stmt, Break):
            if self.break_labels:
                self.builder.emit_jump(self.break_labels[-1])

        elif isinstance(stmt, Continue):
            if self.continue_labels:
                self.builder.emit_jump(self.continue_labels[-1])

    def _compile_assign(self, stmt: Assign, block: BasicBlock) -> None:
        """Compile assignment"""
        value_operand = self._compile_expression(stmt.value, block)

        # Define/update symbol
        if not self.symbol_table.exists(stmt.target):
            # Infer type from value
            if isinstance(stmt.value, Constant):
                inferred_type = self.type_system.infer_literal_type(stmt.value.value)
            else:
                inferred_type = PythonType.any()

            self.symbol_table.define(stmt.target, inferred_type, scope='local')

        # Emit assignment
        self.builder.current_block.instructions.append(
            Assignment(target=stmt.target, source=value_operand)
        )

    def _compile_return(self, stmt: Return, block: BasicBlock) -> None:
        """Compile return statement"""
        if stmt.value:
            value_operand = self._compile_expression(stmt.value, block)
            self.builder.emit_return(value_operand)
        else:
            self.builder.emit_return(IRConstant(0))

    def _compile_if(self, stmt: If, block: BasicBlock) -> None:
        """Compile if statement"""
        test_operand = self._compile_expression(stmt.test, block)

        true_label = self._gen_label("if_true")
        false_label = self._gen_label("if_false")
        end_label = self._gen_label("if_end")

        # Emit branch
        self.builder.current_block.terminator = BranchTerminator(
            condition='nonzero',
            operand1=test_operand,
            operand2=None,
            true_label=true_label,
            false_label=false_label
        )

        # True branch
        true_block = self.builder.new_block(true_label)
        for stmt_item in stmt.body:
            self._compile_statement(stmt_item, true_block)
        if not true_block.terminator:
            self.builder.emit_jump(end_label)

        # False branch (elif/else)
        if stmt.orelse:
            false_block = self.builder.new_block(false_label)
            for stmt_item in stmt.orelse:
                self._compile_statement(stmt_item, false_block)
            if not false_block.terminator:
                self.builder.emit_jump(end_label)
        else:
            # No else, jump directly to end
            self.builder.current_block = self.builder.new_block(false_label)
            self.builder.emit_jump(end_label)

        # End label
        self.builder.current_block = self.builder.new_block(end_label)

    def _compile_while(self, stmt: While, block: BasicBlock) -> None:
        """Compile while loop"""
        loop_label = self._gen_label("while_loop")
        end_label = self._gen_label("while_end")

        # Jump to loop condition
        self.builder.emit_jump(loop_label)

        # Loop block
        loop_block = self.builder.new_block(loop_label)
        test_operand = self._compile_expression(stmt.test, loop_block)

        body_label = self._gen_label("while_body")
        self.builder.current_block.terminator = BranchTerminator(
            condition='nonzero',
            operand1=test_operand,
            operand2=None,
            true_label=body_label,
            false_label=end_label
        )

        # Body block
        body_block = self.builder.new_block(body_label)
        self.break_labels.append(end_label)
        self.continue_labels.append(loop_label)

        for stmt_item in stmt.body:
            self._compile_statement(stmt_item, body_block)

        self.break_labels.pop()
        self.continue_labels.pop()

        if not body_block.terminator:
            self.builder.emit_jump(loop_label)

        # End block
        self.builder.current_block = self.builder.new_block(end_label)

    def _compile_for(self, stmt: For, block: BasicBlock) -> None:
        """Compile for loop: for i in range(n), range(start, stop), range(start, stop, step)"""
        if not isinstance(stmt.iter, Call) or stmt.iter.func != 'range':
            raise NotImplementedError("Only 'for x in range(...)' is supported")

        argc = len(stmt.iter.args)
        if argc == 1:
            # range(n): start=0, stop=n, step=1
            start_operand = IRConstant(0)
            n_operand = self._compile_expression(stmt.iter.args[0], block)
            step_operand = IRConstant(1)
        elif argc == 2:
            # range(start, stop): step=1
            start_operand = self._compile_expression(stmt.iter.args[0], block)
            n_operand = self._compile_expression(stmt.iter.args[1], block)
            step_operand = IRConstant(1)
        elif argc == 3:
            # range(start, stop, step)
            start_operand = self._compile_expression(stmt.iter.args[0], block)
            n_operand = self._compile_expression(stmt.iter.args[1], block)
            step_operand = self._compile_expression(stmt.iter.args[2], block)
        else:
            raise ValueError("range() expects 1, 2, or 3 arguments")

        loop_label = self._gen_label("for_loop")
        body_label = self._gen_label("for_body")
        end_label = self._gen_label("for_end")

        # Initialize loop counter
        counter_temp = self._gen_temp("for_counter")
        self.builder.current_block.instructions.append(
            Assignment(target=counter_temp, source=start_operand)
        )

        self.symbol_table.define(stmt.target, PythonType.int(), scope='local')

        # Jump to condition check
        self.builder.emit_jump(loop_label)

        # Condition block: check counter < n
        loop_block = self.builder.new_block(loop_label)
        condition_temp = self._gen_temp("for_cond")
        loop_block.instructions.append(
            BinaryOp(
                target=condition_temp,
                op='<',
                operand1=VariableValue(counter_temp),
                operand2=n_operand
            )
        )

        loop_block.terminator = BranchTerminator(
            condition='nonzero',
            operand1=VariableValue(condition_temp),
            operand2=None,
            true_label=body_label,
            false_label=end_label
        )

        # Body block
        body_block = self.builder.new_block(body_label)
        body_block.instructions.append(
            Assignment(target=stmt.target, source=VariableValue(counter_temp))
        )

        self.break_labels.append(end_label)
        self.continue_labels.append(loop_label)

        for stmt_item in stmt.body:
            self._compile_statement(stmt_item, body_block)

        self.break_labels.pop()
        self.continue_labels.pop()

        # Increment counter by step
        if not body_block.terminator:
            increment_temp = self._gen_temp("for_inc")
            body_block.instructions.append(
                BinaryOp(
                    target=increment_temp,
                    op='+',
                    operand1=VariableValue(counter_temp),
                    operand2=step_operand
                )
            )
            body_block.instructions.append(
                Assignment(target=counter_temp, source=VariableValue(increment_temp))
            )
            self.builder.emit_jump(loop_label)

        # End block
        self.builder.current_block = self.builder.new_block(end_label)

    def _compile_expression(self, expr: Expr, block: BasicBlock):
        """Compile expression and return operand"""
        if isinstance(expr, Constant):
            if expr.value is None:
                return IRConstant(0)
            elif isinstance(expr.value, bool):
                return IRConstant(1 if expr.value else 0)
            else:
                return IRConstant(expr.value)

        elif isinstance(expr, Name):
            return VariableValue(expr.id)

        elif isinstance(expr, BinOp):
            return self._compile_binary_op(expr, block)

        elif isinstance(expr, UnaryOp):
            return self._compile_unary_op(expr, block)

        elif isinstance(expr, Call):
            return self._compile_call(expr, block)

        elif isinstance(expr, Subscript):
            return self._compile_subscript(expr, block)

        elif isinstance(expr, Attribute):
            return self._compile_attribute(expr, block)

        else:
            raise NotImplementedError(f"Expression type not supported: {type(expr).__name__}")

    def _compile_binary_op(self, expr: BinOp, block: BasicBlock):
        """Compile binary operation"""
        left_operand = self._compile_expression(expr.left, block)
        right_operand = self._compile_expression(expr.right, block)

        # Map Python operators to IR operators
        op_map = {
            '+': 'add',
            '-': 'sub',
            '*': 'mul',
            '/': 'div',
            '//': 'floordiv',
            '%': 'mod',
            '**': 'pow',
            '==': 'eq',
            '!=': 'ne',
            '<': 'lt',
            '<=': 'le',
            '>': 'gt',
            '>=': 'ge',
            'and': 'and',
            'or': 'or',
        }

        ir_op = op_map.get(expr.op, expr.op)

        temp = self._gen_temp("binop")
        block.instructions.append(
            BinaryOp(target=temp, op=ir_op, operand1=left_operand, operand2=right_operand)
        )

        return VariableValue(temp)

    def _compile_unary_op(self, expr: UnaryOp, block: BasicBlock):
        """Compile unary operation"""
        operand = self._compile_expression(expr.operand, block)

        op_map = {
            '-': 'neg',
            '+': 'pos',
            'not': 'not',
        }

        ir_op = op_map.get(expr.op, expr.op)

        temp = self._gen_temp("unop")
        block.instructions.append(
            BinaryOp(target=temp, op=ir_op, operand1=operand, operand2=None)
        )

        return VariableValue(temp)

    def _compile_call(self, expr: Call, block: BasicBlock):
        """Compile function call"""
        args = [self._compile_expression(arg, block) for arg in expr.args]

        temp = self._gen_temp("call")
        block.instructions.append(
            IRCall(target=temp, function_name=expr.func, arguments=args)
        )

        return VariableValue(temp)

    def _compile_subscript(self, expr: Subscript, block: BasicBlock):
        """Compile array subscript: base[index] -> load(base + index)"""
        base_operand = self._compile_expression(expr.value, block)
        index_operand = self._compile_expression(expr.index, block)

        addr_temp = self._gen_temp("sub_addr")
        block.instructions.append(
            BinaryOp(target=addr_temp, op='add', operand1=base_operand, operand2=index_operand)
        )

        temp = self._gen_temp("subscript")
        block.instructions.append(
            Load(target=temp, address=VariableValue(addr_temp))
        )
        return VariableValue(temp)

    def _compile_attribute(self, expr: Attribute, block: BasicBlock):
        """Compile attribute access: value.attr -> load(value + field_offset)"""
        base_operand = self._compile_expression(expr.value, block)
        field_offset = IRConstant(hash(expr.attr) % 2000)

        addr_temp = self._gen_temp("attr_addr")
        block.instructions.append(
            BinaryOp(target=addr_temp, op='add', operand1=base_operand, operand2=field_offset)
        )

        temp = self._gen_temp("attr")
        block.instructions.append(
            Load(target=temp, address=VariableValue(addr_temp))
        )
        return VariableValue(temp)

    def _gen_temp(self, prefix: str = "temp") -> str:
        """Generate unique temporary variable"""
        name = f"{prefix}_{self.temp_counter}"
        self.temp_counter += 1
        return name

    def _gen_label(self, prefix: str = "label") -> str:
        """Generate unique label"""
        name = f"{prefix}_{self.label_counter}"
        self.label_counter += 1
        return name
