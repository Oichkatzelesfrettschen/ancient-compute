"""
Haskell to Babbage IR Compiler

Compiles Haskell source code to Babbage IR through these phases:
1. Lexing: Source → Tokens (via HaskellLexer)
2. Parsing: Tokens → AST (via HaskellParser)
3. Semantic Analysis: Type inference and pattern translation
4. IR Generation: AST → Babbage IR

Key transformations:
- Pattern matching → case expressions
- Lazy evaluation → strict evaluation
- Polymorphic types → Babbage concrete types
- Function definitions → procedures with multiple equations
"""

from __future__ import annotations

import time
from dataclasses import dataclass

from backend.src.compilers.haskell_ast import (
    Application,
    BinOp,
    Case,
    Constructor,
    DataDecl,
    Expr,
    FunctionDef,
    FunctionEquation,
    IfThenElse,
    Lambda,
    Let,
    Literal,
    Module,
    PatternLiteral,
    TypeDecl,
    UnaryOp,
    Variable,
)
from backend.src.compilers.haskell_ast import List as HList
from backend.src.compilers.haskell_ast import Tuple as HTuple
from backend.src.compilers.haskell_lexer import HaskellLexer
from backend.src.compilers.haskell_parser import HaskellParser
from backend.src.compilers.haskell_types import (
    HaskellType,
    HaskellTypeSystem,
    TypeEnvironment,
)
from backend.src.ir_types import (
    Assignment,
    BasicBlock,
    BinaryOp,
    BranchTerminator,
    Function,
    IRBuilder,
    Program,
    ReturnTerminator,
    VariableValue,
)
from backend.src.ir_types import Call as IRCall
from backend.src.ir_types import Constant as IRConstant


@dataclass
class Symbol:
    """Symbol table entry for Haskell compilation"""
    name: str
    htype: HaskellType
    scope: str  # 'global', 'parameter', 'local'


class SymbolTable:
    """Symbol table with scope management"""

    def __init__(self, parent: SymbolTable | None = None) -> None:
        """Initialize symbol table"""
        self.symbols: dict[str, Symbol] = {}
        self.parent = parent

    def define(self, name: str, htype: HaskellType, scope: str = 'local') -> None:
        """Define symbol in current scope"""
        self.symbols[name] = Symbol(name=name, htype=htype, scope=scope)

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


class HaskellCompiler:
    """Haskell to Babbage IR compiler"""

    def __init__(self, verbose: bool = False) -> None:
        """Initialize compiler"""
        self.verbose = verbose
        self.type_system = HaskellTypeSystem()
        self.symbol_table: SymbolTable | None = None
        self.builder: IRBuilder | None = None
        self.temp_counter = 0
        self.label_counter = 0
        self.type_env: TypeEnvironment | None = None

    def compile(self, source: str) -> Program:
        """Compile Haskell source to Babbage IR"""
        start_time = time.time()

        try:
            # Phase 1: Lexing
            if self.verbose:
                print("[HASKELL COMPILER] Phase 1: Lexing...")
            lexer = HaskellLexer(source)
            tokens = lexer.tokenize()

            # Phase 2: Parsing
            if self.verbose:
                print("[HASKELL COMPILER] Phase 2: Parsing...")
            parser = HaskellParser(tokens)
            module = parser.parse()

            # Phase 3: Semantic Analysis
            if self.verbose:
                print("[HASKELL COMPILER] Phase 3: Semantic analysis...")
            self.symbol_table = SymbolTable()
            self.type_env = TypeEnvironment()
            self._analyze_module(module)

            # Phase 4: IR Generation
            if self.verbose:
                print("[HASKELL COMPILER] Phase 4: IR generation...")
            program = self._generate_ir_module(module)

            if self.verbose:
                elapsed = time.time() - start_time
                print(f"[HASKELL COMPILER] Compilation complete in {elapsed:.3f}s")

            return program

        except Exception as e:
            raise RuntimeError(f"Compilation error: {str(e)}") from e

    def _analyze_module(self, module: Module) -> None:
        """First pass: collect declarations and build type environment"""
        for stmt in module.declarations:
            if isinstance(stmt, TypeDecl):
                # Parse type signature and store
                # For now, skip full parsing
                pass
            elif isinstance(stmt, FunctionDef):
                # Infer function type from first equation
                htype = HaskellType.function(
                    HaskellType.var('a'),
                    HaskellType.var('b')
                )
                self.symbol_table.define(stmt.name, htype, scope='global')
                self.type_env.bind(stmt.name, htype)
            elif isinstance(stmt, DataDecl):
                # Register data constructors
                for constructor in stmt.constructors:
                    cons_type = HaskellType(constructor.name)
                    self.symbol_table.define(constructor.name, cons_type, scope='global')

    def _generate_ir_module(self, module: Module) -> Program:
        """Generate IR for module"""
        program = Program(functions={}, global_variables={})

        for stmt in module.declarations:
            if isinstance(stmt, FunctionDef):
                func = self._compile_function(stmt)
                program.functions[func.name] = func

        return program

    def _compile_function(self, func_def: FunctionDef) -> Function:
        """Compile function definition with pattern matching"""
        if self.verbose:
            print(f"[HASKELL COMPILER] Compiling function '{func_def.name}'...")

        # Create builder for this function
        self.builder = IRBuilder(func_def.name, [])
        entry_block = self.builder.new_block("entry")

        # Push new scope
        old_table = self.symbol_table
        self.symbol_table = SymbolTable(parent=old_table)

        # Extract all parameter patterns from first equation
        if func_def.equations:
            first_eq = func_def.equations[0]
            num_params = len(first_eq.patterns)
            param_names = [f"arg{i}" for i in range(num_params)]
        else:
            param_names = []

        # Compile function equations with pattern matching
        if len(func_def.equations) == 1:
            # Single equation
            equation = func_def.equations[0]
            self._compile_equation(equation, entry_block, param_names)
        else:
            # Multiple equations - compile as cascading guards
            for i, equation in enumerate(func_def.equations):
                if i == 0:
                    self._compile_equation(equation, entry_block, param_names)
                else:
                    # Create new block for next equation
                    next_block = self.builder.new_block(f"eq_{i}")
                    self._compile_equation(equation, next_block, param_names)

        # Ensure return
        if not entry_block.instructions or not isinstance(entry_block.terminator, ReturnTerminator):
            self.builder.emit_return(IRConstant(0))

        # Restore scope
        self.symbol_table = old_table

        return self.builder.finalize()

    def _compile_equation(self, equation: FunctionEquation, block: BasicBlock, param_names: list[str]) -> None:
        """Compile single function equation"""
        # Bind parameters
        for name, pattern in zip(param_names, equation.patterns):
            htype = HaskellType.var('a')
            self.symbol_table.define(name, htype, scope='parameter')

        # Compile guard if present
        if equation.guard:
            guard_operand = self._compile_expression(equation.guard, block)
            then_label = self._gen_label("guard_true")
            else_label = self._gen_label("guard_false")

            block.terminator = BranchTerminator(
                condition='nonzero',
                operand1=guard_operand,
                operand2=None,
                true_label=then_label,
                false_label=else_label
            )

            then_block = self.builder.new_block(then_label)
            else_block = self.builder.new_block(else_label)

            body_operand = self._compile_expression(equation.body, then_block)
            self.builder.emit_return(body_operand)

            self.builder.current_block = else_block
            # Fall through to next equation or fail
        else:
            # Compile body directly
            body_operand = self._compile_expression(equation.body, block)
            self.builder.emit_return(body_operand)

    def _compile_expression(self, expr: Expr, block: BasicBlock):
        """Compile expression and return operand"""
        if isinstance(expr, Literal):
            return IRConstant(expr.value)

        elif isinstance(expr, Variable):
            return VariableValue(expr.name)

        elif isinstance(expr, BinOp):
            return self._compile_binary_op(expr, block)

        elif isinstance(expr, UnaryOp):
            return self._compile_unary_op(expr, block)

        elif isinstance(expr, Lambda):
            return self._compile_lambda(expr, block)

        elif isinstance(expr, Application):
            return self._compile_application(expr, block)

        elif isinstance(expr, Let):
            return self._compile_let(expr, block)

        elif isinstance(expr, Case):
            return self._compile_case(expr, block)

        elif isinstance(expr, IfThenElse):
            return self._compile_if(expr, block)

        elif isinstance(expr, HList):
            return self._compile_list(expr, block)

        elif isinstance(expr, HTuple):
            return self._compile_tuple(expr, block)

        elif isinstance(expr, Constructor):
            return self._compile_constructor(expr, block)

        else:
            raise NotImplementedError(f"Expression type not supported: {type(expr).__name__}")

    def _compile_binary_op(self, expr: BinOp, block: BasicBlock):
        """Compile binary operation"""
        left_operand = self._compile_expression(expr.left, block)
        right_operand = self._compile_expression(expr.right, block)

        # Map Haskell operators to IR operators
        op_map = {
            '+': 'add',
            '-': 'sub',
            '*': 'mul',
            '/': 'div',
            '%': 'mod',
            '^': 'pow',
            '==': 'eq',
            '/=': 'ne',
            '<': 'lt',
            '<=': 'le',
            '>': 'gt',
            '>=': 'ge',
            '++': 'concat',  # String/list concat
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

    def _compile_lambda(self, expr: Lambda, block: BasicBlock):
        """Compile lambda expression by lifting to a global function"""
        func_name = self._gen_temp("lambda")
        params = list(expr.params) if hasattr(expr, 'params') else [expr.param] if hasattr(expr, 'param') else []

        # Save current builder state
        old_builder = self.builder
        old_table = self.symbol_table

        self.builder = IRBuilder(func_name, params)
        entry_block = self.builder.new_block("entry")
        self.symbol_table = SymbolTable(parent=old_table)

        for param in params:
            self.symbol_table.define(param, HaskellType.var('a'), scope='parameter')

        body_operand = self._compile_expression(expr.body, entry_block)
        self.builder.emit_return(body_operand)

        # Finalize lifted function (stored externally by builder)
        self.builder.finalize()

        # Restore builder state
        self.builder = old_builder
        self.symbol_table = old_table

        # Return function reference as constant
        return IRConstant(func_name)

    def _compile_application(self, expr: Application, block: BasicBlock):
        """Compile function application"""
        func_operand = self._compile_expression(expr.func, block)
        args = [self._compile_expression(arg, block) for arg in expr.args]

        temp = self._gen_temp("app")
        block.instructions.append(
            IRCall(target=temp, function_name=str(expr.func), arguments=args)
        )

        return VariableValue(temp)

    def _compile_let(self, expr: Let, block: BasicBlock):
        """Compile let expression"""
        # Compile bindings
        for name, binding_expr in expr.bindings:
            binding_operand = self._compile_expression(binding_expr, block)
            block.instructions.append(
                Assignment(target=name, source=binding_operand)
            )
            self.symbol_table.define(name, HaskellType.var('a'), scope='local')

        # Compile body
        return self._compile_expression(expr.body, block)

    def _compile_case(self, expr: Case, block: BasicBlock):
        """Compile case expression"""
        scrutinee_operand = self._compile_expression(expr.expr, block)

        # For MVP, compile as cascading if-then-else
        for i, branch in enumerate(expr.branches):
            branch_label = self._gen_label(f"case_branch_{i}")
            next_label = self._gen_label(f"case_next_{i}")

            # Simple pattern matching - only literals for now
            if isinstance(branch.pattern, PatternLiteral):
                condition_temp = self._gen_temp("case_cond")
                block.instructions.append(
                    BinaryOp(
                        target=condition_temp,
                        op='eq',
                        operand1=scrutinee_operand,
                        operand2=IRConstant(branch.pattern.value)
                    )
                )

                block.terminator = BranchTerminator(
                    condition='nonzero',
                    operand1=VariableValue(condition_temp),
                    operand2=None,
                    true_label=branch_label,
                    false_label=next_label
                )

            body_block = self.builder.new_block(branch_label)
            result_operand = self._compile_expression(branch.body, body_block)
            self.builder.emit_return(result_operand)

            block = self.builder.new_block(next_label)

        # Fallback case
        return IRConstant(0)

    def _compile_if(self, expr: IfThenElse, block: BasicBlock):
        """Compile if-then-else expression"""
        condition_operand = self._compile_expression(expr.condition, block)

        then_label = self._gen_label("if_then")
        else_label = self._gen_label("if_else")
        end_label = self._gen_label("if_end")

        block.terminator = BranchTerminator(
            condition='nonzero',
            operand1=condition_operand,
            operand2=None,
            true_label=then_label,
            false_label=else_label
        )

        # Then branch
        then_block = self.builder.new_block(then_label)
        then_operand = self._compile_expression(expr.then_expr, then_block)
        result_temp = self._gen_temp("if_result")
        then_block.instructions.append(
            Assignment(target=result_temp, source=then_operand)
        )
        self.builder.emit_jump(end_label)

        # Else branch
        else_block = self.builder.new_block(else_label)
        else_operand = self._compile_expression(expr.else_expr, else_block)
        else_block.instructions.append(
            Assignment(target=result_temp, source=else_operand)
        )
        self.builder.emit_jump(end_label)

        # End block
        self.builder.current_block = self.builder.new_block(end_label)
        return VariableValue(result_temp)

    def _compile_list(self, expr: HList, block: BasicBlock):
        """Compile list literal"""
        # For MVP, represent as first element (simplified)
        if expr.elements:
            return self._compile_expression(expr.elements[0], block)
        return IRConstant(0)

    def _compile_tuple(self, expr: HTuple, block: BasicBlock):
        """Compile tuple literal"""
        # For MVP, represent as first element (simplified)
        if expr.elements:
            return self._compile_expression(expr.elements[0], block)
        return IRConstant(0)

    def _compile_constructor(self, expr: Constructor, block: BasicBlock):
        """Compile constructor application"""
        # For MVP, treat like function application
        args = [self._compile_expression(arg, block) for arg in expr.args]
        temp = self._gen_temp("constructor")
        block.instructions.append(
            IRCall(target=temp, function_name=expr.name, arguments=args)
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
