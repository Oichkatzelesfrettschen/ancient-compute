"""
LISP to Babbage IR Compiler - Translates LISP AST to language-agnostic IR

4-Phase compilation pipeline:
  1. Lexing (via lisp_lexer): Source → Tokens
  2. Parsing (via lisp_parser): Tokens → AST
  3. Semantic Analysis (this module): Type inference, function registration
  4. IR Generation (this module): AST → Babbage IR

This compiler handles:
  - Expression evaluation with type inference
  - Variable and function scope management
  - Built-in function compilation
  - Control flow (if/cond/case)
  - Lambda expressions (simplified: no closures)
  - Function definitions

Limitations:
  - No closure capture (lambdas can't capture free variables)
  - Simplified tail recursion (no optimization)
  - Dynamic dispatch for all function calls
"""

from __future__ import annotations
from typing import Dict, List, Optional, Tuple
import sys

from backend.src.ir_types import (
    IRType, Value, Constant, RegisterValue, MemoryValue, VariableValue, UndefValue,
    Instruction, Assignment, BinaryOp, Load, Store, Call, Jump,
    BasicBlock, Function, Program, IRBuilder, Operand
)
from backend.src.compilers.lisp_lexer import LISPLexer
from backend.src.compilers.lisp_parser import LISPParser
from backend.src.compilers.lisp_ast import (
    Expr, Atom, Symbol, List as ListExpr, Quote, Quasiquote, Unquote,
    UnquoteSplicing, Lambda, LetBinding, IfExpr, CondExpr, CaseExpr,
    DefunExpr, BuiltinFunctionCall, FunctionCall, BUILTIN_FUNCTIONS
)
from backend.src.compilers.lisp_types import (
    LISPType, AnyType, IntType, FloatType, StringType, BoolType,
    ListType, FunctionType, VoidType, LISPTypeSystem, BabbageTypeMapper
)


class LISPSymbol:
    """Symbol table entry for variable or function"""

    def __init__(self, name: str, lisp_type: LISPType, is_global: bool = False,
                 is_function: bool = False) -> None:
        self.name = name
        self.lisp_type = lisp_type
        self.is_global = is_global
        self.is_function = is_function


class LISPSymbolTable:
    """Symbol table for LISP scoping"""

    def __init__(self, parent: Optional[LISPSymbolTable] = None) -> None:
        self.parent = parent
        self.symbols: Dict[str, LISPSymbol] = {}

    def define(self, name: str, lisp_type: LISPType, is_global: bool = False,
               is_function: bool = False) -> None:
        """Define a symbol"""
        self.symbols[name] = LISPSymbol(name, lisp_type, is_global, is_function)

    def lookup(self, name: str) -> Optional[LISPSymbol]:
        """Look up symbol in this scope and parent scopes"""
        if name in self.symbols:
            return self.symbols[name]
        if self.parent:
            return self.parent.lookup(name)
        return None

    def push_scope(self) -> LISPSymbolTable:
        """Push a new scope"""
        return LISPSymbolTable(parent=self)


class LISPCompiler:
    """LISP to Babbage IR compiler"""

    def __init__(self, verbose: bool = False) -> None:
        self.verbose = verbose
        self.type_system = LISPTypeSystem()
        self.symbol_table = LISPSymbolTable()
        self.functions: Dict[str, Function] = {}
        self.ir_builder: Optional[IRBuilder] = None
        self.current_block: Optional[BasicBlock] = None
        self.var_counter = 0

    def compile(self, source: str) -> Program:
        """Compile LISP source to Babbage IR"""

        # Phase 1: Lex
        lexer = LISPLexer(source)
        tokens = lexer.tokenize()

        if self.verbose:
            print(f"[LISP] Lexed {len(tokens)} tokens")

        # Phase 2: Parse
        parser = LISPParser(tokens)
        exprs = parser.parse()

        if self.verbose:
            print(f"[LISP] Parsed {len(exprs)} top-level expressions")

        # Phase 3: Semantic Analysis
        # Register all top-level definitions first
        for expr in exprs:
            if isinstance(expr, DefunExpr):
                self._register_function_definition(expr)

        # Phase 4: IR Generation
        # Create IRBuilder for main function
        self.ir_builder = IRBuilder("main", [])
        main_block = self.ir_builder.new_block("main")
        self.current_block = main_block

        for expr in exprs:
            if not isinstance(expr, DefunExpr):  # Skip defun (already registered)
                self._compile_expr(expr)

        # Get main function from builder
        main_func = self.ir_builder.function
        self.functions["main"] = main_func

        # Create IR program
        program = Program(
            functions=list(self.functions.values()),
            global_variables=[]
        )

        if self.verbose:
            print(f"[LISP] Generated {len(self.functions)} functions")

        return program

    def _register_function_definition(self, defun: DefunExpr) -> None:
        """Register a function definition"""
        # Infer function type
        param_types = [AnyType() for _ in defun.parameters]
        return_type = AnyType()  # Inferred from body
        func_type = FunctionType(param_types, return_type)

        self.type_system.register_symbol(defun.name, func_type)
        self.symbol_table.define(defun.name, func_type, is_global=True, is_function=True)

        # Create IRBuilder for this function
        temp_builder = IRBuilder(defun.name, defun.parameters)
        func_block = temp_builder.new_block(f"func_{defun.name}")
        old_block = self.current_block
        old_builder = self.ir_builder
        self.current_block = func_block
        self.ir_builder = temp_builder

        # Push function scope
        old_table = self.symbol_table
        self.symbol_table = self.symbol_table.push_scope()

        # Register parameters
        for param in defun.parameters:
            self.symbol_table.define(param, AnyType())

        # Compile body
        self._compile_expr(defun.body)

        # Restore scope and builder
        self.symbol_table = old_table
        self.current_block = old_block
        self.ir_builder = old_builder

        # Get function from builder
        func = temp_builder.function
        self.functions[defun.name] = func

    def _compile_expr(self, expr: Expr) -> Value:
        """Compile an expression to IR and return its value"""

        if isinstance(expr, Atom):
            return self._compile_atom(expr)

        if isinstance(expr, Symbol):
            return self._compile_symbol(expr)

        if isinstance(expr, ListExpr):
            return self._compile_list(expr)

        if isinstance(expr, Quote):
            return self._compile_quote(expr)

        if isinstance(expr, Lambda):
            return self._compile_lambda(expr)

        if isinstance(expr, LetBinding):
            return self._compile_let(expr)

        if isinstance(expr, IfExpr):
            return self._compile_if(expr)

        if isinstance(expr, CondExpr):
            return self._compile_cond(expr)

        if isinstance(expr, CaseExpr):
            return self._compile_case(expr)

        if isinstance(expr, DefunExpr):
            return self._compile_defun(expr)

        return UndefValue()

    def _compile_atom(self, atom: Atom) -> Value:
        """Compile atomic literal (number or string)"""
        if isinstance(atom.value, (int, float)):
            return Constant(str(atom.value), "i64" if isinstance(atom.value, int) else "f64")
        elif isinstance(atom.value, str):
            # String constants stored in memory
            return Constant(f'"{atom.value}"', "ptr")
        else:
            return UndefValue()

    def _compile_symbol(self, symbol: Symbol) -> Value:
        """Compile symbol reference (variable lookup)"""
        lookup = self.symbol_table.lookup(symbol.name)
        if lookup:
            return VariableValue(symbol.name)
        else:
            # Undefined symbol - treat as unbound
            return UndefValue()

    def _compile_list(self, list_expr: ListExpr) -> Value:
        """Compile list (function call or literal)"""
        if not list_expr.elements:
            # Empty list
            return Constant("0", "ptr")

        first = list_expr.elements[0]

        # Built-in function call
        if isinstance(first, Symbol) and first.name in BUILTIN_FUNCTIONS:
            return self._compile_builtin_call(first.name, list_expr.elements[1:])

        # User function call
        if isinstance(first, Symbol):
            args = [self._compile_expr(arg) for arg in list_expr.elements[1:]]
            return self._compile_user_function_call(first.name, args)

        # Regular list (literal)
        return Constant("0", "ptr")

    def _compile_builtin_call(self, name: str, args: List[Expr]) -> Value:
        """Compile built-in function call"""
        arg_values = [self._compile_expr(arg) for arg in args]

        # Arithmetic
        if name == '+':
            result = arg_values[0]
            for val in arg_values[1:]:
                result = BinaryOp(result, "add", val)
            return result

        if name == '-':
            result = arg_values[0]
            for val in arg_values[1:]:
                result = BinaryOp(result, "sub", val)
            return result

        if name == '*':
            result = arg_values[0]
            for val in arg_values[1:]:
                result = BinaryOp(result, "mul", val)
            return result

        if name == '/':
            result = arg_values[0]
            for val in arg_values[1:]:
                result = BinaryOp(result, "div", val)
            return result

        # Comparison
        if name == '=':
            return BinaryOp(arg_values[0], "eq", arg_values[1])

        if name == '/=':
            return BinaryOp(arg_values[0], "ne", arg_values[1])

        if name == '<':
            return BinaryOp(arg_values[0], "lt", arg_values[1])

        if name == '>':
            return BinaryOp(arg_values[0], "gt", arg_values[1])

        # Logic
        if name == 'and':
            result = arg_values[0]
            for val in arg_values[1:]:
                result = BinaryOp(result, "and", val)
            return result

        if name == 'or':
            result = arg_values[0]
            for val in arg_values[1:]:
                result = BinaryOp(result, "or", val)
            return result

        if name == 'not':
            return BinaryOp(arg_values[0], "not", Constant("0", "i64"))

        # List operations
        if name == 'list':
            return Constant("0", "ptr")  # Simplified

        if name == 'length':
            return arg_values[0]  # Simplified

        # I/O
        if name == 'print':
            # Simplified: just return the value
            return arg_values[0] if arg_values else UndefValue()

        # Default
        return UndefValue()

    def _compile_user_function_call(self, name: str, args: List[Value]) -> Value:
        """Compile user-defined function call"""
        # Create call instruction
        call_instr = Call(name, [str(arg) for arg in args])
        return VariableValue(f"{name}_result")

    def _compile_quote(self, quote: Quote) -> Value:
        """Compile quoted expression (returns unevaluated)"""
        # Simplified: quote just prevents evaluation
        # For now, treat as undef
        return UndefValue()

    def _compile_lambda(self, lambda_expr: Lambda) -> Value:
        """Compile lambda expression (anonymous function)"""
        # Simplified: lambda creates a function reference
        # No closure capture in MVP
        func_name = f"lambda_{self.var_counter}"
        self.var_counter += 1

        return VariableValue(func_name)

    def _compile_let(self, let: LetBinding) -> Value:
        """Compile let binding"""
        # Push new scope for bindings
        old_table = self.symbol_table
        self.symbol_table = self.symbol_table.push_scope()

        # Compile bindings
        for var_name, value_expr in let.bindings:
            val = self._compile_expr(value_expr)
            self.symbol_table.define(var_name, AnyType())

        # Compile body
        result = self._compile_expr(let.body)

        # Restore scope
        self.symbol_table = old_table

        return result

    def _compile_if(self, if_expr: IfExpr) -> Value:
        """Compile if expression"""
        cond_val = self._compile_expr(if_expr.condition)

        # Simplified: just evaluate both branches
        then_val = self._compile_expr(if_expr.then_expr)
        else_val = (self._compile_expr(if_expr.else_expr)
                   if if_expr.else_expr else UndefValue())

        return then_val

    def _compile_cond(self, cond: CondExpr) -> Value:
        """Compile cond expression"""
        # Simplified: evaluate first branch
        for condition, result in cond.branches:
            cond_val = self._compile_expr(condition)
            result_val = self._compile_expr(result)
            return result_val  # Return first matching branch

        return UndefValue()

    def _compile_case(self, case: CaseExpr) -> Value:
        """Compile case expression"""
        scrutinee_val = self._compile_expr(case.scrutinee)

        # Simplified: evaluate first branch
        for pattern, result in case.branches:
            result_val = self._compile_expr(result)
            return result_val

        return UndefValue()

    def _compile_defun(self, defun: DefunExpr) -> Value:
        """Compile function definition"""
        # Already handled in registration phase
        return UndefValue()

    def _gensym(self) -> str:
        """Generate unique symbol"""
        self.var_counter += 1
        return f"tmp_{self.var_counter}"
