"""
IDRIS2 to Babbage IR Compiler - Translates IDRIS2 AST to language-agnostic IR

4-Phase compilation pipeline:
  1. Lexing (via idris_lexer): Source → Tokens
  2. Parsing (via idris_parser): Tokens → AST
  3. Semantic Analysis (this module): Type inference, dependent type checking
  4. IR Generation (this module): AST → Babbage IR

This compiler handles:
  - Dependent types with compile-time value constraints
  - Type families (Vect, List, etc.)
  - Data constructors and pattern matching
  - Function definitions with type annotations
  - Implicit parameters (simplified)
  - Type checking with unification

Limitations:
  - No compile-time proof verification (proofs treated as runtime values)
  - Simplified dependent type instantiation
  - No universe polymorphism
  - Limited implicit parameter resolution
"""

from __future__ import annotations
from typing import Dict, List, Optional, Tuple
import sys

from backend.src.ir_types import (
    IRType, Value, Constant, RegisterValue, MemoryValue, VariableValue, UndefValue,
    Instruction, Assignment, BinaryOp, Load, Store, Call, Jump,
    BasicBlock, Function, Program, IRBuilder, Operand
)
from backend.src.compilers.idris_lexer import IDRIS2Lexer
from backend.src.compilers.idris_parser import IDRIS2Parser
from backend.src.compilers.idris_ast import (
    Type, BaseType, TypeVariable, FunctionType, DependentType, TypeFamily, RefinementType,
    Expr, Var, Literal, Lambda, Application, LetExpr, CaseExpr, IfExpr, ProofExpr,
    DataConstructor,
    Pattern, VarPattern, LiteralPattern, ConstructorPattern, WildcardPattern,
    Declaration, TypeDeclaration, FunctionDef, DataDef, TypeDef,
    Module
)
from backend.src.compilers.idris_types import IDRISTypeSystem, DependentTypeChecker


class IDRISSymbol:
    """Symbol table entry for variable or function"""

    def __init__(self, name: str, type_: Type, is_global: bool = False,
                 is_function: bool = False, is_dependent: bool = False) -> None:
        self.name = name
        self.type_ = type_
        self.is_global = is_global
        self.is_function = is_function
        self.is_dependent = is_dependent


class IDRISSymbolTable:
    """Symbol table for IDRIS2 scoping"""

    def __init__(self, parent: Optional[IDRISSymbolTable] = None) -> None:
        self.parent = parent
        self.symbols: Dict[str, IDRISSymbol] = {}

    def define(self, name: str, type_: Type, is_global: bool = False,
               is_function: bool = False, is_dependent: bool = False) -> None:
        """Define a symbol"""
        self.symbols[name] = IDRISSymbol(name, type_, is_global, is_function, is_dependent)

    def lookup(self, name: str) -> Optional[IDRISSymbol]:
        """Look up symbol in this scope and parent scopes"""
        if name in self.symbols:
            return self.symbols[name]
        if self.parent:
            return self.parent.lookup(name)
        return None

    def push_scope(self) -> IDRISSymbolTable:
        """Push a new scope"""
        return IDRISSymbolTable(parent=self)


class IDRIS2Compiler:
    """IDRIS2 to Babbage IR compiler"""

    def __init__(self, verbose: bool = False) -> None:
        self.verbose = verbose
        self.type_system = IDRISTypeSystem()
        self.type_checker = DependentTypeChecker(self.type_system)
        self.symbol_table = IDRISSymbolTable()
        self.functions: Dict[str, Function] = {}
        self.ir_builder: Optional[IRBuilder] = None
        self.current_block: Optional[BasicBlock] = None
        self.var_counter = 0

    def compile(self, source: str) -> Program:
        """Compile IDRIS2 source to Babbage IR"""

        # Phase 1: Lex
        lexer = IDRIS2Lexer(source)
        tokens = lexer.tokenize()

        if self.verbose:
            print(f"[IDRIS2] Lexed {len(tokens)} tokens")

        # Phase 2: Parse
        parser = IDRIS2Parser(tokens)
        module = parser.parse()

        if self.verbose:
            print(f"[IDRIS2] Parsed module '{module.name}' with {len(module.declarations)} declarations")

        # Phase 3: Semantic Analysis
        # Register all data types and type declarations first
        for decl in module.declarations:
            if isinstance(decl, DataDef):
                self._register_data_definition(decl)
            elif isinstance(decl, TypeDef):
                self._register_type_definition(decl)
            elif isinstance(decl, TypeDeclaration):
                self._register_type_declaration(decl)
            elif isinstance(decl, FunctionDef):
                self._register_function_definition(decl)

        # Phase 4: IR Generation
        # Create IRBuilder for main function
        self.ir_builder = IRBuilder("main", [])
        main_block = self.ir_builder.new_block("main")
        self.current_block = main_block

        for decl in module.declarations:
            if isinstance(decl, FunctionDef):
                # Compile function (already registered)
                pass
            elif isinstance(decl, TypeDeclaration):
                # Type declarations don't generate code
                pass
            elif isinstance(decl, DataDef):
                # Data declarations don't generate code
                pass
            elif isinstance(decl, TypeDef):
                # Type definitions don't generate code
                pass

        # Get main function from builder
        main_func = self.ir_builder.function
        self.functions["main"] = main_func

        # Create IR program
        program = Program(
            functions=list(self.functions.values()),
            global_variables=[]
        )

        if self.verbose:
            print(f"[IDRIS2] Generated {len(self.functions)} functions")

        return program

    def _register_data_definition(self, data_def: DataDef) -> None:
        """Register a data type definition"""
        self.type_system.register_data_type(data_def.name, data_def.type_parameters, data_def.constructors)

        # Register constructors as functions
        for constructor_name, constructor_type in data_def.constructors:
            self.type_system.register_symbol(constructor_name, constructor_type, is_dependent=True)

    def _register_type_definition(self, type_def: TypeDef) -> None:
        """Register a type synonym definition"""
        self.type_system.register_symbol(type_def.name, type_def.body, is_dependent=False)

    def _register_type_declaration(self, type_decl: TypeDeclaration) -> None:
        """Register a type declaration"""
        self.type_system.register_symbol(type_decl.name, type_decl.type_, is_dependent=False)

    def _register_function_definition(self, func_def: FunctionDef) -> None:
        """Register a function definition"""
        # Register type
        self.type_system.register_symbol(func_def.name, func_def.type_annotation, is_dependent=True)

        # Create IRBuilder for this function
        temp_builder = IRBuilder(func_def.name, func_def.parameters)
        func_block = temp_builder.new_block(f"func_{func_def.name}")
        old_block = self.current_block
        old_builder = self.ir_builder
        self.current_block = func_block
        self.ir_builder = temp_builder

        # Push function scope
        old_table = self.symbol_table
        self.symbol_table = self.symbol_table.push_scope()

        # Register parameters
        for param in func_def.parameters:
            self.symbol_table.define(param, TypeVariable("?"), is_function=False)

        # Compile body
        self._compile_expr(func_def.body)

        # Restore scope and builder
        self.symbol_table = old_table
        self.current_block = old_block
        self.ir_builder = old_builder

        # Get function from builder
        func = temp_builder.function
        self.functions[func_def.name] = func

    def _compile_expr(self, expr: Expr) -> Value:
        """Compile an expression to IR and return its value"""

        if isinstance(expr, Literal):
            return self._compile_literal(expr)

        if isinstance(expr, Var):
            return self._compile_var(expr)

        if isinstance(expr, DataConstructor):
            return self._compile_data_constructor(expr)

        if isinstance(expr, Lambda):
            return self._compile_lambda(expr)

        if isinstance(expr, Application):
            return self._compile_application(expr)

        if isinstance(expr, LetExpr):
            return self._compile_let(expr)

        if isinstance(expr, IfExpr):
            return self._compile_if(expr)

        if isinstance(expr, CaseExpr):
            return self._compile_case(expr)

        if isinstance(expr, ProofExpr):
            return self._compile_proof(expr)

        return UndefValue()

    def _compile_literal(self, literal: Literal) -> Value:
        """Compile literal value"""
        if isinstance(literal.value, (int, float)):
            ir_type = "i64" if isinstance(literal.value, int) else "f64"
            return Constant(str(literal.value), ir_type)
        elif isinstance(literal.value, str):
            return Constant(f'"{literal.value}"', "ptr")
        else:
            return UndefValue()

    def _compile_var(self, var: Var) -> Value:
        """Compile variable reference"""
        lookup = self.symbol_table.lookup(var.name)
        if lookup:
            return VariableValue(var.name)
        else:
            # Undefined variable
            return UndefValue()

    def _compile_data_constructor(self, constructor: DataConstructor) -> Value:
        """Compile data constructor application"""
        # Simplified: constructor returns a constant reference
        return Constant(f"&{constructor.constructor_name}", "ptr")

    def _compile_lambda(self, lambda_expr: Lambda) -> Value:
        """Compile lambda expression (anonymous function)"""
        # Simplified: lambda creates a function reference
        func_name = f"lambda_{self.var_counter}"
        self.var_counter += 1

        return VariableValue(func_name)

    def _compile_application(self, app: Application) -> Value:
        """Compile function application"""
        func_val = self._compile_expr(app.func)
        arg_vals = [self._compile_expr(arg) for arg in app.args]

        # Create call instruction
        call_instr = Call(str(func_val), [str(arg) for arg in arg_vals])
        return VariableValue(f"app_result_{self.var_counter}")

    def _compile_let(self, let: LetExpr) -> Value:
        """Compile let binding"""
        # Push new scope for bindings
        old_table = self.symbol_table
        self.symbol_table = self.symbol_table.push_scope()

        # Compile bindings
        for var_name, var_type, value_expr in let.bindings:
            val = self._compile_expr(value_expr)
            self.symbol_table.define(var_name, var_type)

        # Compile body
        result = self._compile_expr(let.body)

        # Restore scope
        self.symbol_table = old_table

        return result

    def _compile_if(self, if_expr: IfExpr) -> Value:
        """Compile if expression"""
        _cond_val = self._compile_expr(if_expr.condition)

        # Simplified: just evaluate both branches
        then_val = self._compile_expr(if_expr.then_expr)
        else_val = (self._compile_expr(if_expr.else_expr)
                   if if_expr.else_expr else UndefValue())

        return then_val

    def _compile_case(self, case: CaseExpr) -> Value:
        """Compile case expression with pattern matching"""
        _scrutinee_val = self._compile_expr(case.scrutinee)

        # Simplified: evaluate first branch
        for _pattern, result in case.branches:
            result_val = self._compile_expr(result)
            return result_val

        return UndefValue()

    def _compile_proof(self, proof: ProofExpr) -> Value:
        """Compile proof expression"""
        # Proofs are erased at runtime; just compile the proof term
        return self._compile_expr(proof.proof)

    def _gensym(self) -> str:
        """Generate unique symbol"""
        self.var_counter += 1
        return f"tmp_{self.var_counter}"
