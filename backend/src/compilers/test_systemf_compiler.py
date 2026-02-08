"""
System F Compiler Tests
"""

import pytest
from backend.src.compilers.systemf_compiler import SystemFCompiler
from backend.src.ir_types import Program, Constant, VariableValue, Call

def test_compile_identity():
    # id = /\a => \x : a => x
    source = r"/\a => \x : a => x"
    compiler = SystemFCompiler()
    program = compiler.compile(source)
    
    assert "main" in program.functions
    assert len(program.functions) >= 2 # main + lambda_1
    
    main_func = program.functions["main"]
    entry = main_func.basic_blocks[0]
    assert isinstance(entry.terminator.value, Constant)
    assert "lambda" in str(entry.terminator.value.value)

def test_compile_type_application():
    source = r"(/\a => \x : a => x) [Int] 42"
    compiler = SystemFCompiler()
    program = compiler.compile(source)
    
    main_func = program.functions["main"]
    entry = main_func.basic_blocks[0]
    assert any(isinstance(instr, Call) for instr in entry.instructions)

def test_compile_let():
    # Note: 'x' is identifier, but my lexer says single lowercase is TYPE_VAR
    # let val : Int = 10 in val
    source = r"let val : Int = 10 in val"
    compiler = SystemFCompiler()
    program = compiler.compile(source)
    
    main_func = program.functions["main"]
    assert "val" in main_func.local_variables
