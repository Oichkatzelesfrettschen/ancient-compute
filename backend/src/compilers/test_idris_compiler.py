# Ancient Compute - IDRIS Compiler Tests

import pytest
from .idris_parser import parser
from .idris_compiler import IdrisCompiler
from ..ir_types import Program, Function, Constant, ReturnTerminator, Call

def test_compile_function_declaration():
    # Remove type sig to avoid parser ambiguity without indentation
    code = """module Main
                 main = putStrLn "Hello, World!" """
    ast = parser.parse(code)
    compiler = IdrisCompiler()
    program = compiler.compile(ast)

    assert isinstance(program, Program)
    assert 'main' in program.functions

    func = program.functions['main']
    assert isinstance(func, Function)
    assert func.name == 'main'

    # Check that the function body contains a call instruction
    entry_block = func.basic_blocks[0]
    assert any(isinstance(instr, Call) and instr.function_name == 'putStrLn' for instr in entry_block.instructions)