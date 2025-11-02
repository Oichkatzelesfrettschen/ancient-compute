# Ancient Compute - LISP Compiler Tests

import pytest
from .lisp_parser import parser
from .lisp_compiler import LispCompiler
from ..ir_types import Program, Function, Constant, ReturnTerminator, BinaryOp, BranchTerminator, Call

def test_compile_defun():
    code = """(defun my-func (a b)
                 (+ a b))"""
    ast = parser.parse(code)
    compiler = LispCompiler()
    program = compiler.compile(ast)

    assert isinstance(program, Program)
    assert 'my-func' in program.functions

    func = program.functions['my-func']
    assert isinstance(func, Function)
    assert func.name == 'my-func'
    assert func.parameters == ['a', 'b']

    # For now, we just check that the function returns a constant
    entry_block = func.basic_blocks[0]
    assert isinstance(entry_block.terminator, ReturnTerminator)
    assert isinstance(entry_block.terminator.value, Constant)

def test_compile_arithmetic():
    code = """(defun my-add (a b)
                 (+ a b))"""
    ast = parser.parse(code)
    compiler = LispCompiler()
    program = compiler.compile(ast)

    assert isinstance(program, Program)
    assert 'my-add' in program.functions

    func = program.functions['my-add']
    assert isinstance(func, Function)

    # Check that the function body contains a binary operation
    entry_block = func.basic_blocks[0]
    assert any(isinstance(instr, BinaryOp) and instr.op == '+' for instr in entry_block.instructions)

def test_compile_nested_arithmetic():
    code = """(defun my-nested-add (a b c)
                 (+ a (+ b c)))"""
    ast = parser.parse(code)
    compiler = LispCompiler()
    program = compiler.compile(ast)

    assert isinstance(program, Program)
    assert 'my-nested-add' in program.functions

    func = program.functions['my-nested-add']
    assert isinstance(func, Function)

    # Check that the function body contains two binary operations
    entry_block = func.basic_blocks[0]
    assert len([instr for instr in entry_block.instructions if isinstance(instr, BinaryOp)]) == 2

def test_compile_variables():
    code = """(defun my-vars (a b)
                 a)"""
    ast = parser.parse(code)
    compiler = LispCompiler()
    program = compiler.compile(ast)

    assert isinstance(program, Program)
    assert 'my-vars' in program.functions

    func = program.functions['my-vars']
    assert isinstance(func, Function)
    assert 'a' in func.local_variables
    assert 'b' in func.local_variables

def test_compile_if():
    code = """(defun my-if (a b)
                 (if (> a b) a b))"""
    ast = parser.parse(code)
    compiler = LispCompiler()
    program = compiler.compile(ast)

    assert isinstance(program, Program)
    assert 'my-if' in program.functions

    func = program.functions['my-if']
    assert isinstance(func, Function)

    # Check that the function has the correct number of basic blocks
    assert len(func.basic_blocks) == 4 # entry, then, else, merge

    # Check that the entry block has a conditional branch
    entry_block = func.basic_blocks[0]
    assert isinstance(entry_block.terminator, BranchTerminator)

def test_compile_function_call():
    code = """(defun my-call (a b)
                 (my-add a b))"""
    ast = parser.parse(code)
    compiler = LispCompiler()
    program = compiler.compile(ast)

    assert isinstance(program, Program)
    assert 'my-call' in program.functions

    func = program.functions['my-call']
    assert isinstance(func, Function)

    # Check that the function body contains a call instruction
    entry_block = func.basic_blocks[0]
    assert any(isinstance(instr, Call) and instr.function_name == 'my-add' for instr in entry_block.instructions)
