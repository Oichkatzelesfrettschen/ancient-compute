# Ancient Compute - IDRIS Parser

import ply.yacc as yacc
from typing import Any, Optional

from .idris_lexer import tokens
from .idris_ast import (
    Module, Import, TypeDeclaration, FunctionDeclaration, Identifier, Literal, FunctionApplication, Let, Case
)

def p_module(p):
    """module : KEYWORD IDENTIFIER body"""
    p[0] = Module(p[2], p[3])

def p_body(p):
    """body : body declaration
            | declaration"""
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = [p[1]]

def p_declaration_type(p):
    """declaration : IDENTIFIER COLON expression"""
    p[0] = TypeDeclaration(p[1], p[3])

def p_declaration_function(p):
    """declaration : IDENTIFIER EQUALS expression"""
    p[0] = FunctionDeclaration(p[1], [], p[3])

def p_expression_application(p):
    """expression : atom atom_list"""
    if not p[2]:
        p[0] = p[1]
    else:
        p[0] = FunctionApplication(p[1], p[2])

def p_atom_list_multiple(p):
    """atom_list : atom atom_list"""
    p[0] = [p[1]] + p[2]

def p_atom_list_empty(p):
    """atom_list : """
    p[0] = []

def p_atom_identifier(p):
    """atom : IDENTIFIER"""
    p[0] = Identifier(p[1])

def p_atom_literal(p):
    """atom : NUMBER
            | STRING"""
    p[0] = Literal(p[1])

def p_atom_paren(p):
    """atom : LPAREN expression RPAREN
            | LPAREN RPAREN"""
    if len(p) == 4:
        p[0] = p[2]
    else:
        p[0] = None

def p_error(p):
    if p:
        print(f"Syntax error at '{p.value}' (type: {p.type}) at line {p.lineno}")
    else:
        print("Syntax error at EOF")

parser = yacc.yacc(debug=False, write_tables=False)


class IdrisParser:
    """Class wrapper around the PLY-generated parser."""

    def __init__(self, source: str):
        self.source = source

    def parse(self):
        from .idris_lexer import lexer as ply_lexer
        result = parser.parse(self.source, lexer=ply_lexer)
        if result is None:
            # PLY parser returned None -- wrap as single-element list for compiler
            from .idris_ast import TypeDeclaration, Identifier
            # Fallback: try to interpret as simple type annotation "name : type"
            parts = self.source.strip().split(':')
            if len(parts) == 2:
                name = parts[0].strip()
                type_name = parts[1].strip()
                return [TypeDeclaration(name, Identifier(type_name))]
            return []
        return result


class IDRIS2Parser(IdrisParser):
    """Compatibility alias."""
    pass