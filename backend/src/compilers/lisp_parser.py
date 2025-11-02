# Ancient Compute - LISP Parser

import ply.yacc as yacc

from .lisp_lexer import tokens
from .lisp_ast import Symbol, Number, String, SExpression


def p_expression_atom(p):
    """expression : atom"""
    p[0] = p[1]


def p_expression_sexpression(p):
    """expression : LPAREN elements RPAREN"""
    p[0] = SExpression(p[2])


def p_elements_list(p):
    """elements : elements expression"""
    p[0] = p[1] + [p[2]]


def p_elements_empty(p):
    """elements :"""
    p[0] = []


def p_atom_symbol(p):
    """atom : SYMBOL"""
    p[0] = Symbol(p[1])


def p_atom_number(p):
    """atom : NUMBER"""
    p[0] = Number(p[1])


def p_atom_string(p):
    """atom : STRING"""
    p[0] = String(p[1])


def p_error(p):
    if p:
        print(f"Syntax error at '{p.value}'")
    else:
        print("Syntax error at EOF")


parser = yacc.yacc()
