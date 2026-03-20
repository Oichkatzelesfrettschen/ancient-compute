# Ancient Compute - LISP Parser

from typing import Any

import ply.yacc as yacc

from .lisp_ast import Number, SExpression, String, Symbol
from .lisp_lexer import tokens  # noqa: F401 -- PLY requires tokens in module scope


def p_expression_atom(p: Any) -> None:
    """expression : atom"""
    p[0] = p[1]


def p_expression_sexpression(p: Any) -> None:
    """expression : LPAREN elements RPAREN"""
    p[0] = SExpression(p[2])


def p_elements_list(p: Any) -> None:
    """elements : elements expression"""
    p[0] = p[1] + [p[2]]


def p_elements_empty(p: Any) -> None:
    """elements :"""
    p[0] = []


def p_atom_symbol(p: Any) -> None:
    """atom : SYMBOL"""
    p[0] = Symbol(p[1])


def p_atom_number(p: Any) -> None:
    """atom : NUMBER"""
    p[0] = Number(p[1])


def p_atom_string(p: Any) -> None:
    """atom : STRING"""
    p[0] = String(p[1])


def p_error(p: Any) -> None:
    if p:
        print(f"Syntax error at '{p.value}'")
    else:
        print("Syntax error at EOF")


parser = yacc.yacc()
