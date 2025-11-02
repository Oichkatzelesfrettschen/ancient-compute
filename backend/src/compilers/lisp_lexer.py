# Ancient Compute - LISP Lexer

import ply.lex as lex

tokens = (
    "LPAREN",
    "RPAREN",
    "SYMBOL",
    "NUMBER",
    "STRING",
)

t_LPAREN = r"\("
t_RPAREN = r"\)"


def t_STRING(t):
    r'"([^"\\]|\\.)*"'
    t.value = t.value[1:-1]
    return t


def t_NUMBER(t):
    r"-?\d+(\.\d+)?"
    if "." in t.value:
        t.value = float(t.value)
    else:
        t.value = int(t.value)
    return t


t_SYMBOL = r"[^\s()]+"

t_ignore = " \t\n"


def t_error(t):
    print(f"Illegal character '{t.value[0]}'")
    t.lexer.skip(1)


lexer = lex.lex()
