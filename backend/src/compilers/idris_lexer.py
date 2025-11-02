# Ancient Compute - IDRIS Lexer

import ply.lex as lex
from typing import List

tokens = (
    "KEYWORD",
    "SYMBOL",
    "OPERATOR",
    "IDENTIFIER",
    "NUMBER",
    "STRING",
    "LPAREN",
    "RPAREN",
    "LBRACK",
    "RBRACK",
    "LBRACE",
    "RBRACE",
    "COMMA",
    "DOT",
    "COLON",
    "EQUALS",
    "RARROW",
    "DARROW",
    "PIPE",
)

keywords = {
    "module": "KEYWORD",
    "import": "KEYWORD",
    "let": "KEYWORD",
    "in": "KEYWORD",
    "where": "KEYWORD",
    "case": "KEYWORD",
    "of": "KEYWORD",
    "if": "KEYWORD",
    "then": "KEYWORD",
    "else": "KEYWORD",
    "do": "KEYWORD",
}

t_LPAREN = "\\("
t_RPAREN = "\\)"
t_LBRACK = "\\["
t_RBRACK = "\\]"
t_LBRACE = "\\{"
t_RBRACE = "\\}"
t_COMMA = ","
t_DOT = "\\."
t_COLON = ":"
t_EQUALS = "="
t_RARROW = "->"
t_DARROW = "=>"
t_PIPE = "\\|"

t_OPERATOR = "\\+|-|\\*|/|==|>|<|>=|<="


def t_IDENTIFIER(t):
    r"[a-zA-Z_][a-zA-Z0-9_]*"
    t.type = keywords.get(t.value, "IDENTIFIER")
    return t


def t_STRING(t):
    r'"([^"\\]|\\.)*"'
    t.value = t.value[1:-1]
    return t


def t_NUMBER(t):
    r"-?\\d+(\\.\\d+)?"
    if "." in t.value:
        t.value = float(t.value)
    else:
        t.value = int(t.value)
    return t


t_ignore = " \t\n"


def t_error(t):
    print(f"Illegal character '{t.value[0]}'")
    t.lexer.skip(1)


lexer = lex.lex()


class IDRIS2Lexer:
    """Compatibility wrapper providing a `tokenize` method.

    Tests expect a class with `tokenize()` returning a list of tokens.
    """

    def __init__(self, source: str):
        self.source = source

    def tokenize(self) -> List:
        lexer.input(self.source)
        toks = []
        while True:
            tok = lexer.token()
            if not tok:
                break
            toks.append(tok)
        return toks
