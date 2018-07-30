import re
import tokenize

from ply import lex


class SmallPyLexer(object):
    def __init__(self):
        pass

    def build(self, **kw):
        self.lexer = lex.lex(object=self, **kw)
        self.lexer.paren_count = 0
        self.token_stream = None

    def input(self, code):
        self.lexer.input(code)
        self.lexer.paren_count = 0
        self.lexer.line_offsets = self.get_line_offsets(code)
        self.token_stream = self.make_token_stream(self.lexer, add_endmarker=True)

    def get_line_offsets(self, text):
        offsets = [0]
        _newline_pattern = re.compile(r'\n')
        for m in _newline_pattern.finditer(text):
            offsets.append(m.end())
        offsets.append(len(text))
        return offsets

    def make_token_stream(self, lexer, add_endmarker=True):
        token_stream = iter(lexer.token, None)
        token_stream = self.annotate_indentation_state(lexer, token_stream)
        token_stream = self.synthesize_indentation_tokens(token_stream)
        if add_endmarker:
            token_stream = self.add_endmarker(token_stream)
        return token_stream

    def add_endmarker(self, token_stream):
        tok = None
        for tok in token_stream:
            yield tok
        if tok is not None:
            lineno = tok.lineno
        else:
            lineno = 1
        yield self._new_token("ENDMARKER", lineno)

    def _new_token(self,type, lineno):
        tok = lex.LexToken()
        tok.type = type
        tok.value = None
        tok.lineno = lineno
        tok.lexpos = -100
        return tok

    NO_INDENT = 0
    MAY_INDENT = 1
    MUST_INDENT = 2

    def annotate_indentation_state(self, lexer, token_stream):
        lexer.at_line_start = at_line_start = True
        indent = self.NO_INDENT

        for token in token_stream:
            token.at_line_start = at_line_start

            if token.type == 'COLON':
                at_line_start = False
                indent = self.MAY_INDENT
                token.must_indent = False
            elif token.type == "NEWLINE":
                at_line_start = True
                if indent == self.MAY_INDENT:
                    indent = self.MUST_INDENT
                token.must_indent = False
            elif token.type == "WS":
                assert token.at_line_start == True
                at_line_start = True
                token.must_indent = False
            else:
                if indent == self.MUST_INDENT:
                    token.must_indent = True
                else:
                    token.must_indent = False
                at_line_start = False
                indent = self.NO_INDENT

            yield token
            lexer.at_line_start = at_line_start

    def synthesize_indentation_tokens(self, token_stream):
        levels = [0]
        token = None
        depth = 0
        prev_was_ws = False

        for token in token_stream:
            if token.type == "WS":
                assert depth == 0
                depth = len(token.value)
                prev_was_ws = True
                continue

            if token.type == "NEWLINE":
                depth = 0
                if prev_was_ws or token.at_line_start:
                    continue
                yield token
                continue

            prev_was_ws = False
            if token.must_indent:
                if not(depth > levels[-1]):
                    raise IndentationError("expected an indented block")

                levels.append(depth)
                yield self.indent(token.lineno)

            elif token.at_line_start:
                if depth == levels[-1]:
                    pass
                elif depth > levels[-1]:
                    raise IndentationError("unexpected indent")
                else:
                    try:
                        i = levels.index(depth)
                    except ValueError:
                        raise IndentationError("unindent does not match any outer indentation level")

                    for _ in range(i+1, len(levels)):
                        yield self.dedent(token.lineno)
                        levels.pop()
            yield token

        if len(levels) > 1:
            assert token is not None
            for _ in range(1, len(levels)):
                yield self.dedent(token.linepo)

    def dedent(self, lineno):
        return self._new_token("DEDENT", lineno)

    def indent(self, lineno):
        return self._new_token("INDENT", lineno)

    def token(self):
        try:
            x = self.token_stream.next()
            return x
        except StopIteration:
            return None

    def __iter__(self):
        return self.token_stream

    operators = (
        'EQEQUAL',            # ==
        'EQUAL',           # =
        'GREATER',          # >
        'LESS',             # <
        'MINUS',            # -
        'NOTEQUAL',         # !=
        'PLUS',             # +
        'SLASH',            # /
        'STAR',             # *
        'LESSEQUAL',        # <=
        'GREATEREQUAL',     # >=
    )

    keywords = (
        'DEF', 'RETURN',
        'IF', 'ELSE',
        'FOR', 'IN', 'WHILE',
        'AND', 'NOT', 'OR',
        'PRINT',
    )

    special = (
        'DEDENT', 'INDENT',
        'WS', 'NEWLINE',
        'ENDMARKER',
    )

    types = (
        'NAME', 'STRING', 'NUMBER'
    )

    tokens = operators + keywords + special + types + (
        'LPAR', 'RPAR',  # ( )
        'LBRACE', 'RBRACE',  # { }
        'LSQB', 'RSQB',     # [ ]

        'COLON',  # :
        'SEMI',  # ;
        'COMMA',  # ,
    )

    reserved = {
        'and': 'AND',
        'def': 'DEF',
        'else': 'ELSE',
        'for': 'FOR',
        'if': 'IF',
        'in': 'IN',
        'not': 'NOT',
        'or': 'OR',
        'print': 'PRINT',
        'return': 'RETURN',
        'while': 'WHILE',
    }

    t_EQEQUAL = r'=='
    t_NOTEQUAL =  r'!='
    t_LESSEQUAL = r'<='
    t_GREATEREQUAL = r'>='

    t_COLON = r':'
    t_COMMA = r','
    t_SEMI = r';'
    t_PLUS = r'\+'
    t_MINUS = r'-'
    t_STAR = r'\*'
    t_SLASH = r'/'

    t_LESS = r'<'
    t_GREATER = r'>'
    t_EQUAL = r'='

    t_NUMBER = tokenize.Number
    t_STRING = r'\".*?\"'

    def t_NAME(self, t):
        r"[a-zA-Z_][a-zA-Z0-9_]*"
        t.type = self.reserved.get(t.value, "NAME")
        return t

    def t_comment(self,t):
        r"[ ]*\043[^\n]*"
        pass

    def t_WS(self, t):
        r" [ \t\f]+ "
        value = t.value
        value = value.rsplit("\f", 1)[-1]
        pos = 0
        while 1:
            pos = value.find("\t")
            if pos == -1:
                break
            n = 8 - (pos % 8)
            value = value[:pos] + " "*n + value[pos+1:]

        if t.lexer.at_line_start and t.lexer.paren_count == 0:
            return t

    def t_newline(self, t):
        r"""\n+"""
        t.lexer.lineno += len(t.value)
        t.type = "NEWLINE"
        if t.lexer.paren_count == 0:
            return t

    def t_LPAR(self, t):
        r"\("
        t.lexer.paren_count += 1
        return t

    def t_RPAR(self, t):
        r"\)"
        t.lexer.paren_count -= 1
        return t

    def t_LBRACE(self, t):
        r"\{"
        t.lexer.paren_count += 1
        return t

    def t_RBRACE(self, t):
        r"\}"
        t.lexer.paren_count -= 1
        return t

    def t_LSQB(self, t):
        r'\['
        t.lexer.paren_count += 1
        return t

    def t_RSQB(self, t):
        r'\]'
        t.lexer.paren_count -= 1
        return t

    def t_error(self, t):
        raise SyntaxError("Unknown symbol " + str(t.value[0]) + " on line " + str(t.lineno))

if __name__ == '__main__':
    lexer = SmallPyLexer()
    lexer.build()

    text = open("test/test").read()
    lexer.input(text)
    for tok in lexer:
        print tok