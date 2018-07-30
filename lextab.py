# lextab.py. This file automatically created by PLY (version 3.9). Don't edit!
_tabversion   = '3.8'
_lextokens    = set(('DEDENT', 'NOTEQUAL', 'STAR', 'LESS', 'EQUAL', 'RPAR', 'WHILE', 'WS', 'PRINT', 'RETURN', 'MINUS', 'DEF', 'RBRACE', 'GREATEREQUAL', 'LPAR', 'SEMI', 'NEWLINE', 'PLUS', 'COLON', 'RSQB', 'EQEQUAL', 'STRING', 'FOR', 'NUMBER', 'ELSE', 'NAME', 'SLASH', 'IN', 'LESSEQUAL', 'LSQB', 'IF', 'AND', 'LBRACE', 'INDENT', 'GREATER', 'NOT', 'COMMA', 'OR', 'ENDMARKER'))
_lexreflags   = 0
_lexliterals  = ''
_lexstateinfo = {'INITIAL': 'inclusive'}
_lexstatere   = {'INITIAL': [('(?P<t_NAME>[a-zA-Z_][a-zA-Z0-9_]*)|(?P<t_comment>[ ]*\\043[^\\n]*)|(?P<t_WS> [ \\t\\f]+ )|(?P<t_newline>\\n+)|(?P<t_LPAR>\\()|(?P<t_RPAR>\\))|(?P<t_LBRACE>\\{)|(?P<t_RBRACE>\\})|(?P<t_LSQB>\\[)|(?P<t_RSQB>\\])|(?P<t_NUMBER>((\\d+[jJ]|((\\d+\\.\\d*|\\.\\d+)([eE][-+]?\\d+)?|\\d+[eE][-+]?\\d+)[jJ])|((\\d+\\.\\d*|\\.\\d+)([eE][-+]?\\d+)?|\\d+[eE][-+]?\\d+)|(0[xX][\\da-fA-F]+[lL]?|0[bB][01]+[lL]?|(0[oO][0-7]+)|(0[0-7]*)[lL]?|[1-9]\\d*[lL]?)))|(?P<t_STRING>\\".*?\\")|(?P<t_NOTEQUAL>!=)|(?P<t_EQEQUAL>==)|(?P<t_STAR>\\*)|(?P<t_GREATEREQUAL>>=)|(?P<t_LESSEQUAL><=)|(?P<t_PLUS>\\+)|(?P<t_LESS><)|(?P<t_MINUS>-)|(?P<t_COMMA>,)|(?P<t_SLASH>/)|(?P<t_COLON>:)|(?P<t_GREATER>>)|(?P<t_SEMI>;)|(?P<t_EQUAL>=)', [None, ('t_NAME', 'NAME'), ('t_comment', 'comment'), ('t_WS', 'WS'), ('t_newline', 'newline'), ('t_LPAR', 'LPAR'), ('t_RPAR', 'RPAR'), ('t_LBRACE', 'LBRACE'), ('t_RBRACE', 'RBRACE'), ('t_LSQB', 'LSQB'), ('t_RSQB', 'RSQB'), (None, 'NUMBER'), None, None, None, None, None, None, None, None, None, None, None, (None, 'STRING'), (None, 'NOTEQUAL'), (None, 'EQEQUAL'), (None, 'STAR'), (None, 'GREATEREQUAL'), (None, 'LESSEQUAL'), (None, 'PLUS'), (None, 'LESS'), (None, 'MINUS'), (None, 'COMMA'), (None, 'SLASH'), (None, 'COLON'), (None, 'GREATER'), (None, 'SEMI'), (None, 'EQUAL')])]}
_lexstateignore = {'INITIAL': ''}
_lexstateerrorf = {'INITIAL': 't_error'}
_lexstateeoff = {}
