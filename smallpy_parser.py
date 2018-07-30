import copy
import pprint
from ply import yacc

from smallpy_lexer import SmallPyLexer
import smallpy_ast as myast
import smallpy_semantic
import smallpy_codegenerator


class CommaSeparatedList(object):
    def __init__(self, values=None):
        self.values = values or []


class SymbolTableElement(object):
    def __init__(self, name, value, type, lineno):
        self.name = name
        self.value = value
        self.type = type
        self.lineno = lineno

    def __repr__(self):
        return self.type.upper() + ': ' + str(self.name) + ' = ' + str(self.value) + ' at line ' + str(self.lineno)


class SmallPyParser(object):
    def __init__(self):
        self.lexer = SmallPyLexer()
        self.lexer.build()
        self.tokens = self.lexer.tokens
        self.parser = yacc.yacc(module=self, start="program")

        self.storage = smallpy_semantic.SmallPyStorage()
        self.myast_parser = smallpy_semantic.SmallPyAstParser()
        self.semantic_check = smallpy_semantic.SmallPySemantic(self)

    def _ast_for_testlist(self, testlist):
        if isinstance(testlist, list):
            value = myast.Tuple()
        else:
            value = testlist
        return value

    def p_program(self, p):
        """program  :  file_input_star ENDMARKER"""
        p[0] = p[1]

    def p_file_input_star_1(self, p):
        """file_input_star : NEWLINE"""
        pass

    def p_file_input_star_2(self, p):
        """file_input_star : stmt"""
        p[0] = myast.Program(stmts=p[1])

    def p_file_input_star_3(self, p):
        """file_input_star : file_input_star stmt"""
        p[0] = p[1].add(p[2])

    def p_suite1(self, p):
        """suite : simple_stmt"""
        stmt = p[1]
        if isinstance(stmt, list):
            res = stmt
        else:
            res = [stmt]
        p[0] = res

    def p_suite2(self, p):
        """suite : NEWLINE INDENT stmt_list DEDENT"""
        p[0] = p[3]

    def p_stmt_list1(self, p):
        """stmt_list : stmt stmt_list"""
        stmt = p[1]
        if isinstance(stmt, list):
            res = stmt + p[2]
        else:
            res = [stmt] + p[2]
        p[0] = res

    def p_stmt_list2(self, p):
        """stmt_list : stmt"""
        stmt = p[1]
        if isinstance(stmt, list):
            res = stmt
        else:
            res = [stmt]
        p[0] = res

    def p_stmt(self, p):
        """stmt : simple_stmt
                 | compound_stmt"""
        p[0] = p[1]

    def p_simple_stmt1(self, p):
        """simple_stmt : small_stmt NEWLINE"""
        p[0] = p[1]

    def p_simple_stmt2(self, p):
        """simple_stmt : small_stmt_list NEWLINE"""
        p[0] = p[1]

    def p_small_stmt_list1(self, p):
        """small_stmt_list : small_stmt SEMI"""
        p[0] = [p[1]]

    def p_small_stmt_list2(self, p):
        """small_stmt_list : small_stmt small_stmt_list_list"""
        p[0] = [p[1]] + p[2]

    def p_small_stmt_list3(self, p):
        """small_stmt_list : small_stmt small_stmt_list_list SEMI"""
        p[0] = [p[1]] + p[2]

    def p_small_stmt_list_list1(self, p):
        """small_stmt_list_list : SEMI small_stmt"""
        p[0] = [p[2]]

    def p_small_stmt_list_list2(self, p):
        """small_stmt_list_list : small_stmt_list_list SEMI small_stmt"""
        p[0] = p[1] + [p[3]]

    def p_small_stmt1(self, p):
        """small_stmt : expr_stmt
                       | print_stmt
                       | return_stmt"""
        p[0] = p[1]

    def p_print_stmt1(self, p):
        """print_stmt : PRINT"""
        p[0] = myast.Print(lineno=p.lineno)

    def p_print_stmt2(self, p):
        """print_stmt : PRINT test"""
        p[0] = myast.Print(p[2], lineno=p.lineno(2))

    def p_print_stmt3(self, p):
        """print_stmt : PRINT print_list"""
        p[0] = myast.Print(p[2], lineno=p.lineno(2))

    def p_print_list1(self, p):
        """print_list : test COMMA"""
        p[0] = [p[1], None]
        
    def p_print_list2(self, p):
        """print_list : test print_list_list"""
        p[0] = [p[1]] + p[2]

    def p_print_list3(self, p):
        """print_list : test print_list_list COMMA"""
        p[0] = [p[1]] + p[2] + [None]

    def p_print_list_list1(self, p):
        """print_list_list : COMMA test"""
        p[0] = [p[2]]

    def p_print_list_list2(self, p):
        """print_list_list : print_list_list COMMA test"""
        p[0] = p[1] + [p[3]]

    def p_return_stmt1(self, p):
        """return_stmt : RETURN"""
        p[0] = myast.Return()

    def p_return_stmt2(self, p):
        """return_stmt : RETURN testlist"""
        value = self._ast_for_testlist(p[2])
        p[0] = myast.Return(value, lineno=p.lineno(2))

    def p_expr_stmt1(self, p):                  # !!!
        """expr_stmt : testlist"""
        p[0] = myast.Expr(self._ast_for_testlist(p[1]), lineno=p.lineno(1))

    def p_expr_stmt3(self, p):
        """expr_stmt : testlist equal_list"""
        p[0] = myast.Assigment(p[1], p[2], lineno=p.lineno(1))

        res = self.myast_parser.assigment_parse(p[0])
        self.storage.assigment_table.append(res)
        if isinstance(p[0].target, myast.Name):
            if p[0].target.value not in self.storage.symbol_table:
                self.storage.symbol_table.append(p[0].target.value)

    def p_equal_list1(self, p):
        """equal_list : EQUAL testlist"""
        p[0] = [p[2]]

    def p_equal_list2(self, p):
        """equal_list : EQUAL testlist equal_list"""
        p[0] = [p[2]] + p[3]

    def p_testlist1(self, p):
        """testlist : test"""
        p[0] = p[1]

    def p_testlist2(self, p):
        """testlist : test COMMA"""
        p[0] = [p[1]]

    def p_testlist3(self, p):
        """testlist : test testlist_list"""
        p[0] = [p[1]] + p[2]

    def p_testlist4(self, p):
        """testlist : test testlist_list COMMA"""
        p[0] = [p[1]] + p[2]

    def p_testlist_list1(self, p):
        """testlist_list : COMMA test"""
        p[0] = [p[2]]

    def p_testlist_list2(self, p):
        """testlist_list : testlist_list COMMA test"""
        p[0] = p[1] + [p[3]]

    def p_compound_stmt(self, p):
        """compound_stmt : if_stmt
                          | while_stmt
                          | for_stmt
                          | funcdef"""
        p[0] = p[1]

    def p_if_stmt1(self, p):
        """if_stmt : IF test COLON suite"""
        p[0] = myast.If(p[2], p[4], [], lineno=p.lineno(2))

    def p_if_stmt3(self, p):
        """if_stmt : IF test COLON suite else_stmt"""
        p[0] = myast.If(p[2], p[4], p[5], lineno=p.lineno(2))

    def p_else_stmt(self, p):
        """else_stmt : ELSE COLON suite"""
        p[0] = p[3]

    def p_while_stmt1(self, p):
        """while_stmt : WHILE test COLON suite"""
        p[0] = myast.While(p[2], p[4], lineno=p.lineno(2))

    def p_for_stmt1(self, p):
        """for_stmt : FOR NAME IN testlist COLON suite"""
        iter = self._ast_for_testlist(p[4])
        p[0] = myast.For(p[2], iter, p[6], lineno=p.lineno(2))
        if p[2] not in self.storage.symbol_table:
            self.storage.symbol_table.append(p[2])
        self.storage.assigment_table.append([p[2], None, ['Name', 'Iter'], p.lineno(2)])

    def p_funcdef(self, p):
        """funcdef : DEF NAME parameters COLON suite"""
        p[0] = myast.Def(p[2], p[3], p[5], lineno=p.lineno(2))

        tmp = {'args': [], 'default': []}
        for i in range(len(p[3].args)):
            tmp['args'].append(p[3].args[i].value)
        for i in range(len(p[3].default)):
            tmp['default'].append(p[3].default[i].value)
        self.storage.funcdef_table.append([p[2], tmp, p.lineno(2)])

        if len(tmp['args']) > 0:
            for arg in tmp['args']:
                self.storage.assigment_table.append([arg, None, ['Name', 'DefArg'], p.lineno(2)])

    def p_parameters1(self, p):
        """parameters : LPAR RPAR"""
        p[0] = myast.DefArgs([], [])

    def p_parameters2(self, p):
        """parameters : LPAR varargslist RPAR"""
        p[0] = p[2]

    def p_test1(self, p):
        """test : or_test"""
        p[0] = p[1]

    def p_or_test1(self, p):
        """or_test : and_test"""
        p[0] = p[1]

    def p_or_test2(self, p):
        """or_test : and_test or_test_list"""
        p[0] = myast.BoolOp('OR', [p[1]] + p[2], lineno=p.lineno(2))

    def p_or_test_list1(self, p):
        """or_test_list : OR and_test"""
        p[0] = [p[2]]

    def p_or_test_list2(self, p):
        """or_test_list : or_test_list OR and_test"""
        p[0] = p[1] + [p[3]]

    def p_and_test1(self, p):
        """and_test : not_test"""
        p[0] = p[1]

    def p_and_test2(self, p):
        """and_test : not_test and_test_list"""
        p[0] = myast.BoolOp('AND', [p[1]] + p[2], lineno=p.lineno(2))

    def p_and_test_list1(self, p):
        """and_test_list : AND not_test"""
        p[0] = [p[2]]

    def p_and_test_list2(self, p):
        """and_test_list : and_test_list AND not_test"""
        p[0] = p[1] + [p[3]]

    def p_not_test(self, p):
        """not_test : comparison"""
        p[0] = p[1]

    def p_not_test2(self, p):
        """not_test : NOT not_test"""
        p[0] = myast.UnOp('NOT', p[2], lineno=p.lineno(2))

    def p_comparison1(self, p):
        """comparison : arith_expr"""
        p[0] = p[1]

    def p_comparison2(self, p):
        """comparison : arith_expr comparison_list"""
        left = p[1]
        ops = p[2][0][0]
        comparators =p[2][0][1]
        p[0] = myast.Compare(ops, left, comparators, lineno=p.lineno(2))

    def p_comparison_list1(self, p):
        """comparison_list : comp_op arith_expr"""
        p[0] = [[p[1], p[2]]]

    def p_comparison_list2(self, p):
        """comparison_list : comparison_list comp_op arith_expr"""
        p[0] = p[1] + [[p[2], p[3]]]

    def p_comp_op1(self, p):
        """comp_op : LESS
                   | GREATER
                   | EQEQUAL
                   | GREATEREQUAL
                   | LESSEQUAL
                   | NOTEQUAL"""
        p[0] = p[1]

    def p_arith_expr1(self, p):
        """arith_expr : term"""
        p[0] = p[1]

    def p_arith_expr2(self, p):
        """arith_expr : term arith_expr_list"""
        node = p[1]
        for op, right in p[2]:
            node = myast.BinOp(op, node, right, lineno=p.lineno(2))
        p[0] = node
        # b = self.myast_parser.binop_parse(p[0])
        # t = smallpy_semantic.SmallPySemantic().binop_parse_by_types(p[0])
        # smallpy_semantic.SmallPySemantic().check_semantic_errors_when_BinOp(b_list=b, t_list=t, parser=self)

    def p_arith_expr_list1(self, p):
        """arith_expr_list : arith_op"""
        p[0] = [p[1]]

    def p_arith_expr_list2(self, p):
        """arith_expr_list : arith_expr_list arith_op"""
        p[0] = p[1] + [p[2]]

    def p_arith_op1(self, p):
        """arith_op : PLUS term"""
        p[0] = ['+', p[2]]

    def p_arith_op2(self, p):
        """arith_op : MINUS term"""
        p[0] = ['-', p[2]]

    def p_term1(self, p):
        """term : power"""
        p[0] = p[1]

    def p_term2(self, p):
        """term : power term_list"""
        node = p[1]
        for op, right in p[2]:
            node = myast.BinOp(op, node, right, lineno=p.lineno(2))
        p[0] = node
        # r = self.myast_parser.binop_parse(p[0])
        # t = smallpy_semantic.SmallPySemantic().binop_parse_by_types(p[0])
        # print r, t

    def p_term_list1(self, p):
        """term_list : term_op"""
        p[0] = [p[1]]
        
    def p_term_list2(self, p):
        """term_list : term_list term_op"""
        p[0] = p[1] + [p[2]]

    def p_term_op1(self, p):
        """term_op : STAR power"""
        p[0] = ['*', p[2]]

    def p_term_op2(self, p):
        """term_op : SLASH power"""
        p[0] = ['/', p[2]]

    def p_power1(self, p):
        """power : atom"""
        p[0] = p[1]

    def p_power3(self, p):
        """power : atom power_list"""
        root = p[1]
        for node in p[2]:
            if isinstance(node, myast.Call):
                node.func = root
                node = myast.Call(root, getattr(node, 'args'), getattr(node, 'keywords'), lineno=p.lineno(2))
            elif isinstance(node, myast.Subscript):
                node = myast.Subscript(root, getattr(node, 'slice'), lineno=p.lineno(2))
            else:
                raise TypeError('Unexpected trailer node: %s' % node)
            root = node
        p[0] = root

    def p_power_list1(self, p):
        """power_list : trailer"""
        p[0] = [p[1]]

    def p_power_list2(self, p):
        """power_list : power_list trailer"""
        p[0] = p[1] + [p[2]]

    def p_atom1(self, p):
        """atom : LPAR RPAR"""
        p[0] = myast.Tuple()

    def p_atom3(self, p):
        """atom : LPAR testlist_comp RPAR"""
        info = p[2]
        if isinstance(info, CommaSeparatedList):
            node = myast.Tuple(info.values, lineno=p.lineno(2))
        else:
            node = info
        p[0] = node

    def p_atom4(self, p):
        """atom : LSQB RSQB"""
        p[0] = myast.List()

    def p_atom5(self, p):
        """atom : LSQB listmaker RSQB"""
        info = p[2]
        if isinstance(info, CommaSeparatedList):
            node = myast.List(info.values, lineno=p.lineno(2))
        else:
            raise TypeError('Unexpected node for listmaker: %s' % info)
        p[0] = node

    def p_atom6(self, p):
        """atom : LBRACE RBRACE"""
        p[0] = myast.Dict([], [])

    def p_atom7(self, p):               # !!!
        """atom : LBRACE dictorsetmaker RBRACE"""
        info = p[2]
        if isinstance(info, CommaSeparatedList):
            keys, values = zip(*info.values)
            node = myast.Dict(list(keys), list(values), lineno=p.lineno(2))
        else:
            raise TypeError('Unexpected node for dictorsetmaker: %s' % info)
        p[0] = node

    def p_atom8(self, p):
        """atom : NAME"""
        p[0] = myast.Name(p[1], lineno=p.lineno(1))

    def p_atom9(self, p):
        """atom : NUMBER"""
        p[0] = myast.Number(eval(p[1]), lineno=p.lineno(1))

    def p_atom10(self, p):
        """atom : atom_string_list"""
        p[0] = myast.String(p[1], lineno=p.lineno(1))

    def p_atom_string_list1(self, p):
        """atom_string_list : STRING"""
        p[0] = p[1]

    def p_atom_string_list2(self, p):
        """atom_string_list : atom_string_list STRING"""
        p[0] = p[1] + p[2]

    def p_listmaker2(self, p):
        """listmaker : test"""
        p[0] = CommaSeparatedList(values=[p[1]])

    def p_listmaker3(self, p):
        """listmaker : test COMMA"""
        p[0] = CommaSeparatedList(values=[p[1]])

    def p_listmaker4(self, p):
        """listmaker : test listmaker_list"""
        values = [p[1]] + p[2]
        p[0] = CommaSeparatedList(values=values)

    def p_listmaker5(self, p):
        """listmaker : test listmaker_list COMMA"""
        values = [p[1]] + p[2]
        p[0] = CommaSeparatedList(values=values)

    def p_listmaker_list1(self, p):
        """listmaker_list : COMMA test"""
        p[0] = [p[2]]

    def p_listmaker_list2(self, p):
        """listmaker_list : listmaker_list COMMA test"""
        p[0] = p[1] + [p[3]]

    def p_testlist_comp2(self, p):
        """testlist_comp : test"""
        p[0] = p[1]

    def p_testlist_comp3(self, p):
        """testlist_comp : test COMMA"""
        p[0] = CommaSeparatedList(values=[p[1]])

    def p_testlist_comp4(self, p):
        """testlist_comp : test testlist_comp_list"""
        values = [p[1]] + p[2]
        p[0] = CommaSeparatedList(values=values)

    def p_testlist_comp5(self, p):
        """testlist_comp : test testlist_comp_list COMMA"""
        values = [p[1]] + p[2]
        p[0] = CommaSeparatedList(values=values)

    def p_testlist_comp_list1(self, p):
        """testlist_comp_list : COMMA test"""
        p[0] = [p[2]]

    def p_testlist_comp_list2(self, p):
        """testlist_comp_list : testlist_comp_list COMMA test"""
        p[0] = p[1] + [p[3]]

    def p_trailer1(self, p):
        """trailer : LPAR RPAR"""
        p[0] = myast.Call(None, [], [])

    def p_trailer2(self, p):
        """trailer : LPAR arglist RPAR"""
        p[0] = myast.Call(None, getattr(p[2], 'args'), getattr(p[2], 'keywords'), lineno=p.lineno(2))

    def p_trailer3(self, p):
        """trailer : LSQB subscriptlist RSQB"""
        p[0] = myast.Subscript(p[2], None, lineno=p.lineno(2))

    def p_subscriptlist1(self, p):
        """subscriptlist : subscript"""
        p[0] = p[1]

    def p_subscriptlist2(self, p):
        """subscriptlist : subscript COMMA"""
        p[0] = myast.ExtSlice([p[1]], lineno=p.lineno(1))

    def p_subscriptlist3(self, p):
        """subscriptlist : subscript subscriptlist_list"""
        p[0] = myast.ExtSlice([p[1]] + p[2], lineno=p.lineno(2))

    def p_subscriptlist4(self, p):
        """subscriptlist : subscript subscriptlist_list COMMA"""
        p[0] = myast.ExtSlice([p[1]] + p[2], lineno=p.lineno(2))

    def p_subscriptlist_list1(self, p):
        """subscriptlist_list : COMMA subscript"""
        p[0] = [p[2]]

    def p_subscript_list2(self, p):
        """subscriptlist_list : subscriptlist_list COMMA subscript"""
        p[0] = p[1] + [p[3]]

    def p_subcript2(self, p):
        """subscript : test"""
        p[0] = myast.Index(p[1], lineno=p.lineno(1))
        
    def p_dictorsetmaker2(self, p):
        """dictorsetmaker : test COLON test"""
        values = [(p[1], p[3])]
        p[0] = CommaSeparatedList(values=values)

    def p_dictorsetmaker3(self, p):
        """dictorsetmaker : test COLON test COMMA"""
        values = [(p[1], p[3])]
        p[0] = CommaSeparatedList(values=values)

    def p_dictorsetmaker4(self, p):
        """dictorsetmaker : test COLON test dosm_colon_list"""
        values = [(p[1], p[3])] + p[4]
        p[0] = CommaSeparatedList(values=values)

    def p_dictorsetmaker5(self, p):
        """dictorsetmaker : test COLON test dosm_colon_list COMMA"""
        values = [(p[1], p[3])] + p[4]
        p[0] = CommaSeparatedList(values=values)

    def p_dosm_colon_list1(self, p):
        """dosm_colon_list : COMMA test COLON test"""
        p[0] = [(p[2], p[4])]

    def p_dosm_colon_list2(self, p):
        """dosm_colon_list : dosm_colon_list COMMA test COLON test"""
        p[0] = p[1] + [(p[3], p[5])]

    def p_arglist1(self, p):
        """arglist : argument"""
        if isinstance(p[1], myast.Keyword):
            p[0] = myast.Arguments([], [p[1]], lineno=p.lineno(1))
        else:
            p[0] = myast.Arguments([p[1]], [], lineno=p.lineno(1))

    def p_arglist2(self, p):
        """arglist : argument COMMA"""
        if isinstance(p[1], myast.Keyword):
            p[0] = myast.Arguments([], [p[1]], lineno=p.lineno(1))
        else:
            p[0] = myast.Arguments([p[1]], [], lineno=p.lineno(1))

    def p_arglist6(self, p):
        """arglist : arglist_list argument"""
        args = []
        kws = []
        for arg in (p[1] + [p[2]]):
            if isinstance(arg, myast.Keyword):
                kws.append(arg)
            else:
                args.append(arg)
        p[0] = myast.Arguments(args, kws, lineno=p.lineno(2))

    def p_arglist7(self, p):
        """arglist : arglist_list argument COMMA"""
        args = []
        kws = []
        for arg in (p[1] + [p[2]]):
            if isinstance(arg, myast.Keyword):
                kws.append(arg)
            else:
                args.append(arg)
        p[0] = myast.Arguments(args, kws, lineno=p.lineno(2))

    def p_arglist_list1(self, p):
        """arglist_list : argument COMMA"""
        p[0] = [p[1]]

    def p_arglist_list2(self, p):
        """arglist_list : arglist_list argument COMMA"""
        p[0] = p[1] + [p[2]]

    def p_argument1(self, p):
        """argument : test"""
        p[0] = p[1]

    def p_argument3(self, p):               # !!!
        """argument : test EQUAL test"""
        p[0] = myast.Keyword(p[1], p[3], lineno=p.lineno(1))

    def p_varargslist4(self, p):
        """varargslist : fpdef"""
        p[0] = myast.DefArgs([p[1]], [], lineno=p.lineno(1))

    def p_varargslist5(self, p):
        """varargslist : fpdef COMMA"""
        p[0] = myast.DefArgs([p[1]], [], lineno=p.lineno(1))

    def p_varargslist9(self, p):
        """varargslist : fpdef varargslist_list"""
        list_args, defaults = p[2]
        args = [p[1]] + list_args
        p[0] = myast.DefArgs(args, defaults, lineno=p.lineno(2))

    def p_varargslist10(self, p):
        """varargslist : fpdef varargslist_list COMMA"""
        list_args, defaults = p[2]
        args = [p[1]] + list_args
        p[0] = myast.DefArgs(args, defaults, lineno=p.lineno(2))

    def p_varargslist14(self, p):
        """varargslist : fpdef EQUAL test"""
        p[0] = myast.DefArgs([p[1]], [p[3]], lineno=p.lineno(1))

    def p_varargslist15(self, p):
        """varargslist : fpdef EQUAL test COMMA"""
        p[0] = myast.DefArgs([p[1]], [p[3]], lineno=p.lineno(1))

    def p_varargslist19(self, p):
        """varargslist : fpdef EQUAL test varargslist_list"""
        list_args, list_defaults = p[4]
        args = [p[1]] + list_args
        defaults = [p[3]] + list_defaults
        p[0] = myast.DefArgs(args, defaults, lineno=p.lineno(1))

    def p_varargslist20(self, p):
        """varargslist : fpdef EQUAL test varargslist_list COMMA"""
        list_args, list_defaults = p[4]
        args = [p[1]] + list_args
        defaults = [p[3]] + list_defaults
        p[0] = myast.DefArgs(args, defaults, lineno=p.lineno(1))

    def p_varargslist_list1(self, p):
        """varargslist_list : COMMA fpdef"""
        p[0] = ([p[2]], [])

    def p_varargslist_list2(self, p):
        """varargslist_list : COMMA fpdef EQUAL test"""
        p[0] = ([p[2]], [p[4]])

    def p_varargslist_list3(self, p):
        """varargslist_list : varargslist_list COMMA fpdef"""
        list_args, list_defaults = p[1]
        args = list_args + [p[3]]
        p[0] = (args, list_defaults)

    def p_varargslist_list4(self, p):
        """varargslist_list : varargslist_list COMMA fpdef EQUAL test"""
        list_args, list_defaults = p[1]
        args = list_args + [p[3]]
        defaults = list_defaults + [p[5]]
        p[0] = (args, defaults)

    def p_fpdef1(self, p):
        """fpdef : NAME"""
        p[0] = myast.Name(p[1], lineno=p.lineno(1))

    def p_fpdef2(self, p):
        """fpdef : LPAR fplist RPAR"""
        p[0] = p[2]

    def p_fplist1(self, p):
        """fplist : fpdef"""
        p[0] = p[1]

    def p_fplist2(self, p):
        """fplist : fpdef COMMA"""
        p[0] = myast.Tuple([p[1]], lineno=p.lineno(1))

    def p_fplist3(self, p):
        """fplist : fpdef fplist_list"""
        p[0] = myast.Tuple([p[1]] + p[2], lineno=p.lineno(2))

    def p_fplist4(self, p):
        """fplist : fpdef fplist_list COMMA"""
        p[0] = myast.Tuple([p[1]] + p[2], lineno=p.lineno(2))

    def p_fplist_list1(self, p):
        """fplist_list : COMMA fpdef"""
        p[0] = [p[2]]

    def p_fplist_list2(self, p):
        """fplist_list : fplist_list COMMA fpdef"""
        p[0] = p[1] + [p[3]]

    def p_error(self, p):
        if p:
            raise SyntaxError('before: ' + str(p.value) + ' at line ' + str(p.lineno))
        else:
            raise SyntaxError('At end of input')

    def parse(self, source):
        parse_tree = self.parser.parse(source, lexer=self.lexer)

        return parse_tree


if __name__ == "__main__":
    z = SmallPyParser()
    fname = 'test/test'
    fp = open(fname, 'r')
    t = z.parse(fp.read())
    for item in t.stmts:
        if hasattr(item, 'generic'):
            item = item.generic()
        pprint.pprint(item)
    fp.close()

    print '\n\n\n'

    print z.storage.symbol_table
    print '\n'

    for elem in z.storage.assigment_table:
        print elem

    print '\n'
    z.storage.check_funcdef_table()
    for elem in z.storage.funcdef_table:
        print elem

    print '\n'
    for stmt in t.stmts:
        z.semantic_check.parse_statement(stmt=stmt)

    print '\n'
    smallpy_codegenerator.SmallPyCodeGen(program=copy.copy(t.stmts), storage=z.storage).start()
