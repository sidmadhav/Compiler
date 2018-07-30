import smallpy_ast as myast
# Binop


class SmallPyStorage(object):
    def __init__(self):
        self.assigment_table = []
        self.symbol_table = []
        self.binop_table = []
        self.funcdef_table = []

    def check_funcdef_table(self):
        lst = [elem[0] for elem in self.funcdef_table]
        if len(lst) != len(set(lst)):
            tmp = dict((x, lst.count(x)) for x in set(lst) if lst.count(x) > 1)
            for func_name in tmp.keys():
                for func in self.funcdef_table:
                    if func_name == func[0]:
                        raise SyntaxError("Function " + func_name + " is defined multiple times at line " + str(func[2]))


class SmallPyAstParser(object):
    def __init__(self):
        self.smallpysemantic = SmallPySemantic(None)

    def assigment_parse(self, obj):

        if isinstance(obj, myast.Assigment) and isinstance(obj.target, myast.Name):

            if isinstance(obj.value[0], myast.Number) or isinstance(obj.value[0], myast.String):
                return [obj.target.value, obj.value[0].value, [obj.target.getname(), obj.value[0].getname()], obj.target.lineno]

            elif isinstance(obj.value[0], myast.Name):
                return [obj.target.value, obj.value[0].value, [obj.target.getname(), obj.value[0].getname()], obj.target.lineno]

            elif isinstance(obj.value[0], myast.List):
                tmp = []
                for i in range(len(obj.value[0].value)):
                    tmp.append(obj.value[0].value[i].value)
                return [obj.target.value, tmp, [obj.target.getname(), obj.value[0].getname()], obj.target.lineno]

            elif isinstance(obj.value[0], myast.Dict):
                tmp = {}
                for i in range((len(obj.value[0].keys))):
                    tmp.update({obj.value[0].keys[i].value: obj.value[0].body[i].value})
                return [obj.target.value, tmp, [obj.target.getname(), obj.value[0].getname()], obj.target.lineno]

            elif isinstance(obj.value[0], myast.BinOp):
                r = self.smallpysemantic.binop_parse_by_value(obj.value[0])
                return [obj.target.value, r, [obj.target.getname(), obj.value[0].getname()], obj.target.lineno]

            elif isinstance(obj.value[0], myast.Call):
                tmp = {'args': [], 'kwargs': []}
                for i in range(len(obj.value[0].args)):
                    tmp['args'].append(obj.value[0].args[i].value)
                for i in range(len(obj.value[0].keywords)):
                    tmp['kwargs'].append(obj.value[0].keywords[i].value.value)
                return [obj.target.value, [obj.value[0].func.value, tmp], [obj.target.getname(), obj.value[0].getname()], obj.target.lineno]

            elif isinstance(obj.value[0], myast.Subscript):
                tmp = [str(obj.value[0].slice.value), str(obj.value[0].value.value.value)]
                return [obj.target.value, tmp, [obj.target.getname(), obj.value[0].getname()], obj.target.lineno]

        elif isinstance(obj, myast.Assigment) and isinstance(obj.target, myast.Subscript): # do more

            if isinstance(obj.target.value.value, myast.BinOp):
                trgt = [obj.target.slice.value, self.smallpysemantic.binop_parse_by_value(obj.target.value.value)]
            else:
                trgt = [obj.target.slice.value, obj.target.value.value.value]

            if isinstance(obj.value[0], myast.Subscript):

                if isinstance(obj.value[0].value.value, myast.BinOp):
                    val = [obj.value[0].slice.value, self.smallpysemantic.binop_parse_by_value(obj.value[0].value.value)]
                else:
                    val = [obj.value[0].slice.value, obj.value[0].value.value.value]

                return [trgt, val, [obj.target.getname(), obj.value[0].getname()], obj.target.slice.lineno]

            elif isinstance(obj.value[0], myast.Name):
                return [trgt, obj.value[0].value, [obj.target.getname(), obj.value[0].getname()], obj.target.slice.lineno]


class SmallPySemantic(object):
    def __init__(self, parser):
        self.parser = parser

    def parse_statement(self, stmt, lineno=None):
        if isinstance(stmt, myast.Assigment):
            if isinstance(stmt.value[0], myast.BinOp):
                line = stmt.target.lineno

                expr_list = self.binop_parse_by_value(stmt.value[0])
                type_list = self.binop_parse_by_types(stmt.value[0])

                name_flag = False
                for item in type_list:
                    if item == 'Name':
                        name_flag = True

                if name_flag == True:
                    for i in range(len(type_list)):
                        if type_list[i] == 'Name':
                            if expr_list[i] in self.parser.storage.symbol_table:
                                tmp = []
                                for elem in self.parser.storage.assigment_table:
                                    if elem[0] == expr_list[i] and elem[3] < line:
                                        tmp.insert(0, elem)


                                expr_list[i] = tmp[0][1]
                                type_list[i] = tmp[0][2][1]

                            else:
                                raise SyntaxError("Symbol " + str(expr_list[i]) + " not found in storage")

                tmp_type_set = set(type_list)
                tmp_type_set.discard('+')
                tmp_type_set.discard('-')
                tmp_type_set.discard('*')
                tmp_type_set.discard('/')

                if len(tmp_type_set) > 1:
                    if 'Num' in tmp_type_set and 'Str' in tmp_type_set:
                        raise SyntaxError('semantic error at line - ' + str(line) + ' bad binary operation with Str and Num')
                    elif 'BinOp' in tmp_type_set:
                        pass
                    elif 'Iter' in tmp_type_set:
                        pass
                    else:
                        raise SyntaxError('sys error at line - ' + str(line) + ' smallpy_semantic -> SamllPySemantic -> def parse_statement -> assigment')

                print expr_list, type_list

            elif isinstance(stmt.target, myast.Subscript):
                self.parse_statement(stmt.target.value)  # check sub correct
                self.name_is_list(stmt.target.slice.value, stmt.target.slice.lineno)

            elif isinstance(stmt.value[0], myast.Call):
                self.parse_statement(stmt.value[0])

        elif isinstance(stmt, myast.Name):
            pass
        elif isinstance(stmt, myast.Number):
            pass
        elif isinstance(stmt, myast.String):
            pass
        elif isinstance(stmt, myast.Print):
            pass
        elif isinstance(stmt, myast.Tuple):
            pass
        elif isinstance(stmt, myast.List):
            pass
        elif isinstance(stmt, myast.If):
            for small_stmt in stmt.body:
                self.parse_statement(small_stmt)
            self.parse_statement(stmt.test)

        elif isinstance(stmt, myast.Else):
            pass
        elif isinstance(stmt, myast.Def):
            for arg in stmt.args.args:
                self.parser.storage.symbol_table.append(arg.value)
            for elem in stmt.body:
                self.parse_statement(elem)

        elif isinstance(stmt, myast.DefArgs):
            pass
        elif isinstance(stmt, myast.For):
            for elem in stmt.body:
                self.parse_statement(elem)
        elif isinstance(stmt, myast.Return):
            pass
        elif isinstance(stmt, myast.Call):
            line = stmt.func.lineno

            if len(stmt.args) > 0 and len(stmt.keywords) > 0:
                raise SyntaxError("semantic error at line " + str(line) + " positional argument with keyword")
            else:
                fnc = []
                for func in self.parser.storage.funcdef_table:
                    if func[0] == stmt.func.value:
                       fnc = func

                if len(fnc) == 0:
                    raise SyntaxError("Semantic Error Not Found Function " + stmt.func.value)
                else:
                    arguments = fnc[1].get('args')
                    defaults = fnc[1].get('default')
                    funcname = fnc[0]
                    funcline = fnc[2]
                    func_args_count = len(arguments)
                    call_args_count = len(stmt.args)
                    if call_args_count == 0 and len(stmt.keywords) > 0:
                        call_args_count = len(stmt.keywords)
                    if func_args_count != call_args_count:
                        raise SyntaxError("Semantic Error Wrong Arguments at line " + str(line))

        elif isinstance(stmt, myast.BinOp):
            line = stmt.left.lineno
            expr_list = self.compare_parse_by_value(stmt)
            type_list = self.compare_parse_by_types(stmt)

            name_flag = False
            for item in type_list:
                if item == 'Name':
                    name_flag = True

            if name_flag == True:
                for i in range(len(type_list)):
                    if type_list[i] == 'Name':
                        if expr_list[i] in self.parser.storage.symbol_table:
                            tmp = []
                            for elem in self.parser.storage.assigment_table:
                                if elem[0] == expr_list[i] and elem[3] < line:
                                    tmp.insert(0, elem)


                            expr_list[i] = tmp[0][1]
                            type_list[i] = tmp[0][2][1]

                        else:
                            raise SyntaxError("Symbol " + str(expr_list[i]) + " not found in storage")

            tmp_type_set = set(type_list)
            tmp_type_set.discard('+')
            tmp_type_set.discard('-')
            tmp_type_set.discard('*')
            tmp_type_set.discard('/')

            if len(tmp_type_set) > 1:
                if 'Num' in tmp_type_set and 'Str' in tmp_type_set:
                    raise SyntaxError('semantic error at line - ' + str(line) + ' bad binary operation with Str and Num')
                elif 'BinOp' in tmp_type_set:
                    pass
                elif 'Iter' in tmp_type_set:
                    pass
                else:
                    raise SyntaxError('sys error at line - ' + str(line) + ' smallpy_semantic -> SamllPySemantic -> def parse_statement -> assigment')

            print expr_list, type_list

        elif isinstance(stmt, myast.BoolOp):
            for elem in stmt.value:
                self.parse_statement(elem)

        elif isinstance(stmt, myast.UnOp):
            pass
        elif isinstance(stmt, myast.Compare):
            line = stmt.left.lineno

            expr_list = self.compare_parse_by_value(stmt)
            type_list = self.compare_parse_by_types(stmt)

            name_flag = False
            for item in type_list:
                if item == 'Name':
                    name_flag = True

            if name_flag == True:
                for i in range(len(type_list)):
                    if type_list[i] == 'Name':
                        if expr_list[i] in self.parser.storage.symbol_table:
                            tmp = []
                            for elem in self.parser.storage.assigment_table:
                                if elem[0] == expr_list[i] and elem[3] < line:
                                    tmp.insert(0, elem)

                            expr_list[i] = tmp[0][1]
                            type_list[i] = tmp[0][2][1]

                        else:
                            raise SyntaxError("Symbol " + str(expr_list[i]) + " not found in storage")

            tmp_type_set = set(type_list)
            tmp_type_set.discard('>')
            tmp_type_set.discard('<')
            tmp_type_set.discard('==')
            tmp_type_set.discard('!=')
            tmp_type_set.discard('<=')
            tmp_type_set.discard('>=')

            if len(tmp_type_set) > 1:
                if 'Num' in tmp_type_set and 'Str' in tmp_type_set:
                    raise SyntaxError('semantic error at line - ' + str(line) + ' bad bool operation with Str and Num')
                elif 'BinOp' in tmp_type_set:
                    pass
                else:
                    raise SyntaxError('sys error at line - ' + str(line) + ' smallpy_semantic -> SamllPySemantic -> def parse_statement -> assigment')

            print expr_list, type_list

        elif isinstance(stmt, myast.Subscript):
            pass
        elif isinstance(stmt, myast.Index):
            if isinstance(stmt.value, myast.BinOp):
                self.parse_statement(stmt.value)

        elif isinstance(stmt, myast.Expr):
            pass
        elif isinstance(stmt, myast.While):
            for small_stmt in stmt.body:
                self.parse_statement(small_stmt)
            self.parse_statement(stmt.test)

        elif isinstance(stmt, myast.Dict):
            pass
        elif isinstance(stmt, myast.Keyword):
            pass
        elif isinstance(stmt, myast.Arguments):
            pass
        elif isinstance(stmt, myast.ExtSlice):
            pass

    def name_is_list(self, arg, lineno):
        if arg in self.parser.storage.symbol_table:
            tmp = []
            for elem in self.parser.storage.assigment_table:
                if elem[0] == arg and elem[3] < lineno:
                    tmp.insert(0, elem)

            cur_value = tmp[0]
            if cur_value[2][1] == 'DefArg' or cur_value[2][1] == 'List':
                pass
            elif cur_value[2][1] == 'Name':
                print cur_value

    def binop_parse_by_types(self, obj):
        if isinstance(obj.left, myast.BinOp) or isinstance(obj.right, myast.BinOp):
            res = self.binop_parse_by_types2(obj)
            return res
        else:
            return [obj.left.getname(), obj.op, obj.right.getname()]

    def binop_parse_by_types2(self, obj):
        if isinstance(obj.right, myast.BinOp):
            r = self.binop_parse_by_types2(obj.right)
            r.insert(0, obj.left.getname())
            r.insert(1, obj.op)
            return r
        elif isinstance(obj.left, myast.BinOp):
            r = self.binop_parse_by_types2(obj.left)
            r.append(obj.op)
            r.append(obj.right.getname())
            return r
        else:
            return [obj.left.getname(), obj.op, obj.right.getname()]

    def binop_parse_by_value(self, obj):
        if isinstance(obj.left, myast.BinOp) or isinstance(obj.right, myast.BinOp):
            res = self.binop_parse_by_value2(obj)
            return res
        else:
            return [obj.left.value, obj.op, obj.right.value]

    def binop_parse_by_value2(self, obj):
        if isinstance(obj.right, myast.BinOp):
            r = self.binop_parse_by_value2(obj.right)
            r.insert(0, obj.left.value)
            r.insert(1, obj.op)
            return r
        elif isinstance(obj.left, myast.BinOp):
            r = self.binop_parse_by_value2(obj.left)
            r.append(obj.op)
            r.append(obj.right.value)
            return r
        else:
            return [obj.left.value, obj.op, obj.right.value]

    def compare_parse_by_value(self, obj):
        if isinstance(obj.left, myast.Compare) or isinstance(obj.right, myast.Compare):
            res = []
            return res
        else:
            if isinstance(obj.right, myast.Subscript) and isinstance(obj.left, myast.Subscript):
                self.parse_statement(obj.left.value)
                self.parse_statement(obj.right.value)
                self.name_is_list(obj.left.slice.value, obj.left.slice.lineno)
                self.name_is_list(obj.right.slice.value, obj.right.slice.lineno)
                return [[obj.left.slice.value, obj.left.value], obj.op, [obj.right.slice.value, obj.right.value]]
            else:
                return [obj.left.value, obj.op, obj.right.value]

    def compare_parse_by_types(self, obj):
        if isinstance(obj.left, myast.Compare) or isinstance(obj.right, myast.Compare):
            res = []
            return res
        else:
            return [obj.left.getname(), obj.op, obj.right.getname()]
