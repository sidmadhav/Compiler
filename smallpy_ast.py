class Program(object):
    def __init__(self, stmts):
        self.stmts = [stmts]

    def add(self, stmt):
        self.stmts.append(stmt)
        return self

class Node(object):
    fields = []

    def __init__(self, *args, **kwargs):
        assert len(self.fields) == len(args), \
            '%s takes %d arguments' % (self.__class__.__name__, len(self.fields))

        try:
            self.lineno = kwargs['lineno']
        except KeyError:
            self.lineno = None

        for i, field in enumerate(self.fields):
            setattr(self, field, args[i])

    def __repr__(self):
        return "%s(%s)" % (self.__class__.__name__,
                           ', '.join([repr(getattr(self, field))
                                    for field in self.fields]))

    def getname(self):
        return self.__class__.__name__

    def generic(self, with_lineno=False):
        values = {}
        if with_lineno:
            values['lineno'] = self.lineno
        for field in self.fields:
            value = getattr(self, field)
            if hasattr(value, 'generic'):
                value = value.generic(with_lineno)
            elif isinstance(value, list):
                items = value
                value = []
                for item in items:
                    if hasattr(item, 'generic'):
                        item = item.generic(with_lineno)
                    value.append(item)
            values[field] = value
        return(self.__class__.__name__, values)


def node(name, fields):
    attrs = {'fields': fields}
    return type(name, (Node,), attrs)

Assigment = node('Assigment', ['target', 'value'])
Name = node('Name', ['value'])
Number = node('Num', ['value'])
String = node('Str', ['value'])
Print = node('Print', ['value'])
Tuple = node('Tuple', ['value'])
List = node('List', ['value'])
If = node('If', ['test', 'body', 'els'])
Else = node('Else', ['value'])
Def = node('Def', ['name', 'args', 'body'])
DefArgs = node('DefArguments', ['args', 'default'])
For = node('For', ['target', 'iter', 'body'])
Return = node('Return', ['value'])
Call = node('Call', ['func', 'args', 'keywords']) # --
BinOp = node('BinOp', ['op', 'left', 'right'])
BoolOp = node('Boolean operation', ['op', 'value'])
UnOp = node('Unary operation', ['op', 'operand'])
Compare = node('Compare', ['op', 'left', 'right'])
Subscript = node('Sub', ['slice', 'value'])
Program1 = node('Program', ['stmts'])
Index = node('Index', ['value'])
Expr = node('Expr', ['value'])
While = node('While', ['test', 'body'])
Dict = node('Dict', ['keys', 'body'])
Keyword = node('Keyword', ['arg', 'value'])
Arguments = node('Arguments', ['args', 'keywords'])
ExtSlice = node('ExtSlice', ['dims'])
