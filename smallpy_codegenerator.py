import llvm as ll
import llvm.core as lc
import llvm_cbuilder as lcb
import llvm.ee as le
import llvm_cbuilder.shortnames as C
from copy import copy
import smallpy_ast as Ast
import numbers
import smallpy_semantic


class ModuleArgs(object):
    def __init__(self):
        self.arguments = {}
        self.function = {}


class Variable(object):
    name = None
    value = None
    value_type = None


class SmallPyCodeGen(object):
    def __init__(self, program, storage):
        self.Program = program
        self.Storage = storage

        self.semantic_parser = smallpy_semantic.SmallPySemantic(parser=None)

        self.module_structure = ModuleArgs()

        self.int32 = lc.Type.int(32)
        self.double = lc.Type.double()

    def start_codegen(self, statement, builder, func):
        if isinstance(statement, Ast.Assigment):
            self.assigment_codegen(statement, builder, func)

        elif isinstance(statement, Ast.While):
            self.while_codegen(statement, builder, func)

        elif isinstance(statement, Ast.For):
            self.for_codegen(statement, builder, func)

        elif isinstance(statement, Ast.If):
            self.if_codegen(statement, builder, func)

        elif isinstance(statement, Ast.Print):
            self.print_codegen(statement, builder, func)

        elif isinstance(statement, Ast.Def):
            self.def_codegen(statement, builder, func)

        elif isinstance(statement, Ast.Return):
            self.return_codegen(statement, builder, func)

        elif isinstance(statement, Ast.Call):
            self.call_codegen(statement, builder, func)

    def call_codegen(self, statement, builder, func):
        print statement

    def return_codegen(self, statement, builder, func):
        if isinstance(statement.value, Ast.Name):
            builder.ret(builder.load(self.module_structure.arguments[statement.value.value]))
        elif isinstance(statement.value, Ast.BinOp):
            bin_op_stmt = statement.value
            l = self.semantic_parser.binop_parse_by_types(obj=bin_op_stmt)
            if 'Name' not in l:
                left = self.get_constant(value=bin_op_stmt.left.value)
                right = self.get_constant(value=bin_op_stmt.right.value)
                res = self.get_op(builder=builder, op=bin_op_stmt.op, lhs=left, rhs=right)
            else:
                if l[0] == 'Name':
                    left = builder.load(ptr=self.module_structure.arguments[bin_op_stmt.left.value])
                else:
                    left = self.get_constant(value=bin_op_stmt.left.value)
                if l[2] == 'Name':
                    right = builder.load(ptr=self.module_structure.arguments[bin_op_stmt.right.value])
                else:
                    right = self.get_constant(value=bin_op_stmt.right.value)
                res = self.get_op(builder=builder, op=bin_op_stmt.op, lhs=left, rhs=right)
            builder.ret(res)

    def def_codegen(self, statement, builder, func):

        f_type = lc.Type.function(return_ty=self.int32, param_tys=[self.int32, self.int32])
        func = lc.Function.new(module=self.module, func_ty=f_type, name=statement.name)
        args = []
        for arg in statement.args.args:
            args.append(arg.value)

        arg1, arg2 = func.args
        arg1.name = args[0]
        arg2.name = args[1]

        bb_entry = func.append_basic_block('entry')
        builder = lc.Builder.new(basic_block=bb_entry)

        tmp = builder.alloca(arg1.type, name=args[0])
        self.module_structure.arguments[args[0]] = tmp
        builder.store(arg1, tmp)

        tmp = builder.alloca(arg2.type, name=args[1])
        self.module_structure.arguments[args[1]] = tmp
        builder.store(arg2, tmp)

        for statement in statement.body:
            self.start_codegen(statement, builder, func)

        self.module_structure.function[func.name] = func

    def print_codegen(self, statement, builder, func):
        cb = lcb.CBuilder(function=func)
        ftm = cb.constant_string(string="string")
        cb.printf(ftm)

    def if_codegen(self, ifstmt, builder, func):
        icond = ifstmt.test
        ibody = ifstmt.body
        ielse = ifstmt.els
        bb_ibody = func.append_basic_block(name='if_body')
        if len(ielse) > 0:
            bb_ielse = func.append_basic_block(name='if_else')
        bb_iexit = func.append_basic_block(name='if_exit')

        cmp_stmt = self.semantic_parser.compare_parse_by_value(obj=icond)
        cmp_stmt_ty = self.semantic_parser.compare_parse_by_types(obj=icond)

        if 'Name' not in cmp_stmt_ty:
            left = self.get_constant(value=cmp_stmt[0])
            right = self.get_constant(value=cmp_stmt[2])
            res = self.get_cmp(builder, cmp_stmt[1], left, right)
        else:
            if cmp_stmt_ty[0] == 'Name':
                left = builder.load(self.module_structure.arguments[cmp_stmt[0]])
            else:
                left = self.get_constant(value=cmp_stmt[0])
            if cmp_stmt_ty[2] == 'Name':
                right = builder.load(self.module_structure.arguments[cmp_stmt[2]])
            else:
                right = self.get_constant(value=cmp_stmt[2])
            res = self.get_cmp(builder, cmp_stmt[1], left, right)

        bb = builder.basic_block

        builder.cbranch(res, bb_ibody, bb_iexit)
        builder.position_at_end(bb_ibody)
        l = []
        for statement in ibody:
            l.extend(self.parse_if_body(statement, builder=builder, func=func))

        if len(ielse) > 2:
            builder.position_at_end(bb_ielse)
            for statement in ielse:
                l.extend(self.parse_if_body(statement, builder=builder, func=func))

        builder.position_at_end(bb_iexit)
        philist = []
        for row in l:
            phi = builder.phi(ty=row[0].type)
            phi.add_incoming(builder.load(self.module_structure.arguments[row[1]]), bb)
            phi.add_incoming(row[0], row[2])
            philist.append([phi, row[1], row[0].type])

        for ph in philist:
            tmp = builder.alloca(ty=ph[2], name=ph[1])
            builder.store(ph[0], tmp)

    def parse_if_body(self, statement, builder, func):
        tmp_list = []
        if isinstance(statement, Ast.Assigment):
            assigment = statement
            if isinstance(assigment.target, Ast.Name):
                if isinstance(assigment.value[0], Ast.Number):
                    data = self.get_constant(value=assigment.value[0].value)
                    tmp_list.append([data, assigment.target.value, builder.basic_block])
        if isinstance(statement, Ast.Return):
            self.return_codegen(statement, builder, func)

        return tmp_list

    def for_codegen(self, forstmt, builder, func):
        fbody = forstmt.body
        phi_dict = []

        bb_fcond = func.append_basic_block(name='for_cond')
        bb_fbody = func.append_basic_block(name='for_body')
        bb_fexit = func.append_basic_block(name='for_exit')

        builder.branch(bblk=bb_fcond)
        builder.position_at_end(bblk=bb_fcond)

        zero = lc.Constant.int(ty=self.int32, value=0)
        tmp = builder.alloca(ty=zero.type, name=forstmt.target)
        self.module_structure.arguments[forstmt.target] = tmp
        builder.store(value=zero, ptr=tmp)

        left = builder.load(self.module_structure.arguments[forstmt.target])
        phi = builder.phi(ty=left.type, name=forstmt.target)
        phi.add_incoming(left, builder.basic_block)
        phi_dict.append([left.type, forstmt.target, phi])
        self.module_structure.arguments[forstmt.target] = phi
        left = phi
        right = self.get_constant(value=forstmt.iter.value)
        res = self.get_cmp(builder, '<', left, right)

        builder.cbranch(res, bb_fbody, bb_fexit)

        builder.position_at_end(bb_fbody)

        for statement in fbody:
            self.parse_while_body(statement, builder, func)

        add = self.get_op(builder, '+', left, lc.Constant.int(self.int32, 1))
        phi.add_incoming(add, bb_fbody)
        builder.branch(bb_fcond)

        builder.position_at_end(bb_fexit)
        for row in phi_dict:
            tmp = builder.alloca(ty=row[0], name=row[1])
            self.module_structure.arguments[row[1]] = tmp
            builder.store(value=row[2], ptr=tmp)

    def while_codegen(self, whilestmt, builder, func):
        wcond = whilestmt.test
        wbody = whilestmt.body
        phi_dict = []
        bb_wcond = func.append_basic_block(name='while_cond')
        bb_wbody = func.append_basic_block(name='while_body')
        bb_wexit = func.append_basic_block(name='while_exit')

        cmp_stmt = self.semantic_parser.compare_parse_by_value(obj=wcond)
        cmp_stmt_ty = self.semantic_parser.compare_parse_by_types(obj=wcond)
        builder.branch(bblk=bb_wcond)
        builder.position_at_end(bblk=bb_wcond)

        if 'Name' not in cmp_stmt_ty:
            left = self.get_constant(value=cmp_stmt[0])
            right = self.get_constant(value=cmp_stmt[2])
            res = self.get_cmp(builder, cmp_stmt[1], left, right)
        else:
            if cmp_stmt_ty[0] == 'Name':
                left = builder.load(self.module_structure.arguments[cmp_stmt[0]])
                phi = builder.phi(ty=left.type, name=cmp_stmt[0])
                phi.add_incoming(left, builder.basic_block)
                phi_dict.append([left.type, cmp_stmt[0], phi])
                self.module_structure.arguments[cmp_stmt[0]] = phi
                left = phi
            else:
                left = self.get_constant(value=cmp_stmt[0])
            if cmp_stmt_ty[2] == 'Name':
                right = builder.load(self.module_structure.arguments[cmp_stmt[2]])
                phi = builder.phi(ty=right.type, name=cmp_stmt[2])
                phi.add_incoming(right, builder.basic_block)
                phi_dict.append([right.type, cmp_stmt[0], phi])
                self.module_structure.arguments[cmp_stmt[2]] = phi
                right = phi
            else:
                right = self.get_constant(value=cmp_stmt[2])
            res = self.get_cmp(builder, cmp_stmt[1], left, right)

        builder.cbranch(res, bb_wbody, bb_wexit)

        builder.position_at_end(bb_wbody)
        for statement in wbody:
            self.parse_while_body(statement=statement, builder=builder, func=func)
        builder.branch(bb_wcond)

        builder.position_at_end(bb_wexit)
        for row in phi_dict:
            tmp = builder.alloca(ty=row[0], name=row[1])
            self.module_structure.arguments[row[1]] = tmp
            builder.store(value=row[2], ptr=tmp)

    def parse_while_body(self, statement, builder, func, phi_dict=None):
        if isinstance(statement, Ast.Assigment):
            assigment = statement
            if isinstance(assigment.target, Ast.Name):
                if isinstance(assigment.value[0], Ast.BinOp):
                    bin_op_stmt = assigment.value[0]
                    l = self.semantic_parser.binop_parse_by_types(obj=bin_op_stmt)
                    if 'Name' not in l:
                        left = self.get_constant(value=bin_op_stmt.left.value)
                        right = self.get_constant(value=bin_op_stmt.right.value)
                        res = self.get_op(builder=builder, op=bin_op_stmt.op, lhs=left, rhs=right)
                    else:
                        if l[0] == 'Name':
                            left = self.module_structure.arguments[bin_op_stmt.left.value]
                            if not isinstance(left, lc.PHINode):
                                left = builder.load(ptr=left)
                        else:
                            left = self.get_constant(value=bin_op_stmt.left.value)

                        if l[2] == 'Name':
                            right = self.module_structure.arguments[bin_op_stmt.right.value]
                            if not isinstance(right, lc.PHINode):
                                right = builder.load(ptr=right)
                        else:
                            right = self.get_constant(value=bin_op_stmt.right.value)
                        res = self.get_op(builder=builder, op=bin_op_stmt.op, lhs=left, rhs=right)

                    if assigment.target.value in self.module_structure.arguments:
                         val = self.module_structure.arguments[assigment.target.value]
                         if isinstance(val, lc.PHINode):
                             self.module_structure.arguments[assigment.target.value].add_incoming(res, builder.basic_block)
                         else:
                             tmp = builder.alloca(ty=left.type, name=assigment.target.value)
                             self.module_structure.arguments[assigment.target.value] = tmp
                             builder.store(value=res, ptr=tmp)
                    else:
                        tmp = builder.alloca(ty=left.type, name=assigment.target.value)
                        self.module_structure.arguments[assigment.target.value] = tmp
                        builder.store(value=res, ptr=tmp)

    def program_codegen(self, statement, builder, func):
        bad = [statement, builder, func]
        builder.ret(value=lc.Constant.int(ty=self.int32, value=0))
        ty_int32 = lc.Type.int(bits=32)
        ty_double = lc.Type.double()

        module = lc.Module.new(id='program')
        self.module = module
        func_main = lc.Function.new(module=module,
                                    func_ty=lc.Type.function(return_ty=ty_int32, param_tys=tuple()),
                                    name='MAIN')
        self.func_main = func_main
        f_main_bb_entry = func_main.append_basic_block(name='entry')
        f_main_builder = lc.Builder.new(basic_block=f_main_bb_entry)

        # str1 = "string value"
        data = lc.Constant.string(strval='string value')
        str1 = f_main_builder.alloca(ty=data.type, name='str1')
        f_main_builder.store(value=data, ptr=str1)

        # num1 = 12345
        data = lc.Constant.int(ty=ty_int32, value=12345)
        num1 = f_main_builder.alloca(ty=data.type, name='num1')
        f_main_builder.store(value=data, ptr=num1)

        # fln = 123.45
        data = lc.Constant.real(ty=ty_double, value=123.45)
        fln = f_main_builder.alloca(ty=data.type, name='fln')
        f_main_builder.store(value=data, ptr=fln)

        # r = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
        data = lc.Constant.array(ty=ty_int32,
                                 consts=[lc.Constant.int(ty=ty_int32, value=10),
                                         lc.Constant.int(ty=ty_int32, value=9),
                                         lc.Constant.int(ty=ty_int32, value=8),
                                         lc.Constant.int(ty=ty_int32, value=7),
                                         lc.Constant.int(ty=ty_int32, value=6),
                                         lc.Constant.int(ty=ty_int32, value=5),
                                         lc.Constant.int(ty=ty_int32, value=4),
                                         lc.Constant.int(ty=ty_int32, value=3),
                                         lc.Constant.int(ty=ty_int32, value=2),
                                         lc.Constant.int(ty=ty_int32, value=1),
                                         lc.Constant.int(ty=ty_int32, value=0)])
        r = f_main_builder.alloca(ty=data.type, name='r')
        f_main_builder.store(value=data, ptr=r)

        # ddd = {"key1": "value1", "key2": "value2"}
        data = lc.Constant.struct(consts=[lc.Constant.struct(consts=[lc.Constant.string('key1'),
                                                                     lc.Constant.string('value1')]),
                                          lc.Constant.struct(consts=[lc.Constant.string('key2'),
                                                                     lc.Constant.string('value2')])])
        ddd = f_main_builder.alloca(ty=data.type, name='ddd')
        f_main_builder.store(value=data, ptr=ddd)

        # res = 3 + 2
        data = f_main_builder.add(lhs=lc.Constant.int(ty=ty_int32, value=3),
                                  rhs=lc.Constant.int(ty=ty_int32, value=2))
        res = f_main_builder.alloca(ty=data.type, name='res')
        f_main_builder.store(value=data, ptr=res)

        # res = res * 3
        data = f_main_builder.add(lhs=f_main_builder.load(ptr=res),
                                  rhs=lc.Constant.int(ty=ty_int32, value=2))
        res = f_main_builder.alloca(ty=data.type, name='res')
        f_main_builder.store(value=data, ptr=res)

        # k = 1
        # s = 8
        # m = k + s
        data = lc.Constant.int(ty=ty_int32, value=1)
        k = f_main_builder.alloca(ty=ty_int32, name='k')
        f_main_builder.store(value=data, ptr=k)

        data = lc.Constant.int(ty=ty_int32, value=8)
        s = f_main_builder.alloca(ty=ty_int32, name='s')
        f_main_builder.store(value=data, ptr=s)

        data = f_main_builder.add(lhs=f_main_builder.load(k), rhs=f_main_builder.load(s))
        m = f_main_builder.alloca(ty=ty_int32, name='m')
        f_main_builder.store(value=data, ptr=m)

        # while k < 23:
        #     k = k + 6
        k = f_main_builder.load(k)
        f_main_bb_while_cond = func_main.append_basic_block('while_cond')
        f_main_bb_while_body = func_main.append_basic_block('while_body')
        f_main_bb_while_end = func_main.append_basic_block('while_end')

        f_main_builder.branch(bblk=f_main_bb_while_cond)
        f_main_builder.position_at_end(bblk=f_main_bb_while_cond)
        kphi = f_main_builder.phi(ty=k.type)
        kphi.add_incoming(k, f_main_bb_entry)
        cond = f_main_builder.icmp(ipred=lc.ICMPEnum.ICMP_SLT, lhs=kphi, rhs=lc.Constant.int(ty=ty_int32, value=23))
        f_main_builder.cbranch(if_value=cond, then_blk=f_main_bb_while_body, else_blk=f_main_bb_while_end)

        f_main_builder.position_at_end(bblk=f_main_bb_while_body)
        tmp = f_main_builder.add(lhs=kphi, rhs=lc.Constant.int(ty=k.type, value=6))
        kphi.add_incoming(tmp, f_main_bb_while_body)
        f_main_builder.branch(f_main_bb_while_cond)

        f_main_builder.position_at_end(bblk=f_main_bb_while_end)
        k = f_main_builder.alloca(ty=ty_int32, name='k')
        f_main_builder.store(value=kphi, ptr=k)

        # i = 1
        # for u in 5:
        #     i = u + 1
        data = lc.Constant.int(ty=ty_int32, value=1)
        i = f_main_builder.alloca(ty=ty_int32, name='i')
        f_main_builder.store(value=data, ptr=i)

        data = lc.Constant.int(ty=ty_int32, value=0)
        u = f_main_builder.alloca(ty=ty_int32, name='u')
        f_main_builder.store(value=data, ptr=u)
        u = f_main_builder.load(u)

        f_main_bb_for_cond = func_main.append_basic_block('for_cond')
        f_main_bb_for_body = func_main.append_basic_block('for_body')
        f_main_bb_for_end = func_main.append_basic_block('for_end')

        f_main_builder.branch(bblk=f_main_bb_for_cond)
        f_main_builder.position_at_end(bblk=f_main_bb_for_cond)
        uphi = f_main_builder.phi(ty=u.type)
        uphi.add_incoming(u, f_main_bb_while_end)
        cond = f_main_builder.icmp(ipred=lc.ICMPEnum.ICMP_SLT, lhs=uphi, rhs=lc.Constant.int(ty=ty_int32, value=5))
        f_main_builder.cbranch(if_value=cond, then_blk=f_main_bb_for_body, else_blk=f_main_bb_for_end)

        f_main_builder.position_at_end(bblk=f_main_bb_for_body)
        tmp = f_main_builder.add(lhs=uphi, rhs=lc.Constant.int(ty=ty_int32, value=1))
        f_main_builder.store(value=tmp, ptr=i)
        uphi.add_incoming(f_main_builder.add(lhs=uphi, rhs=lc.Constant.int(ty=ty_int32, value=1)), f_main_bb_for_body)
        f_main_builder.branch(bblk=f_main_bb_for_cond)

        f_main_builder.position_at_end(bblk=f_main_bb_for_end)
        u = f_main_builder.alloca(ty=ty_int32, name='u')
        f_main_builder.store(value=uphi, ptr=u)

        # if k > s:
        #     k = 10
        k = f_main_builder.load(k)
        s = f_main_builder.load(s)
        f_main_bb_if_true = func_main.append_basic_block('if_true')
        f_main_bb_if_exit = func_main.append_basic_block('if_exit')
        cp_bb = f_main_bb_if_exit
        cond = f_main_builder.icmp(ipred=lc.ICMPEnum.ICMP_SGT, lhs=k, rhs=s)
        f_main_builder.cbranch(if_value=cond, then_blk=f_main_bb_if_true, else_blk=f_main_bb_if_exit)

        f_main_builder.position_at_end(bblk=f_main_bb_if_true)
        tmpk = lc.Constant.int(ty=ty_int32, value=10)
        f_main_builder.branch(f_main_bb_if_exit)

        f_main_builder.position_at_end(bblk=f_main_bb_if_exit)
        kphi = f_main_builder.phi(ty=k.type)
        kphi.add_incoming(k, f_main_bb_for_end)
        kphi.add_incoming(tmpk, f_main_bb_if_true)
        k = f_main_builder.alloca(ty=ty_int32, name='k')
        f_main_builder.store(value=kphi, ptr=k)

        # if s < 1:
        #     s = 6
        f_main_bb_if_true = func_main.append_basic_block('if_true')
        f_main_bb_if_exit = func_main.append_basic_block('if_exit')
        cond = f_main_builder.icmp(ipred=lc.ICMPEnum.ICMP_SLT, lhs=s, rhs=lc.Constant.int(ty=ty_int32, value=1))
        f_main_builder.cbranch(if_value=cond, then_blk=f_main_bb_if_true, else_blk=f_main_bb_if_exit)

        f_main_builder.position_at_end(bblk=f_main_bb_if_true)
        tmps = lc.Constant.int(ty=ty_int32, value=1)
        f_main_builder.branch(f_main_bb_if_exit)

        f_main_builder.position_at_end(bblk=f_main_bb_if_exit)
        sphi = f_main_builder.phi(ty=s.type)
        sphi.add_incoming(s, cp_bb)
        sphi.add_incoming(tmps, f_main_bb_if_true)
        s = f_main_builder.alloca(ty=ty_int32, name='s')
        f_main_builder.store(value=sphi, ptr=s)

        # def sum(a, b):
        #     return a + b
        f_type = lc.Type.function(return_ty=ty_int32, param_tys=[ty_int32, ty_int32])
        f_sum = module.add_function(f_type, "sum")
        f_sum.args[0].name = "a"
        f_sum.args[1].name = "b"

        bb = f_sum.append_basic_block("entry")
        builder = lc.Builder.new(bb)

        tmp = builder.add(f_sum.args[0], f_sum.args[1], "tmp")
        builder.ret(tmp)

        # def cmp(g, h):
        #     if g > h:
        #         return g
        #     return h
        f_type = lc.Type.function(return_ty=ty_int32, param_tys=[ty_int32, ty_int32])
        f_cmp = module.add_function(f_type, "cmp")
        f_cmp.args[0].name = "g"
        f_cmp.args[1].name = "h"
        bb = f_cmp.append_basic_block("entry")
        builder = lc.Builder.new(bb)

        if_true = f_cmp.append_basic_block("if_true")
        if_exit = f_cmp.append_basic_block("if_exit")
        cond = builder.icmp(ipred=lc.ICMPEnum.ICMP_SGT, lhs=f_cmp.args[0], rhs=f_cmp.args[1])
        builder.cbranch(if_value=cond, then_blk=if_true, else_blk=if_exit)

        builder.position_at_end(bblk=if_true)
        builder.ret(value=f_cmp.args[0])

        builder.position_at_end(bblk=if_exit)
        builder.ret(value=f_cmp.args[1])

        # sm = sum(6, 9)
        # cm = cmp(k, s)
        sm = f_main_builder.alloca(ty=ty_int32, name='sm')
        call1 = f_main_builder.call(f_sum, [lc.Constant.int(ty=ty_int32, value=6), lc.Constant.int(ty=ty_int32, value=9)])
        # builder.store(value=call, ptr=sm)

        call2 = f_main_builder.call(f_cmp, [f_main_builder.load(k), f_main_builder.load(s)])
        cm = f_main_builder.alloca(ty=call2.type, name='cm')
        # builder.store(value=call, ptr=cm)

        f_main_builder.ret(value=call1)
        ee = le.ExecutionEngine.new(module)
        retval = ee.run_function(func_main, tuple())
        print module
        print "returned", retval.as_int()

    def assigment_codegen(self, assigment, builder, func):
        variable = Variable()
        if isinstance(assigment.target, Ast.Name):
            variable.name = assigment.target.value
            if isinstance(assigment.value[0], Ast.Number):
                variable.value = assigment.value[0].value
                if type(variable.value) == int:
                    variable.value_type = self.int32
                    data = lc.Constant.int(ty=self.int32, value=variable.value)
                elif type(variable.value) == float:
                    variable.value_type = self.double
                    data = lc.Constant.real(ty=self.double, value=variable.value)
                tmp = builder.alloca(ty=data.type, name=variable.name)
                self.module_structure.arguments[variable.name] = tmp
                builder.store(value=data, ptr=tmp)

            elif isinstance(assigment.value[0], Ast.String):
                variable.value = assigment.value[0].value
                data = lc.Constant.stringz(strval=variable.value)
                variable.value_type = data.type
                tmp = builder.alloca(ty=variable.value_type, name=variable.name)
                self.module_structure.arguments[variable.name] = tmp
                builder.store(value=data, ptr=tmp)

            elif isinstance(assigment.value[0], Ast.BinOp):
                bin_op_stmt = assigment.value[0]
                l = self.semantic_parser.binop_parse_by_types(obj=bin_op_stmt)
                if 'Name' not in l:
                    left = self.get_constant(value=bin_op_stmt.left.value)
                    right = self.get_constant(value=bin_op_stmt.right.value)
                    res = self.get_op(builder=builder, op=bin_op_stmt.op, lhs=left, rhs=right)
                else:
                    if l[0] == 'Name':
                        left = builder.load(ptr=self.module_structure.arguments[bin_op_stmt.left.value])
                    else:
                        left = self.get_constant(value=bin_op_stmt.left.value)
                    if l[2] == 'Name':
                        right = builder.load(ptr=self.module_structure.arguments[bin_op_stmt.right.value])
                    else:
                        right = self.get_constant(value=bin_op_stmt.right.value)
                    res = self.get_op(builder=builder, op=bin_op_stmt.op, lhs=left, rhs=right)

                tmp = builder.alloca(ty=left.type, name=variable.name)
                self.module_structure.arguments[variable.name] = tmp
                builder.store(value=res, ptr=tmp)

            elif isinstance(assigment.value[0], Ast.List):
                for row in self.Storage.assigment_table:
                    if row[0] == variable.name and row[-1] == assigment.target.lineno:
                        variable.value = row[1]
                        break
                consts = [self.get_constant(value=x) for x in variable.value]
                data = lc.Constant.array(ty=consts[0].type, consts=consts)
                tmp = builder.alloca(ty=data.type, name=variable.name)
                self.module_structure.arguments[variable.name] = tmp
                builder.store(value=data, ptr=tmp)

            elif isinstance(assigment.value[0], Ast.Dict):
                for row in self.Storage.assigment_table:
                    if row[0] == variable.name and row[-1] == assigment.target.lineno:
                        variable.value = row[1]
                        break
                consts = [self.get_struct(elem, variable.value[elem]) for elem in variable.value]
                data = lc.Constant.struct(consts=consts)
                tmp = builder.alloca(ty=data.type, name=variable.name)
                builder.store(value=data, ptr=tmp)

            elif isinstance(assigment.value[0], Ast.Call):
                args = []
                for elem in assigment.value[0].args:
                    if isinstance(elem, Ast.Name):
                        arg = builder.load(self.module_structure.arguments[elem.value])
                        args.append(arg)
                    elif isinstance(elem, Ast.Number):
                        arg = self.get_constant(value=elem.value)
                        args.append(arg)
                print assigment.target.value
                tmp = builder.alloca(self.int32, name=assigment.target.value)
                call = builder.call(self.module_structure.function[assigment.value[0].func.value], args)
                builder.store(call, tmp)

    def get_cmp(self, builder, op, lhs, rhs):
        if op == '<':
            return self.get_slt(builder, lhs, rhs)
        elif op == '==':
            return self.get_eq(builder, lhs, rhs)
        elif op == '!=':
            return self.get_ne(builder, lhs, rhs)
        elif op == '>':
            return self.get_sgt(builder, lhs, rhs)
        elif op == '>=' or op == '=>':
            return self.get_sgt(builder, lhs, rhs)
        elif op == '<=' or op == '=<':
            return self.get_sle(builder, lhs, rhs)
        elif op == 'or':
            return self.get_or(builder, lhs, rhs)
        elif op == 'and':
            return self.get_and(builder, lhs, rhs)

    def get_and(self, builder, lhs, rhs):
        return builder.and_(lhs, rhs)

    def get_or(self, builder, lhs, rhs):
        return builder.or_(lhs, rhs)

    def get_sle(self, builder, lhs, rhs):
        return builder.icmp(lc.ICMPEnum.ICMP_SLE, lhs, rhs)

    def get_sge(self, builder, lhs, rhs):
        return builder.icmp(lc.ICMPEnum.ICMP_SGE, lhs, rhs)

    def get_sgt(self, builder, lhs, rhs):
        return builder.icmp(lc.ICMPEnum.ICMP_SGT, lhs, rhs)

    def get_ne(self, builder, lhs, rhs):
        return builder.icmp(lc.ICMPEnum.ICMP_NE, lhs, rhs)

    def get_eq(self, builder, lhs, rhs):
        return builder.icmp(lc.ICMPEnum.ICMP_EQ, lhs, rhs)

    def get_slt(self, builder, lhs, rhs):
        return builder.icmp(lc.ICMPEnum.ICMP_SLT, lhs, rhs)

    def get_struct(self, value1, value2):
        return lc.Constant.struct(consts=[self.get_constant(value=value1), self.get_constant(value=value2)])

    def get_constant(self, value):
        if type(value) == int:
            return self.get_int(value=value)
        elif type(value) == float:
            return self.get_float(value=value)
        else:
            return self.get_string(value=value)

    def get_op(self, builder, op, lhs, rhs):
        if op == '+':
            return self.get_plus(builder=builder, lhs=lhs, rhs=rhs)
        elif op == '-':
            return self.get_minus(builder, lhs, rhs)
        elif op == '*':
            return self.get_mul(builder, lhs, rhs)
        elif op == '/':
            return self.get_div(builder, lhs, rhs)

    def get_string(self, value):
        return lc.Constant.stringz(strval=value)

    def get_int(self, value):
        return lc.Constant.int(ty=self.int32, value=value)

    def get_float(self, value):
        return lc.Constant.real(ty=self.double, value=value)

    def get_plus(self, builder,  lhs, rhs):
        return builder.add(lhs=lhs, rhs=rhs)

    def get_minus(self, builder,  lhs, rhs):
        return builder.sub(lhs=lhs, rhs=rhs)

    def get_mul(self, builder,  lhs, rhs):
        return builder.mul(lhs=lhs, rhs=rhs)

    def get_div(self, builder,  lhs, rhs):
        return builder.sdiv(lhs=lhs, rhs=rhs)

    def start(self):
        """Try hand-convert test.py"""
        module = lc.Module.new('program')
        self.module = module
        self.func_main = module.add_function(lc.Type.function(return_ty=self.int32, param_tys=tuple()), 'MAIN')
        basic_block = self.func_main.append_basic_block(name='entry')
        builder = lc.Builder.new(basic_block=basic_block)

        for pos, statement in enumerate(self.Program):
            if isinstance(statement, Ast.Def):
                tmp = self.Program.pop(pos)
                self.Program.insert(0, tmp)

        for statement in self.Program:
            self.start_codegen(statement, builder, self.func_main)
        print self.module
        self.program_codegen(0, builder, self.func_main)

        print self.module
        print self.module_structure.arguments
        print self.module_structure.function

        ee = le.ExecutionEngine.new(module)
        retval = ee.run_function(self.func_main, tuple())

        print "returned", retval.as_int()





