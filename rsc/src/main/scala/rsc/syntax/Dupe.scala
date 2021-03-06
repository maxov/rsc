// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.syntax

trait Dupe {
  implicit class DupeTreeOps[T <: Tree](tree: T) {
    def dupe: T = {
      val result = tree match {
        case AnonId() =>
          AnonId()
        case Case(pat, cond, stats) =>
          val pat1 = pat.dupe
          val cond1 = cond.map(_.dupe)
          val stats1 = stats.map(_.dupe)
          Case(pat1, cond1, stats1)
        case CtorId() =>
          CtorId()
        case DefnClass(mods, id, tparams, ctor, earlies, inits, self, stats) =>
          val mods1 = mods.dupe
          val id1 = id.dupe
          val tparams1 = tparams.map(_.dupe)
          val ctor1 = ctor.dupe
          val earlies1 = earlies.map(_.dupe)
          val inits1 = inits.map(_.dupe)
          val self1 = self.map(_.dupe)
          val ss1 = stats.map(_.dupe)
          DefnClass(mods1, id1, tparams1, ctor1, earlies1, inits1, self1, ss1)
        case DefnField(mods, id, tpt, rhs) =>
          val mods1 = mods.dupe
          val id1 = id.dupe
          val tpt1 = tpt.map(_.dupe)
          val rhs1 = rhs.map(_.dupe)
          DefnField(mods1, id1, tpt1, rhs1)
        case DefnMacro(mods, id, tparams, paramss, ret, rhs) =>
          val mods1 = mods.dupe
          val id1 = id.dupe
          val tparams1 = tparams.map(_.dupe)
          val paramss1 = paramss.map(_.map(_.dupe))
          val ret1 = ret.map(_.dupe)
          val rhs1 = rhs.dupe
          DefnMacro(mods1, id1, tparams1, paramss1, ret1, rhs1)
        case DefnMethod(mods, id, tparams, paramss, ret, rhs) =>
          val mods1 = mods.dupe
          val id1 = id.dupe
          val tparams1 = tparams.map(_.dupe)
          val paramss1 = paramss.map(_.map(_.dupe))
          val ret1 = ret.map(_.dupe)
          val rhs1 = rhs.map(_.dupe)
          DefnMethod(mods1, id1, tparams1, paramss1, ret1, rhs1)
        case DefnObject(mods, id, earlies, inits, self, stats) =>
          val mods1 = mods.dupe
          val id1 = id.dupe
          val earlies1 = earlies.map(_.dupe)
          val inits1 = inits.map(_.dupe)
          val self1 = self.map(_.dupe)
          val stats1 = stats.map(_.dupe)
          DefnObject(mods1, id1, earlies1, inits1, self1, stats1)
        case DefnPackage(pid, stats) =>
          val pid1 = pid.dupe
          val stats1 = stats.map(_.dupe)
          DefnPackage(pid1, stats1)
        case DefnPackageObject(mods, id, earlies, inits, self, stats) =>
          val mods1 = mods.dupe
          val id1 = id.dupe
          val earlies1 = earlies.map(_.dupe)
          val inits1 = inits.map(_.dupe)
          val self1 = self.map(_.dupe)
          val stats1 = stats.map(_.dupe)
          DefnPackageObject(mods1, id1, earlies1, inits1, self1, stats1)
        case DefnPat(mods, pats, tpt, rhs) =>
          val mods1 = mods.dupe
          val pats1 = pats.map(_.dupe)
          val tpt1 = tpt.map(_.dupe)
          val rhs1 = rhs.map(_.dupe)
          DefnPat(mods1, pats1, tpt1, rhs1)
        case DefnProcedure(mods, id, tparams, paramss, rhs) =>
          val mods1 = mods.dupe
          val id1 = id.dupe
          val tparams1 = tparams.map(_.dupe)
          val paramss1 = paramss.map(_.map(_.dupe))
          val rhs1 = rhs.map(_.dupe)
          DefnProcedure(mods1, id1, tparams1, paramss1, rhs1)
        case DefnTrait(mods, id, tparams, earlies, inits, self, stats) =>
          val mods1 = mods.dupe
          val id1 = id.dupe
          val tparams1 = tparams.map(_.dupe)
          val earlies1 = earlies.map(_.dupe)
          val inits1 = inits.map(_.dupe)
          val self1 = self.map(_.dupe)
          val stats1 = stats.map(_.dupe)
          DefnTrait(mods1, id1, tparams1, earlies1, inits1, self1, stats1)
        case DefnType(mods, id, tparams, lbound, ubound, rhs) =>
          val mods1 = mods.dupe
          val id1 = id.dupe
          val tparams1 = tparams.map(_.dupe)
          val lbound1 = lbound.map(_.dupe)
          val ubound1 = ubound.map(_.dupe)
          val rhs1 = rhs.map(_.dupe)
          DefnType(mods1, id1, tparams1, lbound1, ubound1, rhs1)
        case Import(importers) =>
          val importers1 = importers.map(_.dupe)
          Import(importers1)
        case ImporteeName(id) =>
          val id1 = id.dupe
          ImporteeName(id1)
        case ImporteeRename(from, to) =>
          val from1 = from.dupe
          val to1 = to.dupe
          ImporteeRename(from1, to1)
        case ImporteeUnimport(id) =>
          val id1 = id.dupe
          ImporteeUnimport(id1)
        case ImporteeWildcard() =>
          ImporteeWildcard()
        case Importer(qual, importees) =>
          val qual1 = qual.dupe
          val importees1 = importees.map(_.dupe)
          Importer(qual1, importees1)
        case Init(tpt, argss) =>
          val tpt1 = tpt.dupe
          val argss1 = argss.map(_.map(_.dupe))
          Init(tpt1, argss1)
        case ModAbstract() =>
          ModAbstract()
        case ModAnnotation(init) =>
          val init1 = init.dupe
          ModAnnotation(init1)
        case ModCase() =>
          ModCase()
        case ModContravariant() =>
          ModContravariant()
        case ModCovariant() =>
          ModCovariant()
        case ModFinal() =>
          ModFinal()
        case ModImplicit() =>
          ModImplicit()
        case ModLazy() =>
          ModLazy()
        case ModOverride() =>
          ModOverride()
        case ModPrivate() =>
          ModPrivate()
        case ModPrivateThis() =>
          ModPrivateThis()
        case ModPrivateWithin(id) =>
          val id1 = id.dupe
          ModPrivateWithin(id1)
        case ModProtected() =>
          ModProtected()
        case ModProtectedThis() =>
          ModProtectedThis()
        case ModProtectedWithin(id) =>
          val id1 = id.dupe
          ModProtectedWithin(id1)
        case ModSealed() =>
          ModSealed()
        case ModVal() =>
          ModVal()
        case ModVar() =>
          ModVar()
        case Mods(trees) =>
          val trees1 = trees.map(_.dupe)
          Mods(trees1)
        case Param(mods, id, tpt, rhs) =>
          val mods1 = mods.dupe
          val id1 = id.dupe
          val tpt1 = tpt.map(_.dupe)
          val rhs1 = rhs.map(_.dupe)
          Param(mods1, id1, tpt1, rhs1)
        case PatAlternative(pats) =>
          val pats1 = pats.map(_.dupe)
          PatAlternative(pats1)
        case PatBind(pats) =>
          val pats1 = pats.map(_.dupe)
          PatBind(pats1)
        case PatExtract(fun, targs, args) =>
          val fun1 = fun.dupe
          val targs1 = targs.map(_.dupe)
          val args1 = args.map(_.dupe)
          PatExtract(fun1, targs1, args)
        case PatExtractInfix(lhs, op, rhs) =>
          val lhs1 = lhs.dupe
          val op1 = op.dupe
          val rhs1 = rhs.dupe
          PatExtractInfix(lhs1, op1, rhs1)
        case PatId(value) =>
          PatId(value)
        case PatInterpolate(id, parts, args) =>
          val id1 = id.dupe
          val parts1 = parts.map(_.dupe)
          val args1 = args.map(_.dupe)
          PatInterpolate(id1, parts1, args1)
        case PatLit(value) =>
          PatLit(value)
        case PatRepeat(pat) =>
          val pat1 = pat.dupe
          PatRepeat(pat1)
        case PatSelect(qual, id) =>
          val qual1 = qual.dupe
          val id1 = id.dupe
          PatSelect(qual1, id1)
        case PatTuple(args) =>
          val args1 = args.map(_.dupe)
          PatTuple(args1)
        case PatVar(id, tpt) =>
          val id1 = id.dupe
          val tpt1 = tpt.map(_.dupe)
          PatVar(id1, tpt1)
        case PrimaryCtor(mods, paramss) =>
          val mods1 = mods.dupe
          val paramss1 = paramss.map(_.map(_.dupe))
          PrimaryCtor(mods1, paramss1)
        case SecondaryCtor(mods, id, paramss, rhs) =>
          val mods1 = mods.dupe
          val id1 = id.dupe
          val paramss1 = paramss.map(_.map(_.dupe))
          val rhs1 = rhs.dupe
          SecondaryCtor(mods1, id1, paramss1, rhs1)
        case Self(id, tpt) =>
          val id1 = id.dupe
          val tpt1 = tpt.map(_.dupe)
          Self(id1, tpt1)
        case SomeId(value) =>
          SomeId(value)
        case Source(stats) =>
          val stats1 = stats.map(_.dupe)
          Source(stats1)
        case TermAnnotate(fun, mods) =>
          val fun1 = fun.dupe
          val mods1 = mods.dupe
          TermAnnotate(fun1, mods1)
        case TermApply(fun, args) =>
          val fun1 = fun.dupe
          val args1 = args.map(_.dupe)
          TermApply(fun1, args1)
        case TermApplyInfix(lhs, op, targs, args) =>
          val lhs1 = lhs.dupe
          val op1 = op.dupe
          val targs1 = targs.map(_.dupe)
          val args1 = args.map(_.dupe)
          TermApplyInfix(lhs1, op1, targs1, args1)
        case TermApplyPostfix(arg, op) =>
          val arg1 = arg.dupe
          val op1 = op.dupe
          TermApplyPostfix(arg1, op1)
        case TermApplyPrefix(op, arg) =>
          val op1 = op.dupe
          val arg1 = arg.dupe
          TermApplyPrefix(op1, arg1)
        case TermApplyType(fun, targs) =>
          val fun1 = fun.dupe
          val targs1 = targs.map(_.dupe)
          TermApplyType(fun1, targs1)
        case TermAscribe(term, tpt) =>
          val term1 = term.dupe
          val tpt1 = tpt.dupe
          TermAscribe(term1, tpt1)
        case TermAssign(lhs, rhs) =>
          val lhs1 = lhs.dupe
          val rhs1 = rhs.dupe
          TermAssign(lhs1, rhs1)
        case TermBlock(stats) =>
          val stats1 = stats.map(_.dupe)
          TermBlock(stats1)
        case TermDo(body, cond) =>
          val body1 = body.dupe
          val cond1 = cond.dupe
          TermDo(body1, cond1)
        case TermEta(term) =>
          val term1 = term.dupe
          TermEta(term1)
        case TermFor(enums, body) =>
          val enums1 = enums.map(_.dupe)
          val body1 = body.dupe
          TermFor(enums1, body1)
        case TermForYield(enums, body) =>
          val enums1 = enums.map(_.dupe)
          val body1 = body.dupe
          TermForYield(enums1, body1)
        case TermFunction(params, body) =>
          val params1 = params.map(_.dupe)
          val body1 = body.dupe
          TermFunction(params1, body1)
        case TermId(value) =>
          TermId(value)
        case TermIf(cond, thenp, elsep) =>
          val cond1 = cond.dupe
          val thenp1 = thenp.dupe
          val elsep1 = elsep.map(_.dupe)
          TermIf(cond1, thenp1, elsep1)
        case TermInterpolate(id, parts, args) =>
          val id1 = id.dupe
          val parts1 = parts.map(_.dupe)
          val args1 = args.map(_.dupe)
          TermInterpolate(id1, parts1, args1)
        case TermLit(value) =>
          TermLit(value)
        case TermMatch(term, cases) =>
          val term1 = term.dupe
          val cases1 = cases.map(_.dupe)
          TermMatch(term1, cases1)
        case TermNew(init) =>
          val init1 = init.dupe
          TermNew(init1)
        case TermNewAnonymous(earlies, inits, self, stats) =>
          val earlies1 = earlies.map(_.dupe)
          val inits1 = inits.map(_.dupe)
          val self1 = self.map(_.dupe)
          val stats1 = stats.map(_.map(_.dupe))
          TermNewAnonymous(earlies1, inits1, self1, stats1)
        case TermPartialFunction(cases) =>
          val cases1 = cases.map(_.dupe)
          TermPartialFunction(cases1)
        case TermRepeat(term) =>
          val term1 = term.dupe
          TermRepeat(term1)
        case TermReturn(term) =>
          val term1 = term.map(_.dupe)
          TermReturn(term1)
        case TermSelect(qual, id) =>
          val qual1 = qual.dupe
          val id1 = id.dupe
          TermSelect(qual1, id1)
        case TermSuper(qual, mix) =>
          val qual1 = qual.dupe
          val mix1 = mix.dupe
          TermSuper(qual1, mix1)
        case TermSynthetic() =>
          TermSynthetic()
        case TermThis(qual) =>
          val qual1 = qual.dupe
          TermThis(qual1)
        case TermThrow(term) =>
          val term1 = term.dupe
          TermThrow(term1)
        case TermTry(term, catchp, finallyp) =>
          val term1 = term.dupe
          val catchp1 = catchp.map(_.dupe)
          val finallyp1 = finallyp.map(_.dupe)
          TermTry(term1, catchp1, finallyp1)
        case TermTryWithHandler(term, catchp, finallyp) =>
          val term1 = term.dupe
          val catchp1 = catchp.dupe
          val finallyp1 = finallyp.map(_.dupe)
          TermTryWithHandler(term1, catchp1, finallyp1)
        case TermTuple(args) =>
          val args1 = args.map(_.dupe)
          TermTuple(args1)
        case TermWhile(cond, body) =>
          val cond1 = cond.dupe
          val body1 = body.dupe
          TermWhile(cond1, body1)
        case TermWildcard() =>
          TermWildcard()
        case TermWildcardFunction(ids, body) =>
          val ids1 = ids.map(_.dupe)
          val body1 = body.dupe
          TermWildcardFunction(ids1, body1)
        case TptAnnotate(tpt, mods) =>
          val tpt1 = tpt.dupe
          val mods1 = mods.dupe
          TptAnnotate(tpt1, mods1)
        case TptByName(tpt) =>
          val tpt1 = tpt.dupe
          TptByName(tpt1)
        case TptExistential(tpt, stats) =>
          val tpt1 = tpt.dupe
          val stats1 = stats.map(_.dupe)
          TptExistential(tpt1, stats1)
        case TptFunction(targs) =>
          val targs1 = targs.map(_.dupe)
          TptFunction(targs1)
        case TptId(value) =>
          TptId(value)
        case TptParameterize(fun, targs) =>
          val fun1 = fun.dupe
          val targs1 = targs.map(_.dupe)
          TptParameterize(fun1, targs1)
        case TptParameterizeInfix(lhs, op, rhs) =>
          val lhs1 = lhs.dupe
          val op1 = op.dupe
          val rhs1 = rhs.dupe
          TptParameterizeInfix(lhs1, op1, rhs1)
        case TptProject(qual, id) =>
          val qual1 = qual.dupe
          val id1 = id.dupe
          TptProject(qual1, id1)
        case TptRefine(tpt, stats) =>
          val tpt1 = tpt.map(_.dupe)
          val stats1 = stats.map(_.dupe)
          TptRefine(tpt1, stats1)
        case TptRepeat(tpt) =>
          val tpt1 = tpt.dupe
          TptRepeat(tpt1)
        case TptSelect(qual, id) =>
          val qual1 = qual.dupe
          val id1 = id.dupe
          TptSelect(qual1, id1)
        case TptSingleton(path) =>
          val path1 = path.dupe
          TptSingleton(path1)
        case TptTuple(targs) =>
          val targs1 = targs.map(_.dupe)
          TptTuple(targs1)
        case TptWildcard(lbound, ubound) =>
          val lbound1 = lbound.map(_.dupe)
          val ubound1 = ubound.map(_.dupe)
          TptWildcard(lbound1, ubound1)
        case TptWildcardExistential(ids, tpt) =>
          val ids1 = ids.map(_.dupe)
          val tpt1 = tpt.dupe
          TptWildcardExistential(ids1, tpt1)
        case TptWith(tpts) =>
          val tpt1 = tpts.map(_.dupe)
          TptWith(tpt1)
        case TypeParam(mods, id, tparams, lbound, ubound, vbounds, cbounds) =>
          val mods1 = mods.dupe
          val id1 = id.dupe
          val tparams1 = tparams.map(_.dupe)
          val lbound1 = lbound.map(_.dupe)
          val ubound1 = ubound.map(_.dupe)
          val vbounds1 = vbounds.map(_.dupe)
          val cbounds1 = cbounds.map(_.dupe)
          TypeParam(mods1, id1, tparams1, lbound1, ubound1, vbounds1, cbounds1)
      }
      result.asInstanceOf[T]
    }
  }
}
