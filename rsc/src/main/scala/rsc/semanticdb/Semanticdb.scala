// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semanticdb

import java.io._
import java.nio.file._
import java.util.HashMap
import rsc.gensym._
import rsc.lexis._
import rsc.outline._
import rsc.report._
import rsc.semantics._
import rsc.settings._
import rsc.syntax._
import rsc.util._
import scala.collection.JavaConverters._
import scala.collection.mutable.UnrolledBuffer
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Accessibility.{Tag => a}
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb.SymbolInformation.{Property => p}
import scala.meta.internal.semanticdb.{Language => l}

final class Semanticdb private (
    settings: Settings,
    reporter: Reporter,
    gensyms: Gensyms,
    symtab: Symtab) {
  private val infos = new HashMap[Input, UnrolledBuffer[s.SymbolInformation]]

  def apply(outline: Outline): Unit = {
    val input = outline.pos.input
    if (input == NoInput) crash(outline)
    var buf = infos.get(input)
    if (buf == null) {
      buf = new UnrolledBuffer[s.SymbolInformation]
      infos.put(input, buf)
    }
    val info = s.SymbolInformation(
      symbol = outline.symbol,
      language = outline.language,
      kind = outline.kind,
      properties = outline.properties,
      name = outline.name,
      signature = outline.signature,
      annotations = outline.annotations,
      accessibility = outline.accessibility
    )
    buf += info
  }

  private def builtinPackage(sym: Symbol): s.SymbolInformation = {
    s.SymbolInformation(
      symbol = sym,
      language = l.SCALA,
      kind = k.PACKAGE,
      name = sym.desc.value,
      accessibility = Some(s.Accessibility(a.PUBLIC))
    )
  }

  def save(): Unit = {
    Files.createDirectories(settings.out.toAbsolutePath.getParent)
    val fos = Files.newOutputStream(settings.out)
    val bos = new BufferedOutputStream(fos)
    try {
      val cwd = Paths.get("").toAbsolutePath
      val documents = new UnrolledBuffer[s.TextDocument]
      val it = infos.entrySet.iterator
      while (it.hasNext) {
        val entry = it.next()
        val symbols = entry.getValue
        symbols += builtinPackage(RootPackage)
        symbols += builtinPackage(EmptyPackage)
        val document = s.TextDocument(
          schema = s.Schema.SEMANTICDB4,
          uri = cwd.relativize(entry.getKey.path.toAbsolutePath).toString,
          language = l.SCALA,
          symbols = symbols)
        documents += document
      }
      val payload = s.TextDocuments(documents = documents)
      payload.writeTo(bos)
    } finally {
      bos.close()
      fos.close()
    }
  }

  implicit class OutlineSemanticdbOps(outline: Outline) {
    def symbol: String = {
      outline.id.sym
    }

    def language: s.Language = {
      l.SCALA
    }

    def kind: s.SymbolInformation.Kind = {
      outline match {
        case _: DefnClass => k.CLASS
        case _: DefnField => crash(outline)
        case _: DefnMacro => k.MACRO
        case _: DefnMethod => k.METHOD
        case _: DefnObject => k.OBJECT
        case _: DefnPackage => k.PACKAGE
        case _: DefnPackageObject => k.PACKAGE_OBJECT
        case _: DefnProcedure => k.METHOD
        case _: DefnTrait => k.TRAIT
        case _: DefnType => k.TYPE
        case _: Param => k.PARAMETER
        case _: PatVar => crash(outline)
        case _: PrimaryCtor => k.CONSTRUCTOR
        case _: SecondaryCtor => k.CONSTRUCTOR
        case _: Self => k.SELF_PARAMETER
        case _: TypeParam => k.TYPE_PARAMETER
      }
    }

    def properties: Int = {
      var result = 0
      def set(prop: s.SymbolInformation.Property) = result |= prop.value
      if (outline.hasAbstract) set(p.ABSTRACT)
      outline match {
        case outline: DefnField if outline.rhs.isEmpty => set(p.ABSTRACT)
        case outline: DefnMethod if outline.rhs.isEmpty => set(p.ABSTRACT)
        case outline: DefnProcedure if outline.rhs.isEmpty => set(p.ABSTRACT)
        case outline: DefnType if outline.rhs.isEmpty => set(p.ABSTRACT)
        case _ => ()
      }
      if (outline.hasFinal && !outline.isInstanceOf[Param]) set(p.FINAL)
      if (outline.isInstanceOf[DefnObject]) set(p.FINAL)
      if (outline.isInstanceOf[DefnPackageObject]) set(p.FINAL)
      outline match {
        case outline: DefnClass =>
          outline.inits.foreach {
            case Init(path: TptPath, Nil) if path.id.sym == AnyValClass =>
              set(p.FINAL)
            case _ =>
              ()
          }
        case _ =>
          ()
      }
      if (outline.hasSealed) set(p.SEALED)
      if (outline.hasImplicit) set(p.IMPLICIT)
      if (outline.hasLazy) set(p.LAZY)
      if (outline.hasCase) set(p.CASE)
      if (outline.hasCovariant) set(p.COVARIANT)
      if (outline.hasContravariant) set(p.CONTRAVARIANT)
      if (outline.hasVal) set(p.VAL)
      if (outline.hasVar) set(p.VAR)
      if (outline.isInstanceOf[PrimaryCtor]) set(p.PRIMARY)
      if (outline.isSynthetic) set(p.SYNTHETIC)
      outline match {
        case Param(_, _, _, Some(_)) => set(p.DEFAULT)
        case _ => ()
      }
      result
    }

    def name: String = {
      outline.id match {
        case id: AnonId => "_"
        case id: NamedId => id.value
      }
    }

    def signature: s.Signature = {
      val isCtor = outline.isInstanceOf[DefnCtor]
      outline match {
        case outline: DefnDef =>
          val tparams = Some(s.Scope(outline.tparams.map(_.id.sym)))
          val paramss = {
            val paramss = symtab._paramss.get(outline)
            def isImplicit(xs: List[Param]) = xs match {
              case Nil => false
              case xs => xs.forall(_.hasImplicit)
            }
            if (isCtor && paramss.forall(isImplicit)) {
              s.Scope() +: paramss.map(ps => s.Scope(ps.map(_.id.sym)))
            } else {
              paramss.map(ps => s.Scope(ps.map(_.id.sym)))
            }
          }
          val ret = {
            if (isCtor) s.NoType
            else outline.ret.map(_.tpe).getOrElse(s.NoType)
          }
          s.MethodSignature(tparams, paramss, ret)
        case outline: DefnField =>
          val tpe = outline.tpt.map(_.tpe)
          tpe.map(tpe => s.ValueSignature(tpe)).getOrElse(s.NoSignature)
        case outline: DefnPackage =>
          s.NoSignature
        case outline: DefnTemplate =>
          val tparams = Some(s.Scope(outline.tparams.map(_.id.sym)))
          val parents = outline.parents.map(_.tpe)
          val self = outline.self.flatMap(_.tpt).map(_.tpe).getOrElse(s.NoType)
          val decls = {
            symtab.scopes(outline.id.sym) match {
              case scope: TemplateScope =>
                val maybeMultis = scope._storage.values.asScala.toList
                Some(s.Scope(maybeMultis.flatMap(_.asMulti)))
              case other =>
                crash(other)
            }
          }
          s.ClassSignature(tparams, parents, self, decls)
        case outline: DefnType =>
          val tparams = Some(s.Scope(outline.tparams.map(_.id.sym)))
          val lbound = outline.lo.tpe
          val ubound = outline.hi.tpe
          s.TypeSignature(tparams, lbound, ubound)
        case outline: Param =>
          val tpe = outline.tpt.map(_.tpe)
          tpe.map(tpe => s.ValueSignature(tpe)).getOrElse(s.NoSignature)
        case outline: PatVar =>
          val tpe = outline.tpt.map(_.tpe)
          tpe.map(tpe => s.ValueSignature(tpe)).getOrElse(s.NoSignature)
        case outline: Self =>
          val tpe = outline.tpt.map(_.tpe)
          tpe.map(tpe => s.ValueSignature(tpe)).getOrElse(s.NoSignature)
        case outline: TypeParam =>
          val tparams = Some(s.Scope(outline.tparams.map(_.id.sym)))
          val lbound = outline.lo.tpe
          val ubound = outline.hi.tpe
          s.TypeSignature(tparams, lbound, ubound)
      }
    }

    def annotations: List[s.Annotation] = {
      // FIXME: https://github.com/twitter/rsc/issues/93
      Nil
    }

    def accessibility: Option[s.Accessibility] = {
      outline match {
        case outline: Param =>
          Some(s.Accessibility(a.PUBLIC, ""))
        case outline if outline.hasPrivate =>
          Some(s.Accessibility(a.PRIVATE, ""))
        case outline if outline.hasPrivateThis =>
          Some(s.Accessibility(a.PRIVATE_THIS, ""))
        case outline if outline.hasPrivateWithin =>
          val id = outline.within.get
          Some(s.Accessibility(a.PRIVATE_WITHIN, id.sym))
        case outline if outline.hasProtected =>
          Some(s.Accessibility(a.PROTECTED, ""))
        case outline if outline.hasProtectedThis =>
          Some(s.Accessibility(a.PROTECTED_THIS, ""))
        case outline if outline.hasProtectedWithin =>
          val id = outline.within.get
          Some(s.Accessibility(a.PROTECTED_WITHIN, id.sym))
        case _ =>
          Some(s.Accessibility(a.PUBLIC, ""))
      }
    }
  }

  implicit class TemplateSemanticdbOps(template: DefnTemplate) {
    def parents: List[Tpt] = {
      val rscParents = symtab._parents.get(template)
      val scalacFixup = {
        def parentSym(tpt: Tpt): Symbol = {
          tpt match {
            case path: TptPath => path.id.sym
            case TptAnnotate(tpt, _) => parentSym(tpt)
            case TptApply(tpt, _) => parentSym(tpt)
            case _ => NoSymbol
          }
        }
        def superClass(parentSyms: List[Symbol]): Symbol = {
          parentSyms match {
            case JavaComparableClass :: _ =>
              AnyRefClass
            case JavaSerializableClass :: _ =>
              AnyRefClass
            case AnyClass :: _ =>
              AnyRefClass
            case firstParentSym :: _ =>
              val firstScope = symtab.scopes(firstParentSym)
              firstScope match {
                case firstScope: TemplateScope =>
                  firstScope.tree match {
                    case tree: DefnTrait =>
                      superClass(tree.parents.map(parentSym))
                    case tree: DefnClass =>
                      firstParentSym
                    case tree =>
                      crash(tree)
                  }
                case firstScope: IndexScope =>
                  val firstInfo = symtab._index.apply(firstParentSym)
                  if (firstInfo.kind == k.TRAIT ||
                      firstInfo.kind == k.INTERFACE) {
                    superClass(firstInfo.parents)
                  } else if (firstInfo.kind == k.TYPE) {
                    val aliasShallow = firstInfo.signature match {
                      case s.TypeSignature(_, _, s.TypeRef(_, sym, _)) =>
                        sym
                      case other =>
                        crash(other.asMessage.toProtoString)
                    }
                    val aliasDeep = superClass(List(aliasShallow))
                    if (aliasShallow != aliasDeep) aliasDeep
                    else firstParentSym
                  } else {
                    firstParentSym
                  }
                case firstScope =>
                  crash(firstScope)
              }
            case Nil =>
              crash(template)
          }
        }
        val rscParentSyms = rscParents.map(parentSym)
        val scalacFirstParentSym = superClass(rscParentSyms)
        val rscFirstParentSym = rscParentSyms.headOption.getOrElse(NoSymbol)
        if (scalacFirstParentSym != rscFirstParentSym) {
          val scalacFirstParent = TptId(scalacFirstParentSym.desc.value)
          List(scalacFirstParent.withSym(scalacFirstParentSym))
        } else {
          Nil
        }
      }
      scalacFixup ++ rscParents
    }
  }

  implicit class ModSemanticdbOps(mods: Mods) {
    def annotations: List[s.Annotation] = {
      // FIXME: https://github.com/twitter/rsc/issues/93
      mods.annots.map(annot => s.Annotation(tpe = s.NoType))
    }
  }

  implicit class TptSemanticdbOps(tpt: Tpt) {
    def tpe: s.Type = {
      tpt match {
        case TptAnnotate(tpt, mods) =>
          s.AnnotatedType(mods.annotations, tpt.tpe)
        case TptApply(fun, targs) =>
          fun match {
            case fun: TptPath =>
              def typeRef(targs: List[Tpt]): s.Type = {
                val s.TypeRef(pre, sym, Nil) = fun.tpe
                s.TypeRef(pre, sym, targs.map(_.tpe))
              }
              val wildcards = targs.collect { case tpt: TptWildcard => tpt }
              if (wildcards.isEmpty) {
                typeRef(targs)
              } else {
                val existentials = wildcards.map { wildcard =>
                  val gensym = gensyms(wildcard)
                  val sig = {
                    val tparams = Some(s.Scope())
                    val lo = wildcard.lo.tpe
                    val hi = wildcard.hi.tpe
                    s.TypeSignature(tparams, lo, hi)
                  }
                  s.SymbolInformation(
                    symbol = gensym.local(),
                    language = l.SCALA,
                    kind = k.TYPE,
                    properties = p.ABSTRACT.value,
                    name = "_",
                    signature = sig,
                    annotations = Nil,
                    accessibility = Some(s.Accessibility(a.PUBLIC))
                  )
                }
                val targs1 = targs.map { targ =>
                  val i = wildcards.indexOf(targ)
                  if (i != -1) TptId("_").withSym(existentials(i).symbol)
                  else targ
                }
                val scope = Some(s.Scope(hardlinks = existentials))
                s.ExistentialType(typeRef(targs1), scope)
              }
            case other =>
              // FIXME: https://github.com/scalameta/scalameta/issues/1565
              crash(other)
          }
        case TptByName(tpt) =>
          s.ByNameType(tpt.tpe)
        case TptExistential(tpt, stats) =>
          // FIXME: https://github.com/twitter/rsc/issues/94
          s.NoType
        case tpt: TptId =>
          // FIXME: https://github.com/twitter/rsc/issues/90
          s.TypeRef(s.NoType, tpt.sym, Nil)
        case tpt: TptProject =>
          // FIXME: https://github.com/twitter/rsc/issues/91
          s.NoType
        case TptRefine(tpt, stats) =>
          // FIXME: https://github.com/twitter/rsc/issues/95
          s.NoType
        case TptRepeat(tpt) =>
          s.RepeatedType(tpt.tpe)
        case tpt: TptSelect =>
          // FIXME: https://github.com/twitter/rsc/issues/90
          s.TypeRef(s.NoType, tpt.id.sym, Nil)
        case TptSingleton(id: TermId) =>
          // FIXME: https://github.com/twitter/rsc/issues/90
          s.SingleType(s.NoType, id.sym)
        case TptSingleton(TermSelect(_, id)) =>
          // FIXME: https://github.com/twitter/rsc/issues/90
          s.SingleType(s.NoType, id.sym)
        case TptSingleton(_: TermSuper) =>
          // FIXME: https://github.com/twitter/rsc/issues/96
          s.NoType
        case TptSingleton(TermThis(id)) =>
          s.ThisType(id.sym)
        case _: TptWildcard =>
          crash(tpt)
        case TptWildcardExistential(_, tpt) =>
          tpt.tpe
        case TptWith(tpts) =>
          val tpe = s.WithType(tpts.map(_.tpe))
          val decls = Some(s.Scope())
          s.StructuralType(tpe, decls)
      }
    }
  }
}

object Semanticdb {
  def apply(
      settings: Settings,
      reporter: Reporter,
      gensyms: Gensyms,
      symtab: Symtab): Semanticdb = {
    new Semanticdb(settings, reporter, gensyms, symtab)
  }
}
