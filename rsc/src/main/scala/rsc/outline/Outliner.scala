// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.outline

import rsc.report._
import rsc.semantics._
import rsc.settings._
import rsc.syntax._
import rsc.util._

// FIXME: https://github.com/twitter/rsc/issues/104
final class Outliner private (
    settings: Settings,
    reporter: Reporter,
    symtab: Symtab,
    todo: Todo) {
  def apply(env: Env, work: Work): Unit = {
    work match {
      case scope: Scope => apply(env, scope)
      case sketch: Sketch => apply(env, sketch)
    }
  }

  // ============ SCOPER ============

  private def apply(env: Env, scope: Scope): Unit = {
    if (!scope.status.isPending) {
      crash(scope)
    }
    scope match {
      case scope: ImporterScope =>
        trySucceed(env, scope)
      case scope: PackageScope =>
        scope.succeed()
      case scope: TemplateScope =>
        trySucceed(env, scope)
      case _ =>
        crash(scope)
    }
  }

  private def trySucceed(env: Env, scope: ImporterScope): Unit = {
    val qualResolution = assignSyms(env, scope.tree.qual)
    qualResolution match {
      case BlockedResolution(dep) =>
        scope.block(dep)
      case _: FailedResolution =>
        scope.fail()
      case FoundResolution(qualSym) =>
        resolveScope(qualSym) match {
          case BlockedResolution(dep) =>
            scope.block(dep)
          case _: FailedResolution =>
            scope.fail()
          case FoundResolution(scopeSym) =>
            val parentScope = symtab.scopes(scopeSym)
            parentScope.status match {
              case _: IncompleteStatus =>
                scope.block(parentScope)
              case _: FailedStatus =>
                scope.fail()
              case SucceededStatus =>
                scope.parent = parentScope
                scope.succeed()
            }
        }
    }
  }

  private def trySucceed(env: Env, scope: TemplateScope): Unit = {
    case class ResolvedParent(tpt: Tpt, scope: Scope)
    val buf = List.newBuilder[ResolvedParent]
    def appendParent(env: Env, tpt: Tpt): Unit = {
      if (scope.status.isPending) {
        def loop(tpt: Tpt): Resolution = {
          tpt match {
            case path: TptPath =>
              assignSyms(env, path)
            case TptAnnotate(tpt, mods) =>
              mods.annots.foreach(ann => todo.add(env, ann.init.tpt))
              loop(tpt)
            case TptApply(tpt, targs) =>
              targs.foreach(targ => todo.add(env, targ))
              loop(tpt)
            case TptWildcardExistential(_, tpt) =>
              loop(tpt)
            case _ =>
              reporter.append(IllegalParent(tpt))
              ErrorResolution
          }
        }
        loop(tpt) match {
          case BlockedResolution(dep) =>
            scope.block(dep)
          case _: FailedResolution =>
            scope.fail()
          case FoundResolution(tptSym) =>
            resolveScope(tptSym) match {
              case BlockedResolution(dep) =>
                scope.block(dep)
              case _: FailedResolution =>
                scope.fail()
              case FoundResolution(scopeSym) =>
                buf += ResolvedParent(tpt, symtab.scopes(scopeSym))
            }
        }
      }
    }
    // FIXME: https://github.com/twitter/rsc/issues/98
    def synthesizeParents(env: Env, tree: DefnTemplate): Unit = {
      scope.tree match {
        case tree if tree.hasCase =>
          appendParent(env, TptId("Product").withSym(ProductClass))
          appendParent(env, TptId("Serializable").withSym(SerializableClass))
        case tree: DefnObject =>
          val companionClass = symtab._outlines.get(tree.id.sym.companionClass)
          companionClass match {
            case caseClass: DefnClass if caseClass.hasCase =>
              if (caseClass.tparams.isEmpty && tree.isSynthetic) {
                val params = caseClass.ctor.paramss.headOption.getOrElse(Nil)
                val sym = AbstractFunctionClass(params.length)
                val core = TptId(sym.desc.value).withSym(sym)
                val paramTpts = params.map(_.tpt.get.dupe)
                val caseClassRef = caseClass.id
                val parent = TptParameterize(core, paramTpts :+ caseClassRef)
                appendParent(env, parent)
              }
              val parent = TptId("Serializable").withSym(SerializableClass)
              appendParent(env, parent)
            case _ =>
              ()
          }
        case _ =>
          ()
      }
      if (buf.result.isEmpty) {
        appendParent(env, TptId("AnyRef").withSym(AnyRefClass))
      }
    }
    scope.tree.inits.foreach { case Init(tpt, _) => appendParent(env, tpt) }
    synthesizeParents(env, scope.tree)
    if (scope.status.isPending) {
      val parents = buf.result
      val incompleteParent = parents.find(_.scope.status.isIncomplete)
      incompleteParent match {
        case Some(incompleteParent) =>
          scope.block(incompleteParent.scope)
        case _ =>
          symtab._parents.put(scope.tree, parents.map(_.tpt))
          buf.clear()
          scope.tree.self.foreach {
            case Self(_, Some(TptWith(ts))) => ts.foreach(appendParent(env, _))
            case Self(_, Some(t)) => appendParent(env, t)
            case _ => ()
          }
          val self = buf.result.filter(_.scope != scope)
          val incompleteSelf = self.find(_.scope.status.isIncomplete)
          incompleteSelf match {
            case Some(incompleteSelf) =>
              scope.block(incompleteSelf.scope)
            case _ =>
              scope.parents = parents.map(_.scope)
              scope.self = self.map(_.scope)
              scope.succeed()
          }
      }
    }
  }

  private def assignSyms(startingEnv: Env, path: Path): Resolution = {
    def assignSym(env: Env, id: Id, resolver: => Resolution): Resolution = {
      val cachedSym = id.sym
      cachedSym match {
        case NoSymbol =>
          val resolution = resolver
          resolution match {
            case BlockedResolution(_) =>
              resolution
            case MissingResolution =>
              if (env == startingEnv) reporter.append(UnboundId(id))
              else reporter.append(UnboundMember(env, id))
              ErrorResolution
            case ErrorResolution =>
              ErrorResolution
            case FoundResolution(sym) =>
              id.sym = sym
              resolution
          }
        case cachedSym =>
          FoundResolution(cachedSym)
      }
    }
    def loop(env: Env, atoms: List[Atom]): Resolution = {
      val atom :: rest = atoms
      val resolution = {
        atom match {
          case IdAtom(id) =>
            assignSym(env, id, env.resolve(id.name))
          case ThisAtom(id) =>
            assignSym(env, id, env.resolveThis(id.nameopt))
          case SuperAtom(id) =>
            assignSym(env, id, env.resolveSuper(id.nameopt))
          case atom: UnsupportedAtom =>
            ErrorResolution
        }
      }
      resolution match {
        case BlockedResolution(_) =>
          resolution
        case _: FailedResolution =>
          resolution
        case FoundResolution(sym) =>
          if (rest.isEmpty) {
            resolution
          } else {
            resolveScope(sym) match {
              case resolution: BlockedResolution =>
                resolution
              case resolution: FailedResolution =>
                resolution
              case FoundResolution(scopeSym) =>
                loop(Env(symtab.scopes(scopeSym)), rest)
            }
          }
      }
    }
    loop(startingEnv, path.atoms)
  }

  // ============ OUTLINER ============

  private def apply(env: Env, sketch: Sketch): Unit = {
    sketch.tree match {
      case tpt: Tpt => apply(env, sketch, tpt)
      case within: SomeId => apply(env, sketch, within)
      case other => crash(other)
    }
    if (sketch.status.isPending) {
      sketch.succeed()
    }
  }

  private def apply(env: Env, sketch: Sketch, tpt: Tpt): Unit = {
    tpt match {
      case TptApply(fun, targs) =>
        apply(env, sketch, fun)
        targs.foreach(apply(env, sketch, _))
      case TptAnnotate(tpt, mods) =>
        apply(env, sketch, tpt)
        mods.annots.foreach(annot => apply(env, sketch, annot.init.tpt))
      case TptByName(tpt) =>
        apply(env, sketch, tpt)
      case TptExistential(tpt, stats) =>
        // FIXME: https://github.com/twitter/rsc/issues/94
        ()
      case tpt: TptPath =>
        apply(env, sketch, tpt: Path)
      case TptRefine(tpt, stats) =>
        // FIXME: https://github.com/twitter/rsc/issues/95
        ()
      case TptRepeat(tpt) =>
        apply(env, sketch, tpt)
      case TptWildcard(ubound, lbound) =>
        ubound.foreach(apply(env, sketch, _))
        lbound.foreach(apply(env, sketch, _))
      case TptWildcardExistential(_, tpt) =>
        // FIXME: https://github.com/twitter/rsc/issues/94
        apply(env, sketch, tpt)
      case TptWith(tpts) =>
        tpts.foreach(apply(env, sketch, _))
    }
  }

  private def apply(startingEnv: Env, sketch: Sketch, path: Path): Unit = {
    def loop(env: Env, path: Path): Resolution = {
      path.id.sym match {
        case NoSymbol =>
          path match {
            case id: NamedId =>
              val resolution = {
                id.name match {
                  case name: SomeName => env.resolveWithin(name)
                  case name => env.resolve(name)
                }
              }
              resolution match {
                case _: BlockedResolution =>
                  resolution
                case _: FailedResolution =>
                  if (env == startingEnv) reporter.append(UnboundId(id))
                  else reporter.append(UnboundMember(env, id))
                  resolution
                case FoundResolution(sym) =>
                  id.sym = sym
                  resolution
              }
            case TermSelect(qual: Path, id) =>
              val resolution = loop(env, qual)
              resolution match {
                case _: BlockedResolution =>
                  resolution
                case _: FailedResolution =>
                  resolution
                case FoundResolution(qualSym) =>
                  resolveScope(qualSym) match {
                    case resolution: BlockedResolution =>
                      resolution
                    case resolution: FailedResolution =>
                      resolution
                    case FoundResolution(scopeSym) =>
                      loop(Env(symtab.scopes(scopeSym)), id)
                  }
              }
            case TermSelect(qual, id) =>
              reporter.append(IllegalOutline(qual))
              ErrorResolution
            case TermSuper(qual, mix) =>
              ???
            case TermThis(qual) =>
              val resolution = env.resolveThis(qual.nameopt)
              resolution match {
                case _: BlockedResolution =>
                  resolution
                case _: FailedResolution =>
                  reporter.append(UnboundId(qual))
                  resolution
                case FoundResolution(qualSym) =>
                  qual.sym = qualSym
                  resolution
              }
            case TptProject(qual, id) =>
              // FIXME: https://github.com/twitter/rsc/issues/91
              reporter.append(IllegalOutline(qual))
              ErrorResolution
            case TptSelect(qual, id) =>
              val resolution = loop(env, qual)
              resolution match {
                case _: BlockedResolution =>
                  resolution
                case _: FailedResolution =>
                  resolution
                case FoundResolution(qualSym) =>
                  resolveScope(qualSym) match {
                    case resolution: BlockedResolution =>
                      resolution
                    case resolution: FailedResolution =>
                      resolution
                    case FoundResolution(scopeSym) =>
                      loop(Env(symtab.scopes(scopeSym)), id)
                  }
              }
            case TptSingleton(qual) =>
              loop(env, qual)
          }
        case sym =>
          FoundResolution(sym)
      }
    }
    loop(startingEnv, path) match {
      case BlockedResolution(dep) =>
        if (sketch.status.isPending) sketch.block(dep)
        else ()
      case _: FailedResolution =>
        if (sketch.status.isPending) sketch.fail()
        else ()
      case _: FoundResolution =>
        ()
    }
  }

  // ============ NEXTGEN ============

  def resolveScope(sym: Symbol): Resolution = {
    var scope = symtab.scopes.get(sym)
    if (scope != null) {
      FoundResolution(sym)
    } else {
      def loop(tpt: Tpt): Resolution = {
        tpt match {
          case TptApply(fun, _) =>
            loop(fun)
          case tpt: TptPath =>
            tpt.id.sym match {
              case NoSymbol =>
                // FIXME: https://github.com/twitter/rsc/issues/104
                BlockedResolution(null)
              case sym =>
                FoundResolution(sym)
            }
          case _ =>
            crash(tpt)
        }
      }
      val outline = symtab._outlines.get(sym)
      outline match {
        case DefnMethod(mods, _, _, _, Some(tpt), _) if mods.hasVal => loop(tpt)
        case DefnType(_, _, _, _, Some(hi), _) => loop(hi)
        case DefnType(_, _, _, _, _, Some(alias)) => loop(alias)
        case TypeParam(_, _, _, _, Some(hi), _, _) => loop(hi)
        case null => crash(sym)
        case _ => crash(outline)
      }
    }
  }
}

object Outliner {
  def apply(
      settings: Settings,
      reporter: Reporter,
      symtab: Symtab,
      todo: Todo): Outliner = {
    new Outliner(settings, reporter, symtab, todo)
  }
}
