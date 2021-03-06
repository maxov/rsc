// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.outline

import java.util.{LinkedHashMap, Map}
import rsc.classpath._
import rsc.semantics._
import rsc.syntax._
import rsc.util._
import scala.meta.internal.semanticdb.{Language => l}
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k}

sealed abstract class Scope(val sym: Symbol) extends Work {
  def enter(name: Name, sym: Symbol): Symbol

  def resolve(name: Name): Resolution = {
    status match {
      case PendingStatus =>
        BlockedResolution(this)
      case BlockedStatus(_) =>
        BlockedResolution(this)
      case _: FailedStatus =>
        ErrorResolution
      case SucceededStatus =>
        crash(this)
    }
  }
}

final class ImporterScope private (val tree: Importer) extends Scope(NoSymbol) {
  var _parent: Scope = null

  def parent: Scope = {
    if (status.isSucceeded) {
      _parent
    } else {
      crash(this)
    }
  }

  def parent_=(parent: Scope): Unit = {
    if (status.isPending) {
      _parent = parent
    } else {
      crash(this)
    }
  }

  override def enter(name: Name, sym: Symbol): Symbol = {
    crash(this)
  }

  val _mappings: Map[String, String] = new LinkedHashMap[String, String]
  var _wildcard: Boolean = false
  tree.importees.foreach {
    case ImporteeName(SomeId(value)) =>
      _mappings.put(value, value)
    case ImporteeRename(SomeId(from), SomeId(to)) =>
      _mappings.put(to, from)
    case ImporteeUnimport(SomeId(value)) =>
      _mappings.put(value, null)
    case ImporteeWildcard() =>
      _wildcard = true
  }

  private def remap(name: Name): Name = {
    val value1 = {
      val mapValue = _mappings.get(name.value)
      if (_wildcard && (mapValue == null)) {
        name.value
      } else {
        mapValue
      }
    }
    if (value1 != null) {
      name match {
        case _: SomeName =>
          SomeName(value1)
        case _: TermName =>
          TermName(value1)
        case _: TypeName =>
          TypeName(value1)
      }
    } else {
      null
    }
  }

  override def resolve(name: Name): Resolution = {
    val name1 = remap(name)
    if (name1 != null) {
      status match {
        case PendingStatus =>
          super.resolve(name)
        case BlockedStatus(dep) =>
          super.resolve(name)
        case _: FailedStatus =>
          MissingResolution
        case SucceededStatus =>
          parent.resolve(name1)
      }
    } else {
      MissingResolution
    }
  }

  override def succeed(): Unit = {
    if (_parent == null) {
      crash(this)
    }
    super.succeed()
  }
}

object ImporterScope {
  def apply(tree: Importer): ImporterScope = {
    new ImporterScope(tree)
  }
}

sealed trait IndexScope extends Scope {
  def _index: Index

  val _loaded: Map[Name, Symbol] = new LinkedHashMap[Name, Symbol]
  def _load(name: Name): Symbol = {
    val loadedSym = _loaded.get(name)
    if (loadedSym != null) {
      loadedSym
    } else {
      val loadedSym = loadMember(sym, name)
      _loaded.put(name, loadedSym)
      loadedSym
    }
  }

  private def loadMember(owner: Symbol, name: Name): Symbol = {
    val declSym = loadDecl(owner, name)
    if (declSym != NoSymbol) {
      return declSym
    }

    val info = _index(owner)

    (info.parents ++ info.self).foreach { parent =>
      val memberSym = loadDecl(parent, name)
      if (memberSym != NoSymbol) {
        return memberSym
      }
    }

    if (info.kind == k.PACKAGE) {
      val packageObjectSym = TermSymbol(owner, "package")
      if (_index.contains(packageObjectSym)) {
        val packageObjectMemberSym = loadMember(packageObjectSym, name)
        if (_index.contains(packageObjectMemberSym)) {
          return packageObjectMemberSym
        }
      }
    }

    NoSymbol
  }

  private def loadDecl(owner: Symbol, name: Name): Symbol = {
    val declSym = {
      name match {
        case TermName(value) =>
          TermSymbol(owner, value)
        case TypeName(value) =>
          TypeSymbol(owner, value)
        case SomeName(value) =>
          crash(name)
      }
    }
    if (_index.contains(declSym)) {
      return declSym
    }

    name match {
      case TermName(value) =>
        val packageSym = PackageSymbol(owner, value)
        if (_index.contains(packageSym)) {
          return packageSym
        }

        val javaDeclSym = TypeSymbol(owner, value)
        if (_index.contains(javaDeclSym) &&
            _index(javaDeclSym).language == l.JAVA) {
          return javaDeclSym
        }
      case _ =>
        ()
    }

    NoSymbol
  }
}

final class ClasspathScope private (sym: Symbol, val _index: Index)
    extends Scope(sym)
    with IndexScope {
  override def enter(name: Name, sym: Symbol): Symbol = {
    crash(this)
  }

  override def resolve(name: Name): Resolution = {
    _load(name) match {
      case NoSymbol =>
        MissingResolution
      case sym =>
        FoundResolution(sym)
    }
  }
}

object ClasspathScope {
  def apply(sym: Symbol, index: Index): IndexScope = {
    new ClasspathScope(sym, index)
  }
}

sealed abstract class StorageScope(sym: Symbol) extends Scope(sym) {
  val _storage: Map[Name, Symbol] = new LinkedHashMap[Name, Symbol]

  override def enter(name: Name, sym: Symbol): Symbol = {
    if (status.isPending) {
      val existing = _storage.get(name)
      name match {
        case SomeName(_) =>
          crash(name)
        case _ =>
          sym match {
            case NoSymbol =>
              crash(name)
            case _ =>
              if (existing != null) {
                val actual = MultiSymbol(existing, sym)
                _storage.put(name, actual)
                existing
              } else {
                _storage.put(name, sym)
                NoSymbol
              }
          }
      }
    } else {
      crash(this)
    }
  }

  override def resolve(name: Name): Resolution = {
    if (status.isSucceeded) {
      val result = _storage.get(name)
      if (result != null) {
        FoundResolution(result)
      } else {
        name match {
          case SomeName(value) =>
            crash(name)
          case _ =>
            MissingResolution
        }
      }
    } else {
      super.resolve(name)
    }
  }
}

final class PackageScope private (sym: Symbol, val _index: Index)
    extends StorageScope(sym)
    with IndexScope {
  override def resolve(name: Name): Resolution = {
    super.resolve(name) match {
      case MissingResolution =>
        if (_index.contains(sym)) {
          val loadedSym = _load(name)
          loadedSym match {
            case NoSymbol =>
              MissingResolution
            case loadedSym =>
              FoundResolution(loadedSym)
          }
        } else {
          MissingResolution
        }
      case resolution =>
        resolution
    }
  }
}

object PackageScope {
  def apply(sym: Symbol, index: Index): PackageScope = {
    new PackageScope(sym, index)
  }
}

final class PackageObjectScope private (
    sym: Symbol,
    tree: DefnPackageObject,
    packageScope: PackageScope)
    extends TemplateScope(sym, tree) {
  override def enter(name: Name, sym: Symbol): Symbol = {
    val existingSym = super.enter(name, sym)
    packageScope.enter(name, sym)
    existingSym
  }
}

class TemplateScope protected (sym: Symbol, val tree: DefnTemplate)
    extends StorageScope(sym) {
  var _parents: List[Scope] = null
  var _self: List[Scope] = null
  var _env: Env = null

  def parents: List[Scope] = {
    if (status.isSucceeded) {
      _parents
    } else {
      crash(this)
    }
  }

  def parents_=(parents: List[Scope]): Unit = {
    if (status.isPending) {
      _parents = parents
      recomputeEnv()
    } else {
      crash(this)
    }
  }

  def self: List[Scope] = {
    if (status.isSucceeded) {
      _self
    } else {
      crash(this)
    }
  }

  def self_=(self: List[Scope]): Unit = {
    if (status.isPending) {
      _self = self
      recomputeEnv()
    } else {
      crash(this)
    }
  }

  private def recomputeEnv(): Unit = {
    if (_parents != null && _self != null) {
      _env = Env(_self ++ _parents)
    } else if (_parents != null && _self == null) {
      _env = Env(_parents)
    } else if (_parents == null && _self != null) {
      _env = Env(_self)
    } else {
      _env = Env()
    }
  }

  override def resolve(name: Name): Resolution = {
    super.resolve(name) match {
      case MissingResolution =>
        _env.resolve(name)
      case resolution =>
        resolution
    }
  }

  override def succeed(): Unit = {
    if (_parents == null) {
      crash(this)
    }
    super.succeed()
  }
}

object TemplateScope {
  def apply(tree: DefnTemplate): TemplateScope = {
    new TemplateScope(tree.id.sym, tree)
  }
}

final class ParamScope private (owner: Symbol) extends StorageScope(owner)

object ParamScope {
  def apply(owner: Symbol): ParamScope = {
    new ParamScope(owner)
  }
}

final class SelfScope private (owner: Symbol) extends StorageScope(owner)

object SelfScope {
  def apply(owner: Symbol): SelfScope = {
    new SelfScope(owner)
  }
}

final class TypeParamScope private (owner: Symbol) extends StorageScope(owner)

object TypeParamScope {
  def apply(owner: Symbol): TypeParamScope = {
    new TypeParamScope(owner)
  }
}
