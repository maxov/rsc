// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scalafix.internal.rule.semantics

import scala.meta.internal.semanticdb.Scala._

// FIXME: https://github.com/twitter/rsc/issues/141

sealed trait Scope {
  def lookupThis(name: String): String
}

case class TemplateScope(sym: String) extends Scope {
  def lookupThis(name: String): String = {
    if (sym.desc.name == name) sym
    else Symbols.None
  }
}
