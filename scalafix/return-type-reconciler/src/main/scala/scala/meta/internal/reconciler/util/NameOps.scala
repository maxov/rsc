package scala.meta.internal.reconciler.util

import scala.meta.tokens.Token

object NameOps {

  /** Returns true if this identifier requires a leading space before colon.
    *
    * Example:
    *   needsLeadingSpaceBeforeColon(foo_) // true
    *   needsLeadingSpaceBeforeColon(foo)  // false
    *   val foo_ : Int = 2 // OK
    *   val foo_: Int = 2  // ERROR
    *   val foo: Int = 2   // OK
    *
    **/
  def needsLeadingSpaceBeforeColon(ident: String): Boolean = ident.lastOption.exists {
    case '`' => false
    case ch => !ch.isLetterOrDigit
  }
}
