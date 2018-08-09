package scala.meta.internal.reconciler.patch

import scala.meta._
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Synthetic

class PatchApplier(inputFrom: Input, inputTo: Input, dbTo: s.TextDocument, patch: PositionDiff) {

  def replace(synth: s.Synthetic, f: s.Range => s.Range): s.Synthetic = {

    def replaceTree(tree: s.Tree): s.Tree = tree match {
      case tree: s.ApplyTree => tree.copy(
        fn = replaceTree(tree.fn),
        args = tree.args.map(replaceTree)
      )
      case tree: s.TypeApplyTree => tree.copy(
        fn = replaceTree(tree.fn)
      )
      case tree: s.FunctionTree => tree.copy(
        term = replaceTree(tree.term)
      )
      case tree: s.LiteralTree => tree
      case tree: s.SelectTree =>
        tree.copy(
          qual = replaceTree(tree.qual)
        )
      case tree: s.IdTree => tree
      case tree: s.MacroExpansionTree =>
        tree.copy(
          expandee = replaceTree(tree.expandee)
        )
      case tree: s.OriginalTree =>
        tree.copy(range = tree.range.map(f))
    }

    synth.copy(
      range = synth.range.map(f),
      tree = replaceTree(synth.tree)
    )
  }


  def apply(): s.TextDocument = {
    def rangeReplace(range: s.Range): s.Range = {
      val pos = range.position(inputTo)
      val lastAvailablePos = patch.takeWhile {
        case (p, _) => p < pos.start
      }.lastOption
//      println(lastAvailablePos)
      lastAvailablePos match {
        case None => range
        case Some((_, offset)) => Position.Range(inputFrom, pos.start - offset, pos.end - offset).range
      }
    }
    dbTo.copy(
      synthetics = dbTo.synthetics.map(synth => replace(synth, rangeReplace))
    )
  }

}
