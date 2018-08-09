package scala.meta.internal.reconciler.patch

import scala.meta._
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala._

class PatchDetector(inputFrom: Input, dbFrom: s.TextDocument, inputTo: Input, dbTo: s.TextDocument) {

  def declaredSymbolsInfo(input: Input, db: s.TextDocument): Map[String, Option[Type]] = {
    val sourceTree = input.parse[Source].get
    val occs = db.occurrences.map { occ => occ.range.get -> occ }.toMap

    def hasSingleName(pats: List[Pat]): Boolean = pats match {
      case List(Pat.Var(name)) => true
      case _ => false
    }

    sourceTree.collect {
      case tree: Defn.Val if hasSingleName(tree.pats) =>
        val List(Pat.Var(name)) = tree.pats
        val namePos = name.pos.range
        val occ = occs(namePos)
        (occ.symbol, tree.decltpe)
      case tree: Defn.Var if hasSingleName(tree.pats) =>
        val List(Pat.Var(name)) = tree.pats
        val namePos = name.pos.range
        val occ = occs(namePos)
        (occ.symbol, tree.decltpe)
      case tree: Defn.Def =>
        val namePos = tree.name.pos.range
        val occ = occs(namePos)
        (occ.symbol, tree.decltpe)
    }.filter {
      case (sym, occ) => sym.isGlobal
    }.toMap
  }

  def compute(): PositionDiff = {
    val fromDeclaredSymbols = declaredSymbolsInfo(inputFrom, dbFrom)
    val toDeclaredSymbols = declaredSymbolsInfo(inputTo, dbTo)

    val differingTypes = (fromDeclaredSymbols.keySet | toDeclaredSymbols.keySet).toSeq
      .map { key => (fromDeclaredSymbols(key), toDeclaredSymbols(key)) }

    // def name =
    // def name: <my type> =
    val addedTypes = differingTypes
      .collect {
        case (None, Some(to)) => (to.pos.start - 2, to.pos.size + 2)
      }.sortBy(_._1)

    def addUpDifferingPositions(added: Seq[(Int, Int)], cumulative: Int = 0): List[(Int, Int)] = added match {
      case Seq() => List()
      case (pos, diff) +: rest => (pos, cumulative + diff) :: addUpDifferingPositions(rest, cumulative + diff)
    }

    val cumulativeAdded = addUpDifferingPositions(addedTypes)
    cumulativeAdded
  }

}

