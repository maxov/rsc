package scala.meta.internal.metadiff

import scala.collection.mutable

object Diff {

  sealed trait DiffElem[T]
  case class Insert[T](t: T) extends DiffElem[T]
  case class Delete[T](t: T) extends DiffElem[T]
  case class Keep[T](t: T) extends DiffElem[T]
  case class Edit[T](tFrom: T, tTo: T) extends DiffElem[T]

  def apply[T](
      seqFrom: List[T],
      seqTo: List[T],
      allowEdit: Boolean = true): List[DiffElem[T]] = {
    println(s"running ${seqFrom.length} ${seqTo.length}")
    // map from (from suffix, to suffix) to (number of common elements, diff)
    val cache = mutable.Map.empty[(List[T], List[T]), (Int, List[DiffElem[T]])]
    def loop(seqFrom: List[T], seqTo: List[T]): (Int, List[DiffElem[T]]) = {
      if (cache.contains((seqFrom, seqTo))) cache((seqFrom, seqTo))
      else {
        val result = (seqFrom, seqTo) match {
          case (Nil, Nil) => (0, Nil)
          case (Nil, seqTo) => (seqTo.length, seqTo.map(Insert.apply))
          case (seqFrom, Nil) => (seqFrom.length, seqFrom.map(Delete.apply))
          case (elemFrom +: restFrom, elemTo +: restTo) if elemFrom == elemTo =>
            val (restEditDist, restDiffSeq) = loop(restFrom, restTo)
            (restEditDist, Keep(elemFrom) +: restDiffSeq)
          case (seqFrom, seqTo) =>
            val diffIfInsert = {
              val (restEditDist, diffSeq) = loop(seqFrom, seqTo.tail)
              (restEditDist + 1, Insert(seqTo.head) +: diffSeq)
            }
            val diffIfDelete = {
              val (restEditDist, diffSeq) = loop(seqFrom.tail, seqTo)
              (restEditDist + 1, Delete(seqFrom.head) +: diffSeq)
            }
            val allPossibilities = if (allowEdit) {
              val diffIfEdit = {
                val (restEditDist, diffSeq) = loop(seqFrom.tail, seqTo.tail)
                (restEditDist + 1, Edit(seqFrom.head, seqTo.head) +: diffSeq)
              }
              List(diffIfInsert, diffIfDelete, diffIfEdit)
            } else List(diffIfInsert, diffIfDelete)
            allPossibilities.minBy(_._1)
        }
        cache((seqFrom, seqTo)) = result
        result
      }
    }
    val (_, diffSeq) = loop(seqFrom, seqTo)
    println("finished running")
    diffSeq
  }

}
