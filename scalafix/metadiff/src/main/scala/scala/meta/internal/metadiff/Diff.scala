package scala.meta.internal.metadiff

import scala.collection.mutable

object Diff {

  sealed trait DiffElem[T]
  case class Insert[T](t: T) extends DiffElem[T]
  case class Delete[T](t: T) extends DiffElem[T]
  case class Keep[T](t: T) extends DiffElem[T]

  def apply[T](seqFrom: List[T], seqTo: List[T]): List[DiffElem[T]] = {
    val cache = mutable.Map.empty[(List[T], List[T]), (Int, List[DiffElem[T]])]
    def loop(seqFrom: List[T], seqTo: List[T]): (Int, List[DiffElem[T]]) = {
      if (cache.contains((seqFrom, seqTo))) cache((seqFrom, seqTo))
      (seqFrom, seqTo) match {
        case (Nil, Nil) => (0, Nil)
        case (Nil, seqTo) => (0, seqTo.map(Insert.apply))
        case (seqFrom, Nil) => (0, seqFrom.map(Delete.apply))
        case (elemFrom +: restFrom, elemTo +: restTo) if elemFrom == elemTo =>
          val (restSameLen, restDiffSeq) = loop(restFrom, restTo)
          (restSameLen + 1, Keep(elemFrom) +: restDiffSeq)
        case (seqFrom, seqTo) =>
          val diffIfInsert = loop(seqFrom, seqTo.tail)
          val diffIfDelete = loop(seqFrom.tail, seqTo)
          if (diffIfInsert._1 > diffIfDelete._1)
            (diffIfInsert._1, Insert(seqTo.head) +: diffIfInsert._2)
          else
            (diffIfDelete._1, Delete(seqFrom.head) +: diffIfDelete._2)
      }
    }
    val (_, diffSeq) = loop(seqFrom, seqTo)
    diffSeq
  }

}
