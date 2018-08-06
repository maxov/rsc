package scala.meta.internal.metadiff

import difflib.DiffUtils
import java.nio.file.Path
import scala.meta.cli._
import scala.meta.internal.semanticdb.Locator
import scala.meta.metadiff.Settings
import scala.meta.internal.{semanticdb => s}
import scala.collection.JavaConverters._
import scalapb.GeneratedMessage

class Main(settings: Settings, reporter: Reporter) {

  private val EOL = System.lineSeparator()

  private def collectPayloads(root: Path): Map[String, s.TextDocument] = {
    val builder = Map.newBuilder[String, s.TextDocument]
    Locator(root) { (path, payload) =>
      payload.documents foreach { doc =>
        builder += doc.uri -> doc
      }
    }
    builder.result()
  }

  private def mkDiff(
      linesFrom: List[String],
      linesTo: List[String]): Option[String] = {
    val origLines = linesFrom.asJava
    val patch = DiffUtils.diff(origLines, linesTo.asJava)
    if (patch.getDeltas.isEmpty) None
    else {
      val diffStr = DiffUtils
        .generateUnifiedDiff("", "", origLines, patch, 5)
        .asScala
        .drop(2)
        .mkString(EOL)
      Some(diffStr + EOL)
    }
  }

  private def diffSemantic(
      name: String,
      docFrom: s.TextDocument,
      docTo: s.TextDocument
  ): Option[String] = {
    def diffProtoSeq(f: s.TextDocument => Seq[GeneratedMessage]): List[String] =
      Diff(f(docFrom).toList, f(docTo).toList).collect {
        case Diff.Insert(msg) =>
          mkDiff(List(), msg.toProtoString.lines.toList)
        case Diff.Delete(msg) =>
          mkDiff(msg.toProtoString.lines.toList, List())
        case Diff.Edit(msgFrom, msgTo) =>
          mkDiff(
            msgFrom.toProtoString.lines.toList,
            msgTo.toProtoString.lines.toList)
      }.flatten
    val diffs = diffProtoSeq(_.symbols) /*++ diffProtoSeq(_.occurrences)*/ ++ diffProtoSeq(
      _.synthetics)
    if (diffs.nonEmpty) Some(diffs.mkString)
    else None
  }

  def process(): Boolean = {
    val List(rootFrom, rootTo) = settings.paths
    val payloadsFrom = collectPayloads(rootFrom)
    val payloadsTo = collectPayloads(rootTo)
    val pathsFrom = payloadsFrom.keys.toList.sorted
    val pathsTo = payloadsTo.keys.toList.sorted
    Diff(pathsFrom, pathsTo, allowEdit = false) foreach {
      case Diff.Keep(p) =>
        val payloadFrom = payloadsFrom(p)
        val payloadTo = payloadsTo(p)
        diffSemantic(p, payloadFrom, payloadTo).foreach { d =>
          reporter.out.println(s"--- $p")
          reporter.out.println(s"+++ $p")
          reporter.out.print(d)
        }
      case Diff.Insert(p) =>
        reporter.out.println("---")
        reporter.out.println(s"+++ $p")
      case Diff.Delete(p) =>
        reporter.out.println(s"--- $p")
        reporter.out.println(s"+++")
      case Diff.Edit(_, _) => sys.error("unexpected edit diff")
    }
    true
  }

  implicit val pathOrdering: Ordering[Path] = Ordering.by(_.toString)

}
