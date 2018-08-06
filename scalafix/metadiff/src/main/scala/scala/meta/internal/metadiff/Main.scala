package scala.meta.internal.metadiff

import difflib.DiffUtils
import java.nio.file.Path
import scala.meta.cli._
import scala.meta.internal.semanticdb.Locator
import scala.meta.metadiff.Settings
import scala.meta.internal.{semanticdb => s}
import scala.collection.JavaConverters._

class Main(settings: Settings, reporter: Reporter) {

  private val EOL = System.lineSeparator()

  private def collectPayloads(root: Path): Map[Path, s.TextDocuments] = {
    val builder = Map.newBuilder[Path, s.TextDocuments]
    Locator(root) { (path, payload) =>
      builder += root.relativize(path) -> payload
    }
    builder.result()
  }

  private def mkDiff(
      nameFrom: String,
      nameTo: String,
      linesFrom: List[String],
      linesTo: List[String]): String = {
    val origLines = linesFrom.asJava
    val patch = DiffUtils.diff(origLines, linesTo.asJava)
    DiffUtils
      .generateUnifiedDiff(nameFrom, nameTo, origLines, patch, 5)
      .asScala
      .mkString(EOL)
  }

  def process(): Boolean = {
    val List(rootFrom, rootTo) = settings.paths
    val payloadsFrom = collectPayloads(rootFrom)
    val payloadsTo = collectPayloads(rootTo)
    val pathsFrom = payloadsFrom.keys.toList.sorted
    val pathsTo = payloadsTo.keys.toList.sorted
    Diff(pathsFrom, pathsTo) foreach {
      case Diff.Keep(p) =>
        val payloadFrom = payloadsFrom(p).toProtoString
        val payloadTo = payloadsTo(p).toProtoString
        if (payloadFrom != payloadTo) {
          val outDiff = mkDiff(
            p.toString,
            p.toString,
            payloadFrom.lines.toList,
            payloadTo.lines.toList)
          reporter.out.println(outDiff)
        }
      case Diff.Insert(p) =>
        reporter.out.println("---")
        reporter.out.println(s"+++ $p")
      case Diff.Delete(p) =>
        reporter.out.println(s"--- $p")
        reporter.out.println(s"+++")
    }
    true
  }

  implicit val pathOrdering: Ordering[Path] = Ordering.by(_.toString)

}
