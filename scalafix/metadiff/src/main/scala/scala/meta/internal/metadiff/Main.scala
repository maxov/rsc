package scala.meta.internal.metadiff

import difflib.DiffUtils
import java.nio.file.Path
import java.util.regex.Pattern
import scala.meta.cli._
import scala.meta.internal.semanticdb.Locator
import scala.meta.metadiff.Settings
import scala.meta.internal.{semanticdb => s}
import scala.collection.JavaConverters._

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

  private def diffStrSeq(
      linesFrom: List[String],
      linesTo: List[String]): Option[String] = {
    val origLines = linesFrom.asJava
    val patch = DiffUtils.diff(origLines, linesTo.asJava)
    if (patch.getDeltas.isEmpty) None
    else {
      val diffStr = DiffUtils
        .generateUnifiedDiff("", "", origLines, patch, 1000)
        .asScala
        .drop(3)
        .mkString(EOL)
      Some(diffStr + EOL)
    }
  }

  private def diffName(name: String, diff: Option[String]): Option[String] =
    diff.map { d =>
      s"$name:$EOL$d"
    }

  private def diffObj[T](objFrom: T, objTo: T): Option[String] = {
    if (objFrom == objTo) None
    else
      Some(
        s"-$objFrom" + EOL +
          s"+$objFrom" + EOL
      )
  }

  private def diffEntire(diff: Option[String], add: Boolean): Option[String] =
    diff.map { d =>
      val startChar = if (add) '+' else '-'
      val sb = new StringBuilder
      d.split(Pattern.quote(EOL)).foreach { line =>
        sb += startChar
        sb ++= line
        sb ++= EOL
      }
      sb.toString()
    }

  private def diffCombine(diffs: Option[String]*): Option[String] =
    diffs.flatten match {
      case Seq() => None
      case s => Some(s.mkString)
    }

  private def diffSymInfo(
      infoFrom: s.SymbolInformation,
      infoTo: s.SymbolInformation): Option[String] =
    diffStrSeq(
      infoFrom.toProtoString.lines.toList,
      infoTo.toProtoString.lines.toList)

  private def diffDocument(
      docFrom: s.TextDocument,
      docTo: s.TextDocument): Option[String] = {
    val diffSymbols = {
      val symsFrom = docFrom.symbols.map(s => s.symbol -> s).toMap
      val symsTo = docTo.symbols.map(s => s.symbol -> s).toMap
      val orderDiff = diffStrSeq(
        docFrom.symbols.map(_.symbol).toList,
        docTo.symbols.map(_.symbol).toList)
      val restDiff = orderDiff.flatMap { ord =>
        val infoDiffs = ord.lines.map { line =>
          val firstChar = line.charAt(0)
          val sym = line.substring(1)
          val symDiff =
            if (symsFrom.contains(sym) && symsTo.contains(sym))
              diffSymInfo(symsFrom(sym), symsTo(sym))
            else if (symsFrom.contains(sym))
              diffEntire(Some(symsFrom(sym).toProtoString), add = false)
            else diffEntire(Some(symsTo(sym).toProtoString), add = true)
          diffName(s"symbol '$sym'", symDiff)
        }.toSeq
        diffCombine(infoDiffs: _*)
      }
      diffCombine(
        diffName("symbol order", orderDiff),
        restDiff
      )
    }
    diffCombine(
      diffName("schema", diffObj(docFrom.schema, docTo.schema)),
      diffName("language", diffObj(docFrom.language, docTo.language)),
      diffSymbols
    )
  }

  def process(): Boolean = {
    val List(rootFrom, rootTo) = settings.paths
    val payloadsFrom = collectPayloads(rootFrom)
    val payloadsTo = collectPayloads(rootTo)
    val pathsFrom = payloadsFrom.keySet
    val pathsTo = payloadsTo.keySet
    val pathsShared = pathsFrom & pathsTo
    (pathsFrom | pathsTo).toSeq.sorted foreach { path =>
      if (pathsShared(path)) {
        val payloadFrom = payloadsFrom(path)
        val payloadTo = payloadsTo(path)
        diffDocument(payloadFrom, payloadTo) match {
          case Some(s) =>
            reporter.out.println(s"--- $path")
            reporter.out.println(s"+++ $path")
            reporter.out.print(s)
          case None =>
        }
      } else if (pathsFrom(path)) {
        reporter.out.println(s"--- $path")
        reporter.out.println(s"+++")
      } else if (pathsTo(path)) {
        reporter.out.println("---")
        reporter.out.println(s"+++ $path")
      }
    }
    true
  }

  implicit val pathOrdering: Ordering[Path] = Ordering.by(_.toString)

}
