package scala.meta.internal.reconciler

import java.io.FileOutputStream
import java.nio.file.{Files, Path}
import scala.meta.cli.Reporter
import scala.meta.inputs._
import scala.meta.internal.reconciler.patch.{PatchApplier, PatchDetector}
import scala.meta.internal.semanticdb.{Locator, TextDocuments}
import scala.meta.reconciler.Settings
import scala.meta.internal.{semanticdb => s}

class Main(settings: Settings, reporter: Reporter) {

  private def merge(doc1: s.TextDocument, doc2: s.TextDocument): s.TextDocument = {
    doc1.copy(
      symbols = doc1.symbols ++ doc2.symbols,
      occurrences = doc1.occurrences ++ doc2.occurrences,
      diagnostics = doc1.diagnostics ++ doc2.diagnostics,
      synthetics = doc1.synthetics ++ doc2.synthetics
    )
  }

  private def collectPayloads(root: Path): Map[String, (Path, s.TextDocument)] = {
    val builder = List.newBuilder[(String, Path, s.TextDocument)]
    Locator(root) { (path, payload) =>
      payload.documents.foreach { doc =>
        builder += ((doc.uri, path, doc))
      }
    }
    builder.result().groupBy(_._1).map {
      case (s, opts) => s -> (opts.head._2, opts.map(_._3).reduce(merge))
    }
  }

  def process(): Boolean = {
    val classesFromPayloads = collectPayloads(settings.classesFrom.get)
    val classesToPayloads = collectPayloads(settings.classesTo.get)
    val uris = classesFromPayloads.keySet
    uris.foreach { uri =>
      val inputFrom = Input.File(settings.sourceFrom.get.resolve(uri))
      val inputTo = Input.File(settings.sourceTo.get.resolve(uri))
      val classesToPayload = classesToPayloads(uri)
      val patch = new PatchDetector(inputFrom, classesFromPayloads(uri)._2, inputTo, classesToPayload._2).compute()
      val newDoc = new PatchApplier(inputFrom, inputTo, classesToPayload._2, patch).apply()
      val outputPath = settings.classesDest.get.resolve(settings.classesTo.get.relativize(classesToPayload._1))
      Files.createDirectories(outputPath.getParent)
      s.TextDocuments(Seq(newDoc)).writeTo(new FileOutputStream(outputPath.toFile))
      println(s"Wrote $outputPath")
    }
    return true
  }

}
