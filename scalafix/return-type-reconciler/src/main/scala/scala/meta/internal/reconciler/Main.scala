package scala.meta.internal.reconciler

import java.io.FileOutputStream
import java.nio.file.{Files, Path}
import scala.meta.cli.Reporter
import scala.meta.inputs._
import scala.meta.internal.reconciler.patch.{PatchApplier, PatchDetector}
import scala.meta.internal.semanticdb.Locator
import scala.meta.reconciler.Settings
import scala.meta.internal.{semanticdb => s}

class Main(settings: Settings, reporter: Reporter) {

  private def collectPayloads(root: Path): Map[String, (Path, s.TextDocument)] = {
    val builder = Map.newBuilder[String, (Path, s.TextDocument)]
    Locator(root) { (path, payload) =>
      payload.documents foreach { doc =>
        builder += doc.uri -> (path -> doc)
      }
    }
    builder.result()
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
