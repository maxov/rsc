package scala.meta.internal.metadiff

import scala.meta.cli._
import scala.meta.internal.semanticdb.Locator
import scala.meta.metadiff.Settings

class Main(settings: Settings, reporter: Reporter) {

  def process(): Boolean = {
    if (settings.paths.length != 2) {
      reporter.err.println("Expected exactly two paths to diff")
      return false
    }
    val List(path1, path2) = settings.paths
    Locator(path1) { (path, payload) =>
      println(path, payload)
    }
    true
  }

}
