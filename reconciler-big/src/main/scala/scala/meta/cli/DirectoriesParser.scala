package scala.meta.cli

import java.nio.file.{Path, Paths}
import scala.io.Source

class DirectoriesParser(path: Path) {

  private val text = Source.fromFile(path.toFile).getLines().toList

  def apply(): Seq[Target] = {
    text.map { s =>
      val Array(name, classesStr, sourcesStr) = s.split(' ')
      val Seq(classes) = classesStr.split(':').map(Paths.get(_)).toSeq
      val sources = sourcesStr.split(':').map(Paths.get(_)).toList
      Target(name, classes, sources)
    }
  }

}

case class Target(name: String, classes: Path, sources: List[Path])
