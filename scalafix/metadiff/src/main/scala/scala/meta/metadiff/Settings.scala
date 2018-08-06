package scala.meta.metadiff

import java.io.File
import java.nio.file.{Path, Paths}
import scala.meta.cli.Reporter

final case class Settings(
  paths: List[Path] = List()
)

object Settings {
  def parse(args: List[String], reporter: Reporter): Option[Settings] = {
    def loop(
      settings: Settings,
      allowOptions: Boolean,
      args: List[String]): Option[Settings] = {
      args match {
        case "--" +: rest =>
          loop(settings, false, rest)
        case flag +: rest if allowOptions && flag.startsWith("-") =>
          reporter.out.println(s"unknown flag $flag")
          None
        case pathStr +: rest =>
          val path = pathStr.split(File.pathSeparator).map(Paths.get(_))
          val paths1 = settings.paths ++ path
          loop(settings.copy(paths = paths1), true, rest)
        case Nil =>
          Some(settings)
      }
    }
    loop(Settings(), true, args).filter { settings =>
      if (settings.paths.length != 2) reporter.out.println("Expected exactly two paths to diff")
      settings.paths.length == 2
    }
  }
}
