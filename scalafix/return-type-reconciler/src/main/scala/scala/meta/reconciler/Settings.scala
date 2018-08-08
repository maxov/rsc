package scala.meta.reconciler

import java.nio.file.{Path, Paths}
import scala.meta.cli.Reporter

final case class Settings(
  sourceFrom: Option[Path] = None,
  sourceTo: Option[Path] = None,
  classesFrom: Option[Path] = None,
  classesTo: Option[Path] = None,
  classesDest: Option[Path] = None
)

object Settings {

  def parse(args: List[String], reporter: Reporter): Option[Settings] = {

    implicit class OptionSettingsOps(settings: Option[Settings]) {
      def hasPath(name: String)(f: Settings => Option[Path]): Option[Settings] = {
        settings.filter { s =>
          val shouldKeep = f(s).isDefined
          if (!shouldKeep) reporter.out.println(s"Missing $name")
          shouldKeep
        }
      }
    }

    def loop(
      settings: Settings,
      allowOptions: Boolean,
      args: List[String]): Option[Settings] = {
      args match {
        case "--" +: rest =>
          loop(settings, false, rest)
        case "--from-source" +: sourceFrom +: rest if allowOptions =>
          loop(settings.copy(sourceFrom = Some(Paths.get(sourceFrom))), true, rest)
        case "--to-source" +: sourceTo +: rest if allowOptions =>
          loop(settings.copy(sourceTo = Some(Paths.get(sourceTo))), true, rest)
        case "-o" +: classesDest +: rest if allowOptions =>
          loop(settings.copy(classesDest = Some(Paths.get(classesDest))), true, rest)
        case flag +: rest if allowOptions && flag.startsWith("-") =>
          reporter.out.println(s"unknown flag $flag")
          None
        case classesFrom +: classesTo +: rest =>
          loop(
            settings.copy(classesFrom = Some(Paths.get(classesFrom)),
                          classesTo = Some(Paths.get(classesTo))),
            true, rest)
        case Nil =>
          Some(settings)
      }
    }
    loop(Settings(), true, args)
      .hasPath("from sources root")(_.sourceFrom)
      .hasPath("to sources root")(_.sourceTo)
      .hasPath("from classes root")(_.classesFrom)
      .hasPath("to classes root")(_.classesTo)
      .hasPath("destination classes root")(_.classesDest)
  }
}
