package scala.meta.cli

import scala.meta.internal.reconciler.Main
import scala.meta.reconciler.Settings

object ReturnTypeReconciler {
  def main(args: Array[String]): Unit = {
    val reporter = Reporter().withOut(System.out).withErr(System.err)
    Settings.parse(args.toList, reporter) match {
      case Some(settings) =>
        if (process(settings, reporter)) sys.exit(0)
        else sys.exit(1)
      case None =>
        sys.exit(1)
    }
  }

  def process(settings: Settings, reporter: Reporter): Boolean = {
    val main = new Main(settings, reporter)
    main.process()
  }

}