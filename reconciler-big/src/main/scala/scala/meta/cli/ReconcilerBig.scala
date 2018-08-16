package scala.meta.cli

import java.nio.file.{Files, Paths}
import scala.meta.metadiff
import scala.meta.reconciler

object ReconcilerBig {

  def main(args: Array[String]): Unit = {
    // 1 is with fix, 2 is without fix
    val sd1 = new DirectoriesParser(Paths.get(args(0))).apply().groupBy(_.name)
    val sd2 = new DirectoriesParser(Paths.get(args(1))).apply().groupBy(_.name)
    val src1 = Paths.get(args(2))
    val src2 = Paths.get(args(3))
    val reporter = Reporter()
    if (sd1.keySet != sd2.keySet) sys.error("expected targets to be the same between sources")
    var i = 1
    sd1.keys.foreach { targetName =>
      val Seq(target1) = sd1(targetName)
      val Seq(target2) = sd2(targetName)
      println(s"diffing ($i/${sd1.keys.size}) ${target1} ${target2}")
      val tmpDir = Files.createTempDirectory(s"reconciler-big-$targetName")
      val fixedClassesDir = tmpDir.resolve("classes")
      Files.createDirectories(fixedClassesDir)
      val reconcilerSettings = reconciler.Settings(
        sourceFrom = Some(src2),
        sourceTo = Some(src1),
        classesFrom = Some(target2.classes),
        classesTo = Some(target1.classes),
        classesDest = Some(fixedClassesDir)
      )
      ReturnTypeReconciler.process(reconcilerSettings, reporter)
      val settings = metadiff.Settings(
        paths = List(target2.classes, fixedClassesDir),
        compareSymbols = false,
        compareOccurrences = false,
        compareOrder = false,
        compareSynthetics = true
      )
      Metadiff.process(settings, reporter)
      i += 1
    }
  }

}
