// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.build

import java.io._
import java.io.File.pathSeparator
import java.lang.ProcessBuilder._
import scala.collection.JavaConverters._
import sbt._
import sbt.Keys._
import sbt.plugins._
import complete.DefaultParsers._

object Build extends AutoPlugin {
  override def requires: Plugins = JvmPlugin
  override def trigger: PluginTrigger = allRequirements
  import autoImport._

  private lazy val buildRoot: File = file("").getAbsoluteFile
  private lazy val scalafixRulesProject = ProjectRef(file(""), "scalafixRules")

  private def command(commands: String*): String = command(commands.toList)
  private def command(commands: List[String]): String = {
    commands.map(c => s";$c ").mkString("")
  }

  private def shellout(command: List[String], cwd: File): Unit = {
    val builder = new java.lang.ProcessBuilder()
    builder.command(command.asJava)
    builder.directory(cwd)
    builder.redirectOutput(Redirect.INHERIT)
    builder.redirectError(Redirect.INHERIT)
    val exitcode = builder.start().waitFor()
    if (exitcode != 0) {
      val what = command.mkString(" ")
      sys.error(s"$what in $cwd has failed with code $exitcode")
    }
  }
  private def scalafmt(args: List[String], cwd: File): Unit = {
    val bin = new File(buildRoot, "bin/scalafmt").abs
    shellout(bin +: args, cwd)
  }
  private def scalafix(args: List[String], cwd: File): Unit = {
    val bin = new File(buildRoot, "bin/scalafix").abs
    shellout(bin +: args, cwd)
  }

  implicit class FileOps(file: File) {
    def abs: String = file.getAbsolutePath
  }

  object autoImport {
    val shell = inputKey[Unit]("Run shell command")
    val scalafmtFormat = taskKey[Unit]("Automatically format all files")
    val scalafmtTest = taskKey[Unit]("Test that all files are formatted")
    val rewrite = taskKey[Unit]("Rewrite the project to be compatible with Rsc")

    def computeScalaVersionFromTravisYml(prefix: String): String = {
      val travisYml = IO.read(file(".travis.yml"))
      val scalaRegex = (prefix + ".\\d+").r
      val scalaMatch = scalaRegex.findFirstMatchIn(travisYml)
      scalaMatch.map(_.group(0)).get
    }

    def computeScalafixVersionFromBinScalafix(): String = {
      val binScalafix = IO.read(file("bin/scalafix"))
      val scalaRegex = "VERSION=\"(.*?)\"".r
      val scalaMatch = scalaRegex.findFirstMatchIn(binScalafix)
      scalaMatch.map(_.group(1)).get
    }

    object ui {
      lazy val ciFmt = "scalafmtTest"
      lazy val ciFast = fastTest
      lazy val ciSlow = slowTest
      lazy val ciScalafix = scalafixTest
      lazy val ci = command(ciFmt, ciFast, ciSlow, ciScalafix)
      lazy val cleanAll = command(
        "reload",
        "bench/clean",
        "check/clean",
        "core/clean",
        "function/clean",
        "mjar/clean",
        "rsc/clean",
        "scalafixInput/clean",
        "scalafixOutput/clean",
        "scalafixRules/clean",
        "scalafixTests/clean",
        "scalasig/clean",
        "scalap/clean",
        "tests/clean"
      )
      lazy val compileAll = command(
        "bench/compile",
        "check/compile",
        "core/compile",
        "function/compile",
        "mjar/compile",
        "rsc/compile",
        "scalafixInput/compile",
        "scalafixOutput/compile",
        "scalafixRules/compile",
        "scalafixTests/compile",
        "scalafixTests/test:compile",
        "scalasig/compile",
        "scalap/compile",
        "tests/compile",
        "tests/test:compile"
      )
      lazy val fmtAll = "scalafmtFormat"
      lazy val testAll = command(
        "reload",
        "cleanAll",
        ci
      )
      lazy val benchAll = command(
        "cleanAll",
        "compileAll",
        "bench/jmh:run RscParse RscLink RscOutline RscSemanticdb RscMjar ScalacCompile"
      )
      lazy val publishAll = command(
        "check/publish",
        "mjar/publish",
        "rsc/publish",
        "scalasig/publish",
        "scalap/publish"
      )
      lazy val publishLocal = command(
        "check/publishLocal",
        "mjar/publishLocal",
        "rsc/publishLocal",
        "scalasig/publishLocal",
        "scalap/publishLocal"
      )
      lazy val compile = "tests/test:compile"
      lazy val fastTest = "tests/fast:test"
      lazy val slowTest = command("tests/slow:test")
      lazy val scalafixTest = "scalafixTests/test"
      lazy val test = fastTest
      lazy val benchParse = "bench/jmh:run RscParse"
      lazy val benchLink = "bench/jmh:run RscLink"
      lazy val benchOutline = "bench/jmh:run RscOutline"
      lazy val benchSemanticdb = "bench/jmh:run RscSemanticdb"
      lazy val benchMjar = "bench/jmh:run RscMjar"
      lazy val rewrite = "core/rewrite"
    }

    lazy val isCI = sys.env.contains("CI")

    // https://stackoverflow.com/questions/41229451/how-to-disable-slow-tagged-scalatests-by-default-allow-execution-with-option
    lazy val Fast = config("fast").extend(Test)
    lazy val Slow = config("slow").extend(Test)
  }

  override lazy val globalSettings: Seq[Def.Setting[_]] = List(
    scalafmtFormat := {
      scalafmt(List("--non-interactive"), buildRoot)
    },
    scalafmtTest := {
      scalafmt(List("--test", "--non-interactive", "--quiet"), buildRoot)
    },
    shell := {
      val args = spaceDelimited("<arg>").parsed
      val command = args.mkString(" ")
      val retcode = command.!
      if (retcode != 0) sys.error(s"$command returned $retcode")
    }
  )

  override lazy val projectSettings: Seq[Def.Setting[_]] = List(
    rewrite := {
      val toolClasspath = fullClasspath.in(scalafixRulesProject, Compile).value
      val args = List.newBuilder[String]
      args += "--tool-classpath"
      args += toolClasspath.map(_.data.abs).mkString(pathSeparator)
      args += "--classpath"
      args += products.in(Compile).value.map(_.abs).mkString(pathSeparator)
      args += "--sourceroot"
      args += buildRoot.abs
      args += "--rules"
      args += "scala:scalafix.internal.rule.RscCompat"
      args += baseDirectory.value.abs
      scalafix(args.result, baseDirectory.value)
    }
  )
}
