// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.internal.mjar

import java.io._
import java.nio.file._
import java.util.jar._
import scala.collection.mutable
import scala.meta.cli._
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}
import scala.meta.mjar._
import scala.util.control.NonFatal

class Main(settings: Settings, reporter: Reporter) {
  def process(): Option[Path] = {
    try {
      val in = settings.classpath
      val out = settings.out
      Files.createDirectories(out.toAbsolutePath.getParent)
      val os = Files.newOutputStream(out)
      val bos = new BufferedOutputStream(os)
      val jos = new JarOutputStream(bos)
      try {
        val symtab = Symtab(in)
        val done = mutable.HashSet[String]()
        symtab.toplevels.foreach { sym =>
          if (!done(sym)) {
            try {
              val companionSym = {
                val desc = sym.desc
                if (desc.isTerm) Symbols.Global(sym.owner, d.Type(desc.name))
                else Symbols.Global(sym.owner, d.Term(desc.name))
              }

              val pickle = new Pickle(settings.abi, symtab, sym, companionSym)
              pickle.emitEmbeddedSym(sym, ToplevelMode)
              done += sym
              if (symtab.contains(companionSym)) {
                pickle.emitEmbeddedSym(companionSym, ToplevelMode)
                done += companionSym
              }

              val scalasig = pickle.toScalasig
              val classfile = scalasig.toClassfile
              jos.putNextEntry(new JarEntry(classfile.name + ".class"))
              jos.write(classfile.toBinary)
              jos.closeEntry()
            } catch {
              case NonFatal(ex) =>
                throw ConvertException(in, sym, ex)
            }
          }
        }
        Some(out)
      } finally {
        jos.close()
        bos.close()
        os.close()
      }
    } catch {
      case NonFatal(ex) =>
        ex.printStackTrace(reporter.out)
        None
    }
  }
}
