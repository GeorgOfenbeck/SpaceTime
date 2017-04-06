package scala.lms
package targets
package javalike

import java.io._
import scala.tools.nsc._
import scala.tools.nsc.util._
import scala.tools.nsc.reporters._
import scala.tools.nsc.io._
import scala.tools.nsc.interpreter.AbstractFileClassLoader

trait JavaCompile {

  val codegen: JavaCodegen with TupleHelper

  var compiler: Global = _
  var reporter: ConsoleReporter = _

  def setupCompiler():Global = {
    /*
      output = new ByteArrayOutputStream()
      val writer = new PrintWriter(new OutputStreamWriter(output))
    */
    val settings = new Settings()
    val pathSeparator = System.getProperty("path.separator")

    settings.classpath.value = this.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(pathSeparator)
      case _ => System.getProperty("java.class.path")
    }
    settings.bootclasspath.value = Predef.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(pathSeparator)
      case _ => System.getProperty("sun.boot.class.path")
    }
    settings.encoding.value = "UTF-8"
    settings.outdir.value = "."
    settings.extdirs.value = ""
    //settings.verbose.value = true
    // -usejavacp needed on windows?

    reporter = new ConsoleReporter(settings, null, new PrintWriter(System.out))//writer
    new Global(settings, reporter)
  }


  var compileCount = 0
  var dumpGeneratedCode = true


  def compile[A,R](f: Function1[A,R])(implicit args: codegen.IR.ExposeRep[A], returns: codegen.IR.ExposeRep[R]) = {
    if (this.compiler eq null)
      setupCompiler()

    val className = "staged$" + compileCount
    compileCount += 1

    val source = new StringWriter()
    val writer = new PrintWriter(source)
    val esc = codegen.emitSource(f, className, writer)
    val staticData: List[(codegen.IR.Exp[_], Any)] = List()

    writer.println(codegen.emitDataStructures())

    if (dumpGeneratedCode) println(source)

    val compiler = this.compiler
    val run = new compiler.Run

    val fileSystem = new VirtualDirectory("<vfs>", None)
    compiler.settings.outputDirs.setSingleOutput(fileSystem)
    //      compiler.genJVM.outputDir = fileSystem

    run.compileSources(List(new util.BatchSourceFile("<stdin>", source.toString)))
    reporter.printSummary()

    if (!reporter.hasErrors)
      println("compilation: ok")
    else
      println("compilation: had errors")

    reporter.reset
    //output.reset

    val parent = this.getClass.getClassLoader
    val loader = new AbstractFileClassLoader(fileSystem, this.getClass.getClassLoader)

    val cls: Class[_] = loader.loadClass(className)

    val cons = cls.getConstructor(staticData.map(_._1.tp.erasure):_*)


    //val obj: A=>R = cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]):_*).asInstanceOf[A=>R]

    val obj: Any=>Any = cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]):_*).asInstanceOf[Any=>Any]
    //GO: making this any to any since we unparse into Tuples not A=>R
    (obj,esc)
  }


}
