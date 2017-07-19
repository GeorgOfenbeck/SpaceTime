package scala.lms
package internal

// TODO: add logging, etc.
trait Logging extends Config{
  def __ = throw new RuntimeException("unsupported embedded dsl operation")

  def printdbg(x: =>Any): Unit = { if (verbosity >= 2) System.err.println(x) }
  def printlog(x: =>Any): Unit =  { if (verbosity >= 1) System.err.println(x) }
  def printerr(x: =>Any): Unit =  { System.err.println(x); hadErrors = true }

  def printsrc(x: =>Any): Unit =  { if (sourceinfo >= 1) System.err.println(x) }
  
  var hadErrors = false
}

