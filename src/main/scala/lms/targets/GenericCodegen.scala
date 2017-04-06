package scala.lms
package targets

import java.io.PrintWriter

import internal._
import scala.reflect._

trait GenericCodegen extends Emit[Vector[String]]{
  self =>
  val IR: BaseExp with FunctionsExp
  import IR._

  def emitDataStructures(): String = {""}


  override def emitc(start: Vector[String],
                     it: Iterator[self.IR.TP[_]],
                     block_callback: (self.IR.Block,Vector[String]) => Vector[String] ): Vector[String] = {
    it.foldLeft(start){
      (acc,ele) => {
        val t: Vector[String] = emitNode(ele,acc,block_callback)
        acc ++ t
      }
    }
  }

  def emitStream(stream: PrintWriter, acc: Vector[String], it: Iterator[self.IR.TP[_]], block_callback: (self.IR.Block, Vector[String]) => Vector[String]): Unit = {
    acc.map(stream.print(_))
    for (ele <- it) {
      val x = ele
      val t: Vector[String] = emitNode(x,acc,block_callback)
      t map (ele => stream.print(ele))
    }
/*    it.map(ele => {
      val t = emitNode(ele,acc,block_callback)
      stream.print(t)
    })*/
  }



  def emit[A,R]( stream: PrintWriter, start: Vector[String], f: Function1[A,R])(implicit args: IR.ExposeRep[A], returns: IR.ExposeRep[R]): specEsc
  = {
    val schedule = new Schedule {
      override val IR: self.IR.type = self.IR
    }
    emit(stream, schedule,start,f)(args,returns)
  }

  def emit[A,R]( stream: PrintWriter, schedule: Schedule{ val IR: self.IR.type},
                 start: Vector[String],
                 f: Function1[A,R])
               (implicit args: IR.ExposeRep[A], returns: IR.ExposeRep[R]): (specEsc)  = {
    val reify = new ReifyPure {
      override val IR: self.IR.type = self.IR
    }
    stream.println("//bla!")
    printlog("starting reify")
    val reification = reify.reifyProgram(f)(args, returns)
    printlog("starting codemotion")
    val cm: specCM = CodeMotion(reification)
    printlog("starting schedule")
    val exposedScheduleChoice: specEsc = ExposeScheduleChoice(cm)
    printlog("starting getting Iterator")
    val iteratable = schedule.getSchedulewithIterator(exposedScheduleChoice)
    def blockcallback (block: self.IR.Block, bstart: Vector[String]): Vector[String] = {
      val bit = iteratable.iterator(block)
      emitStream(stream,bstart,bit,blockcallback)
      Vector.empty
    }
    printlog("starting iterating")
    //emitStream(start,iteratable.iterator,blockcallback)
    emitStream(stream,start,iteratable.iterator,blockcallback)
    stream.println("//bla end!")
    printlog("finished iterating")
    exposedScheduleChoice
  }


  //
  def getBlockResults[A](s: Block): Vector[Exp[_]] = s.res




  def quote(x: Exp[_]) : String = {
    if (x == null)
      assert(false, "how does that happen?")
    val tp: TP[_] = id2tp(x.id)
    quote(tp)
  }

  def quote(x: TP[_]) : String = x.sym match {
    case Const(s: String) => "\""+s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n")+"\"" // TODO: more escapes?
    case Const(c: Char) => "'"+(""+c).replace("'", "\\'").replace("\n", "\\n")+"'"
    case Const(f: Float) => "%1.10f".format(f) + "f"
    case Const(l: Long) => l.toString + "L"
    case Const(null) => "null"
    case Const(z) => z.toString
    case Exp(n) => "x"+n
    case _ => throw new RuntimeException("could not quote " + x)
  }


  // Provides automatic quoting and remapping in the gen string interpolater
  implicit class CodegenHelper(sc: StringContext) {
    def printToStream(arg: Any): String = {
      quoteOrRemap(arg)
    }

    def quoteOrRemap(arg: Any): String = arg match {
      case xs: Seq[_] => xs.map(quoteOrRemap).mkString(",")
      //case tp: TP[_] => quote(tp)
      case exp: Exp[_] => quote(exp)
      case m: Manifest[_] => remap(m)
      case s: String => s
      case _ => throw new RuntimeException(s"Could not quote or remap $arg")
    }

    // First line of a part of the context may contain
    // a | and therefore should not be stripped
    def stripContextPart(part: String): String = {
      val lines = part.linesWithSeparators
      if (!lines.hasNext) part
      else lines.next + (lines.foldLeft("")(_+_).stripMargin)
    }

    def src(args: Any*): String = {
      sc.raw(args.map(quoteOrRemap): _*).stripMargin
    }

    def gen(args: Any*): String = {
      sc.checkLengths(args)
      val start :: contextStrings = sc.parts.iterator.toList
      val s0 = start.stripMargin

      (args zip contextStrings).foldLeft(s0){
        (acc,ele) => {
          val (arg,contextString) = ele
          acc + arg + stripContextPart(contextString)
        }
      }
    }
  }


  // optional type remapping (default is identity)
  def remap(s: String): String = s
  def remap[A](s: String, method: String, t: Manifest[A]) : String = remap(s, method, t.toString)
  def remap(s: String, method: String, t: String) : String = s + method + "[" + remap(t) + "]"
  def remap[A](m: Manifest[A]): String = m match {
     //TODO: GO -> check with Vojin / Tiark - this got lost in the macro-trans - do we care?
    //case rm: RefinedManifest[A] =>  "AnyRef{" + rm.fields.foldLeft(""){(acc, f) => {val (n,mnf) = f; acc + "val " + n + ": " + remap(mnf) + ";"}} + "}"
    case _ if m.erasure == classOf[Variable[Any]] =>
      remap(m.typeArguments.head)
    case _ =>
      // call remap on all type arguments
      val targs = m.typeArguments
      if (targs.length > 0) {
        val ms = m.toString
        ms.take(ms.indexOf("[")+1) + targs.map(tp => remap(tp)).mkString(", ") + "]"
      }
      else m.toString
  }
  def remapImpl[A](m: Manifest[A]): String = remap(m)


}
