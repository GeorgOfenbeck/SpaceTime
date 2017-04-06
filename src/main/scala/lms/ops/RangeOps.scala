package scala.lms
package ops

import java.io.PrintWriter
import org.scala_lang.virtualized.SourceContext
import scala.lms.internal._
import util.OverloadHack

trait RangeOps extends OverloadHack {
  this: ImplicitOps =>
  // workaround for infix not working with manifests
  implicit def repRangeToRangeOps(r: Rep[Range]) = new rangeOpsCls(r)
  class rangeOpsCls(r: Rep[Range]){
    //def foldLeft[B](ini: B)(f: ((B,Rep[Int])) => B) = range_foldLeft(r,ini,f)
    //def foreach[B](f: (B,Rep[Int]) => Rep[B])(implicit pos: SourceContext) = range_foldLeft(r, f)
  }
  //def range_foldLeft[B](r: Rep[Range], ini: B, body: ((B,Rep[Int])) => B)(implicit exposeB: ExposeRep[B]): B
  
  def range_create(start: Rep[Int], end: Rep[Int]): Rep[Range]
}

trait RangeOpsExp extends RangeOps with BaseExp with FunctionsExp{
  this: ImplicitOpsExp =>
  case class Until(start: Exp[Int], end: Exp[Int]) extends Def[Range]
  case class RangeStart(r: Exp[Range]) extends Def[Int]
  case class RangeStep(r: Exp[Range]) extends Def[Int]
  case class RangeEnd(r: Exp[Range]) extends Def[Int]
  //case class RangeForeach(r: Exp[Range], i: Exp[Int], body: Exp[Unit]) extends Def[Unit]
  //case class RangeForeach(start: Exp[Int], end: Exp[Int], i: Sym[Int], body: Block[Unit]) extends Def[Unit]

  def range_create(start: Exp[Int], end: Exp[Int]): Exp[Range] = Until(start,end)
  
  case class RangeFoldLeft[B](expose: ExposeRep[B], r: Exp[Range], ini: B, loopvar: Exp[Int], loopacc: B, body: Exp[_ => _]) extends Def[Any]
  def range_foldLeft[B](r: Exp[Range], ini: B, body: ((B,Exp[Int])) => B)(implicit exposeB: ExposeRep[B]): B = {

    val exposeTuple =  new ExposeRep[(B,Exp[Int])]() {
      val freshExps = (u: Unit) => exposeB.freshExps() :+ Arg[Int]
      val vec2t: Vector[Exp[_]] => (B,Exp[Int]) = (in: Vector[Exp[_]]) => {
        val b: B = exposeB.vec2t(in)
        val n: Exp[Int] = in.last.asInstanceOf[Rep[Int]]
        (b,n)
      }
      val t2vec: ((B,Exp[Int])) => Vector[Exp[_]] = (in: (B,Exp[Int])) => {
        exposeB.t2vec(in._1) :+ in._2
      }
    }

    val lambda = doInternalLambda(body, false, None)(exposeTuple,exposeB)
    val newsyms = exposeB.freshExps()
    val looptuple = exposeTuple.freshExps()
    val (loopacc,loopvar) = exposeTuple.vec2t(looptuple)
    val sumloopnode = RangeFoldLeft(exposeB, r, ini, loopvar, loopacc, lambda.exp)
    val sumnodeexp = toAtom(sumloopnode)

    val returnNodes = if (newsyms.size > 1) {
      newsyms.zipWithIndex.map(fsym => {
        //had do to this ugly version since the compile time type is not know at this stage (of the return - Rep[_])
        val otp = exp2tp(fsym._1)
        val tag: TypeRep[Any] = otp.tag.asInstanceOf[TypeRep[Any]]
        val cc: Def[Any] = ReturnArg(sumnodeexp, fsym._1, fsym._2, true, newsyms.size == fsym._2 + 1)
        val newx = toAtom(cc)(tag, null)
        newx
      })
    } else {
      newsyms.zipWithIndex.map(fsym => {
        val tag: TypeRep[Any] = exp2tp(fsym._1).tag.asInstanceOf[TypeRep[Any]]
        val cc: Def[Any] = ReturnArg(sumnodeexp, fsym._1, fsym._2, false, true)
        val newx = toAtom(cc)(tag, null)
        newx
      })
    }
    exposeB.vec2t(returnNodes)
  }
  







}
