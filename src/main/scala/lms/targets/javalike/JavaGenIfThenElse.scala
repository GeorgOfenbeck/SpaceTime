package scala.lms
package targets
package javalike

import ops._
import scala.lms.internal.FunctionsExp

trait JavaGenIfThenElse extends JavaCodegen with TupleHelper{
  val IR: IfThenElsePureExp with FunctionsExp
  import IR._

  override def emitNode(tp: TP[_], acc: Vector[String],
                        block_callback: (Block,Vector[String]) => Vector[String]): Vector[String] = tp.rhs match {
    case myIfThenElse(cond,thenp,elsep,resexpose) => {
      val thenlambda = exp2tp(thenp)
      val elselambda = exp2tp(elsep)

      val rets: Vector[String] = (thenlambda.rhs,elselambda.rhs) match {
        case (InternalLambda(tf,tx,ty,thot,targs,treturns),InternalLambda(ef,ex,ey,ehot,eargs,ereturns)) => Vector({
          val l1 = "val " + quote(tp) + " = if (" + quote(cond) + ") {\n"
          val l2 = block_callback(ty,Vector(l1))
          val trestuple: Vector[String] = ty.res.map(r => quote(r))
          val l3: String = l2.mkString("") + tupledeclarehelper(trestuple,"")
          val l4 = l3 + "\n} else {\n"
          val l5 = block_callback(ey,Vector(l4))
          val erestuple: Vector[String] = ey.res.map(r => quote(r))
          val l6: String = l5.mkString("") + tupledeclarehelper(erestuple,"")
          l6 + "\n}\n"
        } )
        case _ => {
          assert(false, "got an if statment which does not contain lambdas for its branches")
          Vector.empty
        }
      }
      rets

    }
    /*case IfThenElse(c,a,b) =>
      val l1 = "val " + quote(tp) + " = if (" + quote(c) + ") {"
      val l2 = block_callback(a,l1)
      val l3: String = l2 + quote(getBlockResults(a).head) //RF - fix for multi result
      val l4 = l3 + "} else {"
      val l5 = block_callback(b,l4)
      val l6: String = l5 + quote(getBlockResults(b).head) //RF - fix for multi result
      l6 + "}"*/
    case _ => super.emitNode(tp,acc,block_callback)
  }
}
