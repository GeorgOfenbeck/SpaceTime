package scala.lms
package targets
package scalalike


import ops._
import scala.lms.internal.FunctionsExp

trait ScalaGenArrayOps extends ScalaCodegen{
 val IR: ArrayOpsExp with FunctionsExp
 import IR._

 override def emitNode(tp: TP[_], acc: Vector[String],
                       block_callback: (Block,Vector[String]) => Vector[String]): Vector[String] = {
  val ma = tp.rhs match {
   case a@ArrayNew(n) => Vector(emitValDef(tp, src"new Array[${remap(a.m)}]($n)"))
   case ArrayUpdate(x,n,y) => Vector(emitValDef(tp, src"$x($n) = $y"))
   case _ => super.emitNode(tp,acc,block_callback)
  }
  ma
 }
}
