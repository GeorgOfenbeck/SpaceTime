package scala.lms
package targets
package javalike


import ops.BooleanOpsExp

trait JavaGenBooleanOps extends JavaCodegen{
  val IR: BooleanOpsExp
  import IR._

  override def emitNode(tp: TP[_], acc: Vector[String],
                        block_callback: (Block,Vector[String]) => Vector[String]): Vector[String] = {
    val ma = tp.rhs match {
      case BooleanNegate(b) => Vector(emitValDef(tp, src"!$b"))
      case BooleanAnd(lhs,rhs) => Vector(emitValDef(tp, src"$lhs && $rhs"))
      case BooleanOr(lhs,rhs) => Vector(emitValDef(tp, src"$lhs || $rhs"))
      case _ => super.emitNode(tp,acc,block_callback)
    }
    ma
  }
}
