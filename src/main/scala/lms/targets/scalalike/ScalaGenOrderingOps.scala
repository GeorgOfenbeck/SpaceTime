package scala.lms
package targets
package scalalike

import ops._

trait ScalaGenOrderingOps extends ScalaCodegen {
  val IR: OrderingOpsExp with internal.FunctionsExp

  import IR._

  override def emitNode(tp: TP[_], acc: Vector[String],
                        block_callback: (Block, Vector[String]) => Vector[String]): Vector[String] = {
    val ma = tp.rhs match {
      case OrderingLT(a, b) => Vector(emitValDef(tp, src"$a < $b"))
      case OrderingLTEQ(a, b) => Vector(emitValDef(tp, src"$a <= $b"))
      case OrderingGT(a, b) => Vector(emitValDef(tp, src"$a > $b"))
      case OrderingGTEQ(a, b) => Vector(emitValDef(tp, src"$a >= $b"))
      case c@OrderingEquiv(a, b) => c.mev match {
        case m if m == Manifest.Int => Vector(emitValDef(tp, src"$a == $b"))
        case m if m == Manifest.Long => Vector(emitValDef(tp, src"$a == $b"))
        case m if m == Manifest.Double => Vector(emitValDef(tp, src"$a == $b"))
        case m if m == Manifest.Float => Vector(emitValDef(tp, src"$a == $b"))
        case m if m == Manifest.Boolean => Vector(emitValDef(tp, src"$a == $b"))
        case m if m == Manifest.Byte => Vector(emitValDef(tp, src"$a == $b"))
        case m if m == Manifest.Char => Vector(emitValDef(tp, src"$a == $b"))
        case m if m == Manifest.Short => Vector(emitValDef(tp, src"$a == $b"))
        case _ => Vector(emitValDef(tp, src"$a equiv $b"))

      }
      case OrderingMax(a, b) => Vector(emitValDef(tp, src"$a max $b"))
      case OrderingMin(a, b) => Vector(emitValDef(tp, src"$a min $b"))
      case c@OrderingCompare(a, b) => c.mev match {
        case m if m == Manifest.Int => Vector(emitValDef(tp, "java.lang.Integer.compare(" + quote(a) + "," + quote(b) + "))"))
        case m if m == Manifest.Long => Vector(emitValDef(tp, "java.lang.Long.compare(" + quote(a) + "," + quote(b) + "))"))
        case m if m == Manifest.Double => Vector(emitValDef(tp, "java.lang.Double.compare(" + quote(a) + "," + quote(b) + "))"))
        case m if m == Manifest.Float => Vector(emitValDef(tp, "java.lang.Float.compare(" + quote(a) + "," + quote(b) + "))"))
        case m if m == Manifest.Boolean => Vector(emitValDef(tp, "java.lang.Boolean.compare(" + quote(a) + "," + quote(b) + "))"))
        case m if m == Manifest.Byte => Vector(emitValDef(tp, "java.lang.Byte.compare(" + quote(a) + "," + quote(b) + "))"))
        case m if m == Manifest.Char => Vector(emitValDef(tp, "java.lang.Character.compare(" + quote(a) + "," + quote(b) + "))"))
        case m if m == Manifest.Short => Vector(emitValDef(tp, "java.lang.Short.compare(" + quote(a) + "," + quote(b) + "))"))
        case _ => Vector(emitValDef(tp, quote(a) + " compare " + quote(b)))
      }
      case _ => super.emitNode(tp, acc, block_callback)
    }
    ma
  }
}