package scala.lms
package targets
package clike

import ops.PurePrimitiveOpsExp
import scala.lms.targets.clike._

trait CLikeGenPrimitiveOps  extends CLikeCodegen {
  val IR: PurePrimitiveOpsExp with internal.FunctionsExp
  import IR._

  override def emitNode(tp: TP[_], acc: Vector[String],
                        block_callback: (Block,Vector[String]) => Vector[String]): Vector[String] = {
    val ma = tp.rhs match {
      case ObjDoubleMinValue() => Vector(emitValDef(tp, "DBL_MIN"))
      case ObjDoubleMaxValue() => Vector(emitValDef(tp, "DBL_MAX"))
      case DoublePlus(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " + " + quote(rhs)))
      case DoubleMinus(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " - " + quote(rhs)))
      case DoubleTimes(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " * " + quote(rhs)))
      case DoubleDivide(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " / " + quote(rhs)))
      case DoubleToInt(lhs) => Vector(emitValDef(tp, "(int32_t)" + quote(lhs)))
      case DoubleToFloat(lhs) => Vector(emitValDef(tp, "(float)" + quote(lhs)))
      case FloatToInt(lhs) => Vector(emitValDef(tp, "(int32_t)" + quote(lhs)))
      case FloatToDouble(lhs) => Vector(emitValDef(tp, "(double)" + quote(lhs)))
      case FloatPlus(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " + " + quote(rhs)))
      case FloatMinus(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " - " + quote(rhs)))
      case FloatTimes(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " * " + quote(rhs)))
      case FloatDivide(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " / " + quote(rhs)))
      case ObjIntMaxValue() => Vector(emitValDef(tp, "INT32_MAX"))
      case ObjIntMinValue() => Vector(emitValDef(tp, "INT32_MAX"))
      case IntPlus(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " + " + quote(rhs)))
      case IntMinus(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " - " + quote(rhs)))
      case IntTimes(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " * " + quote(rhs)))
      case IntDivide(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " / " + quote(rhs)))
      case IntMod(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " % " + quote(rhs)))
      case IntBinaryOr(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " | " + quote(rhs)))
      case IntBinaryAnd(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " & " + quote(rhs)))
      case IntBinaryXor(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " ^ " + quote(rhs)))
      case IntShiftLeft(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " << " + quote(rhs)))
      case IntShiftRightArith(lhs, rhs) => Vector(emitValDef(tp, quote(lhs) + " >> " + quote(rhs)))
      case IntShiftRightLogical(lhs, rhs) => Vector(emitValDef(tp, "(uint32_t)" + quote(lhs) + " >> " + quote(rhs)))
      case IntBitwiseNot(lhs) => Vector(emitValDef(tp, "~" + quote(lhs)))
      case IntToLong(lhs) => Vector(emitValDef(tp, "(int64_t)"+quote(lhs)))
      case IntToFloat(lhs) => Vector(emitValDef(tp, "(float)"+quote(lhs)))
      case IntToDouble(lhs) => Vector(emitValDef(tp, "(double)"+quote(lhs)))
      case ObjLongMaxValue() => Vector(emitValDef(tp, "INT64_MAX"))
      case ObjLongMinValue() => Vector(emitValDef(tp, "INT64_MIN"))
      case LongBinaryOr(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " | " + quote(rhs)))
      case LongBinaryAnd(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " & " + quote(rhs)))
      case LongBinaryXor(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " ^ " + quote(rhs)))
      case LongShiftLeft(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " << " + quote(rhs)))
      case LongShiftRightSigned(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " >> " + quote(rhs)))
      case LongShiftRightUnsigned(lhs,rhs) => Vector(emitValDef(tp, "(uint64_t)" + quote(lhs) + " >> " + quote(rhs)))
      case LongToInt(lhs) => Vector(emitValDef(tp, "(int32_t)"+quote(lhs)))
      case LongToFloat(lhs) => Vector(emitValDef(tp, "(float)"+quote(lhs)))
      case LongToDouble(lhs) => Vector(emitValDef(tp, "(double)"+quote(lhs)))
      case LongPlus(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " + " + quote(rhs)))
      case LongMinus(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " - " + quote(rhs)))
      case LongTimes(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " * " + quote(rhs)))
      case LongDivide(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " / " + quote(rhs)))
      case LongMod(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " % " + quote(rhs)))
      case _ => super.emitNode(tp,acc,block_callback)
    }
    ma
  }
}