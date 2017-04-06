package scala.lms
package targets
package javalike

import ops.PurePrimitiveOpsExp

trait JavaGenPrimitivOps extends JavaCodegen{
 val IR: PurePrimitiveOpsExp with internal.FunctionsExp
 import IR._

 override def emitNode(tp: TP[_], acc: Vector[String],
                       block_callback: (Block,Vector[String]) => Vector[String]): Vector[String] = {
  val ma = tp.rhs match {
      
   //case ObjDoubleParseDouble(s) => Vector(emitValDef(tp, src"java.lang.Double.parseDouble($s)")
   case ObjDoublePositiveInfinity() => Vector(emitValDef(tp, "scala.Double.PositiveInfinity"))
   case ObjDoubleNegativeInfinity() => Vector(emitValDef(tp, "scala.Double.NegativeInfinity"))
   case ObjDoubleMinValue() => Vector(emitValDef(tp, "scala.Double.MinValue") )
   case ObjDoubleMaxValue() => Vector(emitValDef(tp, "scala.Double.MaxValue"))
   //case DoubleFloatValue(lhs) => Vector(emitValDef(tp, quote(lhs) + ".floatValue()")
   case DoublePlus(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " + " + quote(rhs)))
   case DoubleMinus(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " - " + quote(rhs)))
   case DoubleTimes(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " * " + quote(rhs)))
   case DoubleDivide(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " / " + quote(rhs)))
   case DoubleToInt(lhs) => Vector(emitValDef(tp, quote(lhs) + ".toInt"))
   case DoubleToFloat(lhs) => Vector(emitValDef(tp, quote(lhs) + ".toFloat"))
   //case ObjFloatParseFloat(s) => Vector(emitValDef(tp, "java.lang.Float.parseFloat(" + quote(s) + ")")
   case FloatToInt(lhs) => Vector(emitValDef(tp, quote(lhs) + ".toInt"))
   case FloatToDouble(lhs) => Vector(emitValDef(tp, quote(lhs) + ".toDouble"))
   case FloatPlus(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " + " + quote(rhs)))
   case FloatMinus(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " - " + quote(rhs)))
   case FloatTimes(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " * " + quote(rhs)))
   case FloatDivide(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " / " + quote(rhs)))
   //case ObjIntegerParseInt(s) => Vector(emitValDef(tp, "java.lang.Integer.parseInt(" + quote(s) + ")")
   case ObjIntMaxValue() => Vector(emitValDef(tp, "scala.Int.MaxValue"))
   case ObjIntMinValue() => Vector(emitValDef(tp, "scala.Int.MinValue"))
   case IntPlus(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " + " + quote(rhs)))
/*   case IntPlus(lhs,rhs) => {
//    println("BE AWARE THAT WE ON PURPOSE INTRODUCED A BUG HERE!")
    Vector(emitValDef(tp, "if (" + quote(lhs) + "%2 == 0) 7 else " + quote(lhs) + " + " + quote(rhs)))
   }*/
   case IntMinus(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " - " + quote(rhs)))
   case IntTimes(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " * " + quote(rhs)))
   // case IntDivideFrac(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " / " + quote(rhs)))
   case IntDivide(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " / " + quote(rhs)))
   case IntMod(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " % " + quote(rhs)))
   case IntBinaryOr(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " | " + quote(rhs)))
   case IntBinaryAnd(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " & " + quote(rhs)))
   case IntBinaryXor(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " ^ " + quote(rhs)))
   case IntShiftLeft(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " << " + quote(rhs)))
   case IntShiftRightArith(lhs, rhs) => Vector(emitValDef(tp, quote(lhs) + " >> " + quote(rhs)))
   case IntShiftRightLogical(lhs, rhs) => Vector(emitValDef(tp, quote(lhs) + " >>> " + quote(rhs)))
   //case IntDoubleValue(lhs) => Vector(emitValDef(tp, quote(lhs) + ".doubleValue()")
   //case IntFloatValue(lhs) => Vector(emitValDef(tp, quote(lhs) + ".floatValue()")
   case IntBitwiseNot(lhs) => Vector(emitValDef(tp, "~" + quote(lhs)))
   case IntToLong(lhs) => Vector(emitValDef(tp, quote(lhs) + ".toLong"))
   case IntToFloat(lhs) => Vector(emitValDef(tp, quote(lhs) + ".toFloat"))
   case IntToDouble(lhs) => Vector(emitValDef(tp, quote(lhs) + ".toDouble"))
   //case ObjLongParseLong(s) => Vector(emitValDef(tp, "java.lang.Long.parseLong(" + quote(s) + ")")
   case LongBinaryOr(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " | " + quote(rhs)))
   case LongBinaryAnd(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " & " + quote(rhs)))
   case LongShiftLeft(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " << " + quote(rhs)))
   case LongShiftRightUnsigned(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " >>> " + quote(rhs)))
   case LongToInt(lhs) => Vector(emitValDef(tp, quote(lhs) + ".toInt"))
   case LongPlus(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " + " + quote(rhs)))
   case LongMinus(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " - " + quote(rhs)))
   case LongTimes(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " * " + quote(rhs)))
   // case LongDivideFrac(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " / " + quote(rhs)))
   case LongDivide(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " / " + quote(rhs)))
   case LongMod(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " % " + quote(rhs)))
   case LongBinaryOr(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " | " + quote(rhs)))
   case LongBinaryAnd(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " & " + quote(rhs)))
   case LongBinaryXor(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " ^ " + quote(rhs)))
   case LongShiftLeft(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " << " + quote(rhs)))

   //case LongDoubleValue(lhs) => Vector(emitValDef(tp, quote(lhs) + ".doubleValue()")
   //case LongFloatValue(lhs) => Vector(emitValDef(tp, quote(lhs) + ".floatValue()")
   case LongToFloat(lhs) => Vector(emitValDef(tp, quote(lhs) + ".toFloat"))
   case LongToDouble(lhs) => Vector(emitValDef(tp, quote(lhs) + ".toDouble"))
   case _ => super.emitNode(tp,acc,block_callback)
  }
  ma
 }
}
