package scala.lms
package targets
package scalalike


import ops._


trait ScalaGenStringOps  extends ScalaCodegen{
  val IR: StringOpsExp with internal.FunctionsExp
  import IR._

  override def emitNode(tp: TP[_], acc: Vector[String],
                        block_callback: (Block,Vector[String]) => Vector[String]): Vector[String] = {
    val ma = tp.rhs match {
      case StringPlus(s1,s2) => Vector(emitValDef(tp, src"$s1+$s2"))
      case StringStartsWith(s1,s2) => Vector(emitValDef(tp, src"$s1.startsWith($s2)"))
      case StringTrim(s) => Vector(emitValDef(tp, src"$s.trim()"))
      case StringSplit(s, sep, l) => Vector(emitValDef(tp, src"$s.split($sep,$l)"))
      case StringEndsWith(s, e) => Vector(emitValDef(tp, "%s.endsWith(%s)".format(quote(s), quote(e))))
      case StringCharAt(s,i) => Vector(emitValDef(tp, "%s.charAt(%s)".format(quote(s), quote(i))))
      case StringValueOf(a) => Vector(emitValDef(tp, src"java.lang.String.valueOf($a)"))
      case StringToDouble(s) => Vector(emitValDef(tp, src"$s.toDouble"))
      case StringToFloat(s) => Vector(emitValDef(tp, src"$s.toFloat"))
      case StringToInt(s) => Vector(emitValDef(tp, src"$s.toInt"))
      case StringToLong(s) => Vector(emitValDef(tp, src"$s.toLong"))
      case StringContains(s1,s2) => Vector(emitValDef(tp, "%s.contains(%s)".format(quote(s1),quote(s2))))
      case StringSubstring(s,a,b) => Vector(emitValDef(tp, src"$s.substring($a,$b)"))
      case StringLength(s) => Vector(emitValDef(tp, src"$s.length"))

      case _ => super.emitNode(tp,acc,block_callback)
    }
    ma
  }
}
