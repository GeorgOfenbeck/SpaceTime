package scala.lms
package ops

import java.io.PrintWriter
import org.scala_lang.virtualized.SourceContext
import scala.lms.internal.Expressions
import util.OverloadHack


trait LiftString {
  this: StringOpsExp =>

  implicit def strToRepStr(s: String) = unit(s)
}

trait StringOps extends OverloadHack {
  this: ImplicitOps =>
// NOTE: if something doesn't get lifted, this won't give you a compile time error,
  //       since string concat is defined on all objects

  // TODO(trans) need to check if string concat works
  //def infix_+(s1: String, s2: Rep[Any])(implicit o: Overloaded1, pos: SourceContext) = string_plus(unit(s1), s2)
  //def infix_+(s1: Rep[String], s2: Rep[Any])(implicit o: Overloaded1, pos: SourceContext) = string_plus(s1, s2)
  def infix_+(s1: Rep[String], s2: Rep[String])(implicit o: Overloaded3, pos: SourceContext) = string_plus(s1, s2)
  //def infix_+(s1: Rep[Any], s2: Rep[String])(implicit o: Overloaded5, pos: SourceContext) = string_plus(s1, s2)
  //def infix_+(s1: Rep[Any], s2: String)(implicit o: Overloaded7, pos: SourceContext) = string_plus(s1, unit(s2))

  implicit class StringOpsInfixRepString(s1: Rep[String]) {
    def startsWith(s2: Rep[String])(implicit pos: SourceContext) = string_startswith(s1,s2)
    def trim(separators: Rep[String])(implicit pos: SourceContext) = string_split(s1, separators, unit(0))
    def split(separators: Rep[String], limit: Rep[Int])(implicit pos: SourceContext) = string_split(s1, separators, limit)
    def charAt(i: Rep[Int])(implicit pos: SourceContext) = string_charAt(s1,i)
    def endsWith(e: Rep[String])(implicit pos: SourceContext) = string_endsWith(s1,e)
    def contains(s2: Rep[String])(implicit pos: SourceContext) = string_contains(s1,s2)
    def toDouble(start: Rep[Int], end: Rep[Int])(implicit pos: SourceContext) = string_substring(s1,start,end)
    // TODO(trans) check if FIXME still valid
    // FIXME: enabling this causes trouble with DeliteOpSuite. investigate!!
    def length(s1: Rep[String])(implicit pos: SourceContext) = string_length(s1)
  }

  object String {
    def valueOf(a: Rep[Any])(implicit pos: SourceContext) = string_valueof(a)
  }

  //def string_plus(s: Rep[Any], o: Rep[Any])(implicit pos: SourceContext): Rep[String]
  def string_plus(s: Rep[String], o: Rep[String])(implicit pos: SourceContext): Rep[String]
  def string_startswith(s1: Rep[String], s2: Rep[String])(implicit pos: SourceContext): Rep[Boolean]
  def string_trim(s: Rep[String])(implicit pos: SourceContext): Rep[String]
  def string_split(s: Rep[String], separators: Rep[String], limit: Rep[Int])(implicit pos: SourceContext): Rep[Array[String]]
  def string_valueof(d: Rep[Any])(implicit pos: SourceContext): Rep[String]
  def string_charAt(s: Rep[String], i: Rep[Int])(implicit pos: SourceContext): Rep[Char]
  def string_endsWith(s: Rep[String], e: Rep[String])(implicit pos: SourceContext): Rep[Boolean]
  def string_contains(s1: Rep[String], s2: Rep[String])(implicit pos: SourceContext): Rep[Boolean]
  def string_todouble(s: Rep[String])(implicit pos: SourceContext): Rep[Double]
  def string_tofloat(s: Rep[String])(implicit pos: SourceContext): Rep[Float]
  def string_toint(s: Rep[String])(implicit pos: SourceContext): Rep[Int]
  def string_tolong(s: Rep[String])(implicit pos: SourceContext): Rep[Long]
  def string_substring(s: Rep[String], start:Rep[Int], end:Rep[Int])(implicit pos: SourceContext): Rep[String]
  def string_length(s: Rep[String])(implicit pos: SourceContext): Rep[Int]
}

trait StringOpsExp extends StringOps with BaseExp {
  this: ImplicitOpsExp =>
  case class StringPlus(s: Exp[String], o: Exp[String]) extends Def[String]
  case class StringStartsWith(s1: Exp[String], s2: Exp[String]) extends Def[Boolean]
  case class StringTrim(s: Exp[String]) extends Def[String]
  case class StringSplit(s: Exp[String], separators: Exp[String], limit: Exp[Int]) extends Def[Array[String]]
  case class StringEndsWith(s: Exp[String], e: Exp[String]) extends Def[Boolean]
  case class StringCharAt(s: Exp[String], i: Exp[Int]) extends Def[Char]
  case class StringValueOf(a: Exp[Any]) extends Def[String]
  case class StringToDouble(s: Exp[String]) extends Def[Double]
  case class StringToFloat(s: Exp[String]) extends Def[Float]
  case class StringToInt(s: Exp[String]) extends Def[Int]
  case class StringContains(s1: Exp[String], s2: Exp[String]) extends Def[Boolean]
  case class StringToLong(s: Exp[String]) extends Def[Long]
  case class StringSubstring(s: Exp[String], start:Exp[Int], end:Exp[Int]) extends Def[String]
  case class StringLength(s: Exp[String]) extends Def[Int]

  def string_plus(s: Exp[String], o: Exp[String])(implicit pos: SourceContext): Rep[String] = StringPlus(s,o)
  def string_startswith(s1: Exp[String], s2: Exp[String])(implicit pos: SourceContext) = StringStartsWith(s1,s2)
  def string_trim(s: Exp[String])(implicit pos: SourceContext) : Rep[String] = StringTrim(s)
  def string_split(s: Exp[String], separators: Exp[String], limit: Exp[Int])(implicit pos: SourceContext) : Rep[Array[String]] = StringSplit(s, separators, limit)
  def string_valueof(a: Exp[Any])(implicit pos: SourceContext) = StringValueOf(a)
  def string_charAt(s: Exp[String], i: Exp[Int])(implicit pos: SourceContext) = StringCharAt(s,i)
  def string_endsWith(s: Exp[String], e: Exp[String])(implicit pos: SourceContext) = StringEndsWith(s,e)
  def string_contains(s1: Exp[String], s2: Exp[String])(implicit pos: SourceContext) = StringContains(s1,s2)
  def string_todouble(s: Rep[String])(implicit pos: SourceContext) = StringToDouble(s)
  def string_tofloat(s: Rep[String])(implicit pos: SourceContext) = StringToFloat(s)
  def string_toint(s: Rep[String])(implicit pos: SourceContext) = StringToInt(s)
  def string_tolong(s: Rep[String])(implicit pos: SourceContext) = StringToLong(s)
  def string_substring(s: Rep[String], start:Rep[Int], end:Rep[Int])(implicit pos: SourceContext) = StringSubstring(s,start,end)
  def string_length(s: Rep[String])(implicit pos: SourceContext) = StringLength(s)

}
