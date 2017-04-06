
package scala.lms
package ops

import java.io.PrintWriter
import org.scala_lang.virtualized.SourceContext
import scala.lms.internal.Expressions
import util.OverloadHack


trait LiftPrimitives {
  this: PurePrimitiveOpsExp =>

  implicit def intToRepInt(x: Int) = unit(x)
  implicit def floatToRepFloat(x: Float) = unit(x)
  implicit def doubleToRepDouble(x: Double) = unit(x)
  implicit def longToRepLong(x: Long) = unit(x)
}

/**
 * This file is extremely boilerplate. In fact, most of the code here is copied from a
 * Forge-generated file. We need a static version since Delite (and other projects) depend
 * on it without using Forge.
 */
trait PurePrimitiveOps extends OverloadHack {
  this: ImplicitOps =>
  /**
   * Primitive conversions
   */

  implicit def repIntToRepDouble(x: Rep[Int]): Rep[Double] = x.toDouble
  implicit def repIntToRepFloat(x: Rep[Int]): Rep[Float] = x.toFloat
  implicit def repIntToRepLong(x: Rep[Int]): Rep[Long] = x.toLong
  implicit def repFloatToRepDouble(x: Rep[Float]): Rep[Double] = x.toDouble
  implicit def repLongToRepFloat(x: Rep[Long]): Rep[Float] = x.toFloat
  implicit def repLongToRepDouble(x: Rep[Long]): Rep[Double] = x.toDouble

  /**
   * Enumerate all combinations of primitive math.
   * Avoids certain fragile behavior, including compiler crashes and some erroneous or inaccessible type errors.
   */


  // -- BEGIN FORGE-GENERATED SECTION

  implicit def repToPrimitiveMathOpsDoubleOpsCls(x: Rep[Double])(implicit __pos: SourceContext) = new PrimitiveMathOpsDoubleOpsCls(x)(__pos)
  implicit def liftToPrimitiveMathOpsDoubleOpsCls(x: Double)(implicit __pos: SourceContext) = new PrimitiveMathOpsDoubleOpsCls(unit(x))(__pos)


  class PrimitiveMathOpsDoubleOpsCls(val self: Rep[Double])(implicit __pos: SourceContext) {
    def +(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded9) = { double_plus(self, unit(rhs)) }
    def -(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded9) = { double_minus(self, unit(rhs)) }
    def *(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded9) = { double_times(self, unit(rhs)) }
    def /(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded10) = { double_divide(self, unit(rhs)) }
    def +(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded10) = { double_plus(self, rhs) }
    def -(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded10) = { double_minus(self, rhs) }
    def *(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded10) = { double_times(self, rhs) }
    def /(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded11) = { double_divide(self, rhs) }
    /*def +(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded11) = { double_plus(self, readVar(rhs)) }
    def -(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded11) = { double_minus(self, readVar(rhs)) }
    def *(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded11) = { double_times(self, readVar(rhs)) }
    def /(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded12) = { double_divide(self, readVar(rhs)) }*/
    def +(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded12) = { double_plus(self, unit(rhs.toDouble)) }
    def -(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded12) = { double_minus(self, unit(rhs.toDouble)) }
    def *(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded12) = { double_times(self, unit(rhs.toDouble)) }
    def /(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded13) = { double_divide(self, unit(rhs.toDouble)) }
    def +(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded13) = { double_plus(self, rhs.toDouble) }
    def -(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded13) = { double_minus(self, rhs.toDouble) }
    def *(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded13) = { double_times(self, rhs.toDouble) }
    def /(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded14) = { double_divide(self, rhs.toDouble) }
    /*def +(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded14) = { double_plus(self, readVar(rhs).toDouble) }
    def -(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded14) = { double_minus(self, readVar(rhs).toDouble) }
    def *(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded14) = { double_times(self, readVar(rhs).toDouble) }
    def /(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded15) = { double_divide(self, readVar(rhs).toDouble) }*/
    def +(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded15) = { double_plus(self, unit(rhs.toDouble)) }
    def -(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded15) = { double_minus(self, unit(rhs.toDouble)) }
    def *(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded15) = { double_times(self, unit(rhs.toDouble)) }
    def /(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded16) = { double_divide(self, unit(rhs.toDouble)) }
    def +(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded16) = { double_plus(self, rhs.toDouble) }
    def -(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded16) = { double_minus(self, rhs.toDouble) }
    def *(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded16) = { double_times(self, rhs.toDouble) }
    def /(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded17) = { double_divide(self, rhs.toDouble) }
    /*def +(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded17) = { double_plus(self, readVar(rhs).toDouble) }
    def -(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded17) = { double_minus(self, readVar(rhs).toDouble) }
    def *(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded17) = { double_times(self, readVar(rhs).toDouble) }
    def /(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded18) = { double_divide(self, readVar(rhs).toDouble) }*/
    def +(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded18) = { double_plus(self, unit(rhs.toDouble)) }
    def -(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded18) = { double_minus(self, unit(rhs.toDouble)) }
    def *(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded18) = { double_times(self, unit(rhs.toDouble)) }
    def /(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded19) = { double_divide(self, unit(rhs.toDouble)) }
    def +(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded19) = { double_plus(self, rhs.toDouble) }
    def -(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded19) = { double_minus(self, rhs.toDouble) }
    def *(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded19) = { double_times(self, rhs.toDouble) }
    def /(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded20) = { double_divide(self, rhs.toDouble) }
    /*def +(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded20) = { double_plus(self, readVar(rhs).toDouble) }
    def -(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded20) = { double_minus(self, readVar(rhs).toDouble) }
    def *(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded20) = { double_times(self, readVar(rhs).toDouble) }
    def /(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded21) = { double_divide(self, readVar(rhs).toDouble) }*/
  }

  implicit def repToPrimitiveMathOpsFloatOpsCls(x: Rep[Float])(implicit __pos: SourceContext) = new PrimitiveMathOpsFloatOpsCls(x)(__pos)
  implicit def liftToPrimitiveMathOpsFloatOpsCls(x: Float)(implicit __pos: SourceContext) = new PrimitiveMathOpsFloatOpsCls(unit(x))(__pos)


  class PrimitiveMathOpsFloatOpsCls(val self: Rep[Float])(implicit __pos: SourceContext) {
    def +(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded41) = { double_plus(self.toDouble, unit(rhs)) }
    def -(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded41) = { double_minus(self.toDouble, unit(rhs)) }
    def *(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded41) = { double_times(self.toDouble, unit(rhs)) }
    def /(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded42) = { double_divide(self.toDouble, unit(rhs)) }
    def +(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded42) = { double_plus(self.toDouble, rhs) }
    def -(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded42) = { double_minus(self.toDouble, rhs) }
    def *(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded42) = { double_times(self.toDouble, rhs) }
    def /(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded43) = { double_divide(self.toDouble, rhs) }
/*    def +(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded43) = { double_plus(self.toDouble, readVar(rhs)) }
    def -(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded43) = { double_minus(self.toDouble, readVar(rhs)) }
    def *(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded43) = { double_times(self.toDouble, readVar(rhs)) }
    def /(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded44) = { double_divide(self.toDouble, readVar(rhs)) }
    def +(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded44) = { float_plus(self, unit(rhs)) }*/
    def -(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded44) = { float_minus(self, unit(rhs)) }
    def *(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded44) = { float_times(self, unit(rhs)) }
    def /(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded45) = { float_divide(self, unit(rhs)) }
    def +(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded45) = { float_plus(self, rhs) }
    def -(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded45) = { float_minus(self, rhs) }
    def *(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded45) = { float_times(self, rhs) }
    def /(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded46) = { float_divide(self, rhs) }
    /*def +(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded46) = { float_plus(self, readVar(rhs)) }
    def -(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded46) = { float_minus(self, readVar(rhs)) }
    def *(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded46) = { float_times(self, readVar(rhs)) }
    def /(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded47) = { float_divide(self, readVar(rhs)) }*/
    def +(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded47) = { float_plus(self, unit(rhs.toFloat)) }
    def -(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded47) = { float_minus(self, unit(rhs.toFloat)) }
    def *(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded47) = { float_times(self, unit(rhs.toFloat)) }
    def /(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded48) = { float_divide(self, unit(rhs.toFloat)) }
    def +(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded48) = { float_plus(self, rhs.toFloat) }
    def -(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded48) = { float_minus(self, rhs.toFloat) }
    def *(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded48) = { float_times(self, rhs.toFloat) }
    def /(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded49) = { float_divide(self, rhs.toFloat) }
/*    def +(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded49) = { float_plus(self, readVar(rhs).toFloat) }
    def -(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded49) = { float_minus(self, readVar(rhs).toFloat) }
    def *(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded49) = { float_times(self, readVar(rhs).toFloat) }
    def /(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded50) = { float_divide(self, readVar(rhs).toFloat) }*/
    def +(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded50) = { float_plus(self, unit(rhs.toFloat)) }
    def -(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded50) = { float_minus(self, unit(rhs.toFloat)) }
    def *(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded50) = { float_times(self, unit(rhs.toFloat)) }
    def /(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded51) = { float_divide(self, unit(rhs.toFloat)) }
    def +(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded51) = { float_plus(self, rhs.toFloat) }
    def -(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded51) = { float_minus(self, rhs.toFloat) }
    def *(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded51) = { float_times(self, rhs.toFloat) }
    def /(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded52) = { float_divide(self, rhs.toFloat) }
/*    def +(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded52) = { float_plus(self, readVar(rhs).toFloat) }
    def -(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded52) = { float_minus(self, readVar(rhs).toFloat) }
    def *(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded52) = { float_times(self, readVar(rhs).toFloat) }
    def /(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded53) = { float_divide(self, readVar(rhs).toFloat) }*/
  }

  implicit def repToPrimitiveMathOpsIntOpsCls(x: Rep[Int])(implicit __pos: SourceContext) = new PrimitiveMathOpsIntOpsCls(x)(__pos)
  implicit def liftToPrimitiveMathOpsIntOpsCls(x: Int)(implicit __pos: SourceContext) = new PrimitiveMathOpsIntOpsCls(unit(x))(__pos)


  class PrimitiveMathOpsIntOpsCls(val self: Rep[Int])(implicit __pos: SourceContext) {
    def +(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded73) = { double_plus(self.toDouble, unit(rhs)) }
    def -(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded73) = { double_minus(self.toDouble, unit(rhs)) }
    def *(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded73) = { double_times(self.toDouble, unit(rhs)) }
    def /(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded74) = { double_divide(self.toDouble, unit(rhs)) }
    def +(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded74) = { double_plus(self.toDouble, rhs) }
    def -(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded74) = { double_minus(self.toDouble, rhs) }
    def *(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded74) = { double_times(self.toDouble, rhs) }
    def /(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded75) = { double_divide(self.toDouble, rhs) }
    /*def +(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded75) = { double_plus(self.toDouble, readVar(rhs)) }
    def -(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded75) = { double_minus(self.toDouble, readVar(rhs)) }
    def *(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded75) = { double_times(self.toDouble, readVar(rhs)) }
    def /(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded76) = { double_divide(self.toDouble, readVar(rhs)) }*/
    def +(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded76) = { float_plus(self.toFloat, unit(rhs)) }
    def -(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded76) = { float_minus(self.toFloat, unit(rhs)) }
    def *(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded76) = { float_times(self.toFloat, unit(rhs)) }
    def /(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded77) = { float_divide(self.toFloat, unit(rhs)) }
    def +(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded77) = { float_plus(self.toFloat, rhs) }
    def -(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded77) = { float_minus(self.toFloat, rhs) }
    def *(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded77) = { float_times(self.toFloat, rhs) }
    def /(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded78) = { float_divide(self.toFloat, rhs) }
    /*def +(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded78) = { float_plus(self.toFloat, readVar(rhs)) }
    def -(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded78) = { float_minus(self.toFloat, readVar(rhs)) }
    def *(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded78) = { float_times(self.toFloat, readVar(rhs)) }
    def /(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded79) = { float_divide(self.toFloat, readVar(rhs)) }*/
    def +(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded79) = { int_plus(self, unit(rhs)) }
    def -(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded79) = { int_minus(self, unit(rhs)) }
    def *(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded79) = { int_times(self, unit(rhs)) }
    def /(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded80) = { int_divide(self, unit(rhs)) }
    def +(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded80) = { int_plus(self, rhs) }
    def -(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded80) = { int_minus(self, rhs) }
    def *(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded80) = { int_times(self, rhs) }
    def /(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded81) = { int_divide(self, rhs) }
    /*def +(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded81) = { int_plus(self, readVar(rhs)) }
    def -(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded81) = { int_minus(self, readVar(rhs)) }
    def *(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded81) = { int_times(self, readVar(rhs)) }
    def /(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded82) = { int_divide(self, readVar(rhs)) }*/
    def +(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded82) = { long_plus(self.toLong, unit(rhs)) }
    def -(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded82) = { long_minus(self.toLong, unit(rhs)) }
    def *(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded82) = { long_times(self.toLong, unit(rhs)) }
    def /(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded83) = { long_divide(self.toLong, unit(rhs)) }
    def +(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded83) = { long_plus(self.toLong, rhs) }
    def -(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded83) = { long_minus(self.toLong, rhs) }
    def *(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded83) = { long_times(self.toLong, rhs) }
    def /(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded84) = { long_divide(self.toLong, rhs) }
    /*def +(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded84) = { long_plus(self.toLong, readVar(rhs)) }
    def -(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded84) = { long_minus(self.toLong, readVar(rhs)) }
    def *(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded84) = { long_times(self.toLong, readVar(rhs)) }
    def /(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded85) = { long_divide(self.toLong, readVar(rhs)) }*/
  }

  implicit def repToPrimitiveMathOpsLongOpsCls(x: Rep[Long])(implicit __pos: SourceContext) = new PrimitiveMathOpsLongOpsCls(x)(__pos)
  implicit def liftToPrimitiveMathOpsLongOpsCls(x: Long)(implicit __pos: SourceContext) = new PrimitiveMathOpsLongOpsCls(unit(x))(__pos)


  class PrimitiveMathOpsLongOpsCls(val self: Rep[Long])(implicit __pos: SourceContext) {
    def +(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded105) = { double_plus(self.toDouble, unit(rhs)) }
    def -(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded105) = { double_minus(self.toDouble, unit(rhs)) }
    def *(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded105) = { double_times(self.toDouble, unit(rhs)) }
    def /(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded106) = { double_divide(self.toDouble, unit(rhs)) }
    def +(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded106) = { double_plus(self.toDouble, rhs) }
    def -(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded106) = { double_minus(self.toDouble, rhs) }
    def *(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded106) = { double_times(self.toDouble, rhs) }
    def /(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded107) = { double_divide(self.toDouble, rhs) }
/*    def +(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded107) = { double_plus(self.toDouble, readVar(rhs)) }
    def -(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded107) = { double_minus(self.toDouble, readVar(rhs)) }
    def *(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded107) = { double_times(self.toDouble, readVar(rhs)) }
    def /(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded108) = { double_divide(self.toDouble, readVar(rhs)) }*/
    def +(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded108) = { float_plus(self.toFloat, unit(rhs)) }
    def -(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded108) = { float_minus(self.toFloat, unit(rhs)) }
    def *(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded108) = { float_times(self.toFloat, unit(rhs)) }
    def /(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded109) = { float_divide(self.toFloat, unit(rhs)) }
    def +(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded109) = { float_plus(self.toFloat, rhs) }
    def -(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded109) = { float_minus(self.toFloat, rhs) }
    def *(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded109) = { float_times(self.toFloat, rhs) }
    def /(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded110) = { float_divide(self.toFloat, rhs) }
    /*def +(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded110) = { float_plus(self.toFloat, readVar(rhs)) }
    def -(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded110) = { float_minus(self.toFloat, readVar(rhs)) }
    def *(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded110) = { float_times(self.toFloat, readVar(rhs)) }
    def /(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded111) = { float_divide(self.toFloat, readVar(rhs)) }*/
    def +(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded111) = { long_plus(self, unit(rhs.toLong)) }
    def -(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded111) = { long_minus(self, unit(rhs.toLong)) }
    def *(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded111) = { long_times(self, unit(rhs.toLong)) }
    def /(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded112) = { long_divide(self, unit(rhs.toLong)) }
    def +(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded112) = { long_plus(self, rhs.toLong) }
    def -(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded112) = { long_minus(self, rhs.toLong) }
    def *(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded112) = { long_times(self, rhs.toLong) }
    def /(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded113) = { long_divide(self, rhs.toLong) }
    /*def +(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded113) = { long_plus(self, readVar(rhs).toLong) }
    def -(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded113) = { long_minus(self, readVar(rhs).toLong) }
    def *(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded113) = { long_times(self, readVar(rhs).toLong) }
    def /(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded114) = { long_divide(self, readVar(rhs).toLong) }*/
    def +(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded114) = { long_plus(self, unit(rhs)) }
    def -(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded114) = { long_minus(self, unit(rhs)) }
    def *(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded114) = { long_times(self, unit(rhs)) }
    def /(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded115) = { long_divide(self, unit(rhs)) }
    def +(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded115) = { long_plus(self, rhs) }
    def -(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded115) = { long_minus(self, rhs) }
    def *(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded115) = { long_times(self, rhs) }
    def /(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded116) = { long_divide(self, rhs) }
    /*def +(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded116) = { long_plus(self, readVar(rhs)) }
    def -(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded116) = { long_minus(self, readVar(rhs)) }
    def *(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded116) = { long_times(self, readVar(rhs)) }
    def /(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded117) = { long_divide(self, readVar(rhs)) }*/
  }


  // -- END FORGE-GENERATED SECTION


  /**
   *  Double
   */

  object Double {
    def PositiveInfinity(implicit pos: SourceContext) = obj_double_positive_infinity
    def NegativeInfinity(implicit pos: SourceContext) = obj_double_negative_infinity
    def MinValue(implicit pos: SourceContext) = obj_double_min_value
    def MaxValue(implicit pos: SourceContext) = obj_double_max_value
  }

  implicit def doubleToDoubleOps(n: Double): DoubleOpsCls = new DoubleOpsCls(unit(n))
  implicit def repDoubleToDoubleOps(n: Rep[Double]): DoubleOpsCls = new DoubleOpsCls(n)
  //implicit def varDoubleToDoubleOps(n: Var[Double]): DoubleOpsCls = new DoubleOpsCls(readVar(n))

  class DoubleOpsCls(self: Rep[Double]) {
    // def +(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded21) = { double_plus(self,unit(__arg1.toDouble)) }
    // def +(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded22) = { double_plus(self,unit(__arg1.toDouble)) }
    // def +(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded23) = { double_plus(self,unit(__arg1)) }
    // def +(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded24) = { double_plus(self,unit(__arg1.toDouble)) }
    // def +(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded30) = { double_plus(self,__arg1.toDouble) }
    // def +(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded31) = { double_plus(self,__arg1.toDouble) }
    // def +(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded32) = { double_plus(self,__arg1) }
    // def +(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded33) = { double_plus(self,__arg1.toDouble) }
    // def +(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded40) = { double_plus(self,readVar(__arg1).toDouble) }
    // def +(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded41) = { double_plus(self,readVar(__arg1).toDouble) }
    // def +(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded42) = { double_plus(self,readVar(__arg1)) }
    // def +(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded43) = { double_plus(self,readVar(__arg1).toDouble) }
    // def -(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded21) = { double_minus(self,unit(__arg1.toDouble)) }
    // def -(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded22) = { double_minus(self,unit(__arg1.toDouble)) }
    // def -(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded23) = { double_minus(self,unit(__arg1)) }
    // def -(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded24) = { double_minus(self,unit(__arg1.toDouble)) }
    // def -(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded30) = { double_minus(self,__arg1.toDouble) }
    // def -(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded31) = { double_minus(self,__arg1.toDouble) }
    // def -(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded32) = { double_minus(self,__arg1) }
    // def -(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded33) = { double_minus(self,__arg1.toDouble) }
    // def -(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded40) = { double_minus(self,readVar(__arg1).toDouble) }
    // def -(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded41) = { double_minus(self,readVar(__arg1).toDouble) }
    // def -(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded42) = { double_minus(self,readVar(__arg1)) }
    // def -(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded43) = { double_minus(self,readVar(__arg1).toDouble) }
    // def *(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded22) = { double_times(self,unit(__arg1.toDouble)) }
    // def *(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded23) = { double_times(self,unit(__arg1.toDouble)) }
    // def *(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded24) = { double_times(self,unit(__arg1)) }
    // def *(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded25) = { double_times(self,unit(__arg1.toDouble)) }
    // def *(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded31) = { double_times(self,__arg1.toDouble) }
    // def *(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded32) = { double_times(self,__arg1.toDouble) }
    // def *(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded33) = { double_times(self,__arg1) }
    // def *(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded34) = { double_times(self,__arg1.toDouble) }
    // def *(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded40) = { double_times(self,readVar(__arg1).toDouble) }
    // def *(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded41) = { double_times(self,readVar(__arg1).toDouble) }
    // def *(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded42) = { double_times(self,readVar(__arg1)) }
    // def *(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded43) = { double_times(self,readVar(__arg1).toDouble) }
    // def /(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded21) = { double_divide(self,unit(__arg1.toDouble)) }
    // def /(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded22) = { double_divide(self,unit(__arg1.toDouble)) }
    // def /(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded23) = { double_divide(self,unit(__arg1)) }
    // def /(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded24) = { double_divide(self,unit(__arg1.toDouble)) }
    // def /(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded30) = { double_divide(self,__arg1.toDouble) }
    // def /(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded31) = { double_divide(self,__arg1.toDouble) }
    // def /(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded32) = { double_divide(self,__arg1) }
    // def /(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded33) = { double_divide(self,__arg1.toDouble) }
    // def /(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded40) = { double_divide(self,readVar(__arg1).toDouble) }
    // def /(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded41) = { double_divide(self,readVar(__arg1).toDouble) }
    // def /(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded42) = { double_divide(self,readVar(__arg1)) }
    // def /(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded43) = { double_divide(self,readVar(__arg1).toDouble) }
    def toInt(implicit pos: SourceContext) = double_to_int(self)
    def toFloat(implicit pos: SourceContext) = double_to_float(self)
    def toLong(implicit pos: SourceContext) = double_to_long(self)
  }

  def obj_double_positive_infinity(implicit pos: SourceContext): Rep[Double]
  def obj_double_negative_infinity(implicit pos: SourceContext): Rep[Double]
  def obj_double_min_value(implicit pos: SourceContext): Rep[Double]
  def obj_double_max_value(implicit pos: SourceContext): Rep[Double]
  def double_plus(lhs: Rep[Double], rhs: Rep[Double])(implicit pos: SourceContext): Rep[Double]
  def double_minus(lhs: Rep[Double], rhs: Rep[Double])(implicit pos: SourceContext): Rep[Double]
  def double_times(lhs: Rep[Double], rhs: Rep[Double])(implicit pos: SourceContext): Rep[Double]
  def double_divide(lhs: Rep[Double], rhs: Rep[Double])(implicit pos: SourceContext): Rep[Double]
  def double_to_int(lhs: Rep[Double])(implicit pos: SourceContext): Rep[Int]
  def double_to_float(lhs: Rep[Double])(implicit pos: SourceContext): Rep[Float]
  def double_to_long(lhs: Rep[Double])(implicit pos: SourceContext): Rep[Long]


  /**
   * Float
   */

  implicit def repToFloatOpsCls(x: Rep[Float])(implicit pos: SourceContext) = new FloatOpsCls(x)(pos)
  implicit def liftToFloatOpsCls(x: Float)(implicit pos: SourceContext) = new FloatOpsCls(unit(x))(pos)
  //implicit def varToFloatOpsCls(x: Var[Float])(implicit pos: SourceContext) = new FloatOpsCls(readVar(x))(pos)

  class FloatOpsCls(val self: Rep[Float])(implicit pos: SourceContext) {
    // def +(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded18) = { float_plus(self,unit(__arg1.toFloat)) }
    // def +(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded19) = { float_plus(self,unit(__arg1)) }
    // def +(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded20) = { double_plus(self.toDouble,unit(__arg1)) }
    // def +(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded21) = { float_plus(self,unit(__arg1.toFloat)) }
    // def +(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded27) = { float_plus(self,__arg1.toFloat) }
    // def +(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded28) = { float_plus(self,__arg1) }
    // def +(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded29) = { double_plus(self.toDouble,__arg1) }
    // def +(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded30) = { float_plus(self,__arg1.toFloat) }
    // def +(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded37) = { float_plus(self,readVar(__arg1).toFloat) }
    // def +(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded38) = { float_plus(self,readVar(__arg1)) }
    // def +(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded39) = { double_plus(self.toDouble,readVar(__arg1)) }
    // def +(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded40) = { float_plus(self,readVar(__arg1).toFloat) }
    // def -(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded18) = { float_minus(self,unit(__arg1.toFloat)) }
    // def -(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded19) = { float_minus(self,unit(__arg1)) }
    // def -(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded20) = { double_minus(self.toDouble,unit(__arg1)) }
    // def -(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded21) = { float_minus(self,unit(__arg1.toFloat)) }
    // def -(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded27) = { float_minus(self,__arg1.toFloat) }
    // def -(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded28) = { float_minus(self,__arg1) }
    // def -(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded29) = { double_minus(self.toDouble,__arg1) }
    // def -(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded30) = { float_minus(self,__arg1.toFloat) }
    // def -(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded37) = { float_minus(self,readVar(__arg1).toFloat) }
    // def -(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded38) = { float_minus(self,readVar(__arg1)) }
    // def -(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded39) = { double_minus(self.toDouble,readVar(__arg1)) }
    // def -(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded40) = { float_minus(self,readVar(__arg1).toFloat) }
    // def *(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded19) = { float_times(self,unit(__arg1.toFloat)) }
    // def *(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded20) = { float_times(self,unit(__arg1)) }
    // def *(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded21) = { double_times(self.toDouble,unit(__arg1)) }
    // def *(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded22) = { float_times(self,unit(__arg1.toFloat)) }
    // def *(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded28) = { float_times(self,__arg1.toFloat) }
    // def *(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded29) = { float_times(self,__arg1) }
    // def *(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded30) = { double_times(self.toDouble,__arg1) }
    // def *(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded31) = { float_times(self,__arg1.toFloat) }
    // def *(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded37) = { float_times(self,readVar(__arg1).toFloat) }
    // def *(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded38) = { float_times(self,readVar(__arg1)) }
    // def *(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded39) = { double_times(self.toDouble,readVar(__arg1)) }
    // def *(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded40) = { float_times(self,readVar(__arg1).toFloat) }
    // def /(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded18) = { float_divide(self,unit(__arg1.toFloat)) }
    // def /(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded19) = { float_divide(self,unit(__arg1)) }
    // def /(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded20) = { double_divide(self.toDouble,unit(__arg1)) }
    // def /(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded21) = { float_divide(self,unit(__arg1.toFloat)) }
    // def /(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded27) = { float_divide(self,__arg1.toFloat) }
    // def /(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded28) = { float_divide(self,__arg1) }
    // def /(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded29) = { double_divide(self.toDouble,__arg1) }
    // def /(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded30) = { float_divide(self,__arg1.toFloat) }
    // def /(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded37) = { float_divide(self,readVar(__arg1).toFloat) }
    // def /(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded38) = { float_divide(self,readVar(__arg1)) }
    // def /(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded39) = { double_divide(self.toDouble,readVar(__arg1)) }
    // def /(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded40) = { float_divide(self,readVar(__arg1).toFloat) }
    def toInt(implicit pos: SourceContext) = float_to_int(self)
    def toDouble(implicit pos: SourceContext) = float_to_double(self)
  }

  def float_plus(lhs: Rep[Float], rhs: Rep[Float])(implicit pos: SourceContext): Rep[Float]
  def float_minus(lhs: Rep[Float], rhs: Rep[Float])(implicit pos: SourceContext): Rep[Float]
  def float_times(lhs: Rep[Float], rhs: Rep[Float])(implicit pos: SourceContext): Rep[Float]
  def float_divide(lhs: Rep[Float], rhs: Rep[Float])(implicit pos: SourceContext): Rep[Float]
  def float_to_int(lhs: Rep[Float])(implicit pos: SourceContext): Rep[Int]
  def float_to_double(lhs: Rep[Float])(implicit pos: SourceContext): Rep[Double]


  /**
   * Int
   */

  object Int {
    def MaxValue(implicit pos: SourceContext) = obj_int_max_value
    def MinValue(implicit pos: SourceContext) = obj_int_min_value
  }

  implicit def intToIntOps(n: Int): IntOpsCls = new IntOpsCls(unit(n))
  implicit def repIntToIntOps(n: Rep[Int]): IntOpsCls = new IntOpsCls(n)
  //implicit def varIntToIntOps(n: Var[Int]): IntOpsCls = new IntOpsCls(readVar(n))

  class IntOpsCls(self: Rep[Int])(implicit pos: SourceContext) {
    // def +(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded15) = { int_plus(self,unit(__arg1)) }
    // def +(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded16) = { float_plus(self.toFloat,unit(__arg1)) }
    // def +(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded17) = { double_plus(self.toDouble,unit(__arg1)) }
    // def +(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded18) = { long_plus(self.toLong,unit(__arg1)) }
    // def +(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded24) = { int_plus(self,__arg1) }
    // def +(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded25) = { float_plus(self.toFloat,__arg1) }
    // def +(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded26) = { double_plus(self.toDouble,__arg1) }
    // def +(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded27) = { long_plus(self.toLong,__arg1) }
    // def +(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded33) = { int_plus(self,readVar(__arg1)) }
    // def +(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded34) = { float_plus(self.toFloat,readVar(__arg1)) }
    // def +(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded35) = { double_plus(self.toDouble,readVar(__arg1)) }
    // def +(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded36) = { long_plus(self.toLong,readVar(__arg1)) }
    // def -(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded15) = { int_minus(self,unit(__arg1)) }
    // def -(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded16) = { float_minus(self.toFloat,unit(__arg1)) }
    // def -(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded17) = { double_minus(self.toDouble,unit(__arg1)) }
    // def -(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded18) = { long_minus(self.toLong,unit(__arg1)) }
    // def -(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded24) = { int_minus(self,__arg1) }
    // def -(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded25) = { float_minus(self.toFloat,__arg1) }
    // def -(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded26) = { double_minus(self.toDouble,__arg1) }
    // def -(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded27) = { long_minus(self.toLong,__arg1) }
    // def -(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded33) = { int_minus(self,readVar(__arg1)) }
    // def -(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded34) = { float_minus(self.toFloat,readVar(__arg1)) }
    // def -(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded35) = { double_minus(self.toDouble,readVar(__arg1)) }
    // def -(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded36) = { long_minus(self.toLong,readVar(__arg1)) }
    // def *(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded16) = { int_times(self,unit(__arg1)) }
    // def *(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded17) = { float_times(self.toFloat,unit(__arg1)) }
    // def *(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded18) = { double_times(self.toDouble,unit(__arg1)) }
    // def *(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded19) = { long_times(self.toLong,unit(__arg1)) }
    // def *(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded25) = { int_times(self,__arg1) }
    // def *(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded26) = { float_times(self.toFloat,__arg1) }
    // def *(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded27) = { double_times(self.toDouble,__arg1) }
    // def *(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded28) = { long_times(self.toLong,__arg1) }
    // def *(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded33) = { int_times(self,readVar(__arg1)) }
    // def *(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded34) = { float_times(self.toFloat,readVar(__arg1)) }
    // def *(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded35) = { double_times(self.toDouble,readVar(__arg1)) }
    // def *(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded36) = { long_times(self.toLong,readVar(__arg1)) }
    // def /(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded15) = { int_divide(self,unit(__arg1)) }
    // def /(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded16) = { float_divide(self.toFloat,unit(__arg1)) }
    // def /(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded17) = { double_divide(self.toDouble,unit(__arg1)) }
    // def /(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded18) = { long_divide(self.toLong,unit(__arg1)) }
    // def /(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded24) = { int_divide(self,__arg1) }
    // def /(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded25) = { float_divide(self.toFloat,__arg1) }
    // def /(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded26) = { double_divide(self.toDouble,__arg1) }
    // def /(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded27) = { long_divide(self.toLong,__arg1) }
    // def /(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded33) = { int_divide(self,readVar(__arg1)) }
    // def /(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded34) = { float_divide(self.toFloat,readVar(__arg1)) }
    // def /(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded35) = { double_divide(self.toDouble,readVar(__arg1)) }
    // def /(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded36) = { long_divide(self.toLong,readVar(__arg1)) }
    def %(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded1) = int_mod(self, __arg1)
    def &(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded1) = int_binaryand(self, __arg1)
    def |(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded1) = int_binaryor(self, __arg1)
    def ^(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded1) = int_binaryxor(self, __arg1)
    def <<(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded1) = int_leftshift(self, __arg1)
    def >>(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded1) = int_rightshiftarith(self, __arg1)
    def >>>(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded1) = int_rightshiftlogical(self, __arg1)
    def unary_~(implicit pos: SourceContext) = int_bitwise_not(self)
    def toLong(implicit pos: SourceContext) = int_to_long(self)
    def toDouble(implicit pos: SourceContext) = int_to_double(self)
    def toFloat(implicit pos: SourceContext) = int_to_float(self)
  }

  def obj_int_max_value(implicit pos: SourceContext): Rep[Int]
  def obj_int_min_value(implicit pos: SourceContext): Rep[Int]
  def int_plus(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_minus(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_times(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_divide(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]

  def int_mod(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_binaryor(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_binaryand(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_binaryxor(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_bitwise_not(lhs: Rep[Int])(implicit pos: SourceContext) : Rep[Int]
  def int_to_long(lhs: Rep[Int])(implicit pos: SourceContext) : Rep[Long]
  def int_to_float(lhs: Rep[Int])(implicit pos: SourceContext) : Rep[Float]
  def int_to_double(lhs: Rep[Int])(implicit pos: SourceContext) : Rep[Double]
  def int_leftshift(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_rightshiftarith(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_rightshiftlogical(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]


  /**
   * Long
   */

  object Long {
    def MaxValue(implicit pos: SourceContext) = obj_long_max_value
    def MinValue(implicit pos: SourceContext) = obj_long_min_value
  }

  implicit def longToLongOps(n: Long): LongOpsCls = new LongOpsCls(unit(n))
  implicit def repLongToLongOps(n: Rep[Long]): LongOpsCls = new LongOpsCls(n)
  //implicit def varLongToLongOps(n: Var[Long]): LongOpsCls = new LongOpsCls(readVar(n))

  class LongOpsCls(self: Rep[Long])(implicit pos: SourceContext) {
    // def +(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded15) = { long_plus(self,unit(__arg1.toLong)) }
    // def +(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded16) = { float_plus(self.toFloat,unit(__arg1)) }
    // def +(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded17) = { double_plus(self.toDouble,unit(__arg1)) }
    // def +(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded18) = { long_plus(self,unit(__arg1)) }
    // def +(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded24) = { long_plus(self,__arg1.toLong) }
    // def +(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded25) = { float_plus(self.toFloat,__arg1) }
    // def +(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded26) = { double_plus(self.toDouble,__arg1) }
    // def +(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded27) = { long_plus(self,__arg1) }
    // def +(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded30) = { long_plus(self,readVar(__arg1).toLong) }
    // def +(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded31) = { float_plus(self.toFloat,readVar(__arg1)) }
    // def +(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded32) = { double_plus(self.toDouble,readVar(__arg1)) }
    // def +(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded33) = { long_plus(self,readVar(__arg1)) }
    // def -(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded15) = { long_minus(self,unit(__arg1.toLong)) }
    // def -(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded16) = { float_minus(self.toFloat,unit(__arg1)) }
    // def -(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded17) = { double_minus(self.toDouble,unit(__arg1)) }
    // def -(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded18) = { long_minus(self,unit(__arg1.toLong)) }
    // def -(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded24) = { long_minus(self,__arg1) }
    // def -(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded25) = { float_minus(self.toFloat,__arg1) }
    // def -(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded26) = { double_minus(self.toDouble,__arg1) }
    // def -(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded27) = { long_minus(self,__arg1) }
    // def -(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded30) = { long_minus(self,readVar(__arg1).toLong) }
    // def -(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded31) = { float_minus(self.toFloat,readVar(__arg1)) }
    // def -(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded32) = { double_minus(self.toDouble,readVar(__arg1)) }
    // def -(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded33) = { long_minus(self,readVar(__arg1)) }
    // def *(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded16) = { long_times(self,unit(__arg1.toLong)) }
    // def *(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded17) = { float_times(self.toFloat,unit(__arg1)) }
    // def *(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded18) = { double_times(self.toDouble,unit(__arg1)) }
    // def *(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded19) = { long_times(self,unit(__arg1.toLong)) }
    // def *(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded25) = { long_times(self,__arg1.toLong) }
    // def *(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded26) = { float_times(self.toFloat,__arg1) }
    // def *(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded27) = { double_times(self.toDouble,__arg1) }
    // def *(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded28) = { long_times(self,__arg1) }
    // def *(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded30) = { long_times(self,readVar(__arg1).toLong) }
    // def *(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded31) = { float_times(self.toFloat,readVar(__arg1)) }
    // def *(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded32) = { double_times(self.toDouble,readVar(__arg1)) }
    // def *(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded33) = { long_times(self,readVar(__arg1)) }
    // def /(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded15) = { long_divide(self,unit(__arg1.toLong)) }
    // def /(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded16) = { float_divide(self.toFloat,unit(__arg1)) }
    // def /(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded17) = { double_divide(self.toDouble,unit(__arg1)) }
    // def /(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded18) = { long_divide(self,unit(__arg1)) }
    // def /(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded24) = { long_divide(self,__arg1.toLong) }
    // def /(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded25) = { float_divide(self.toFloat,__arg1) }
    // def /(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded26) = { double_divide(self.toDouble,__arg1) }
    // def /(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded27) = { long_divide(self,__arg1) }
    // def /(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded30) = { long_divide(self,readVar(__arg1).toLong) }
    // def /(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded31) = { float_divide(self.toFloat,readVar(__arg1)) }
    // def /(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded32) = { double_divide(self.toDouble,readVar(__arg1)) }
    // def /(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded33) = { long_divide(self,readVar(__arg1)) }
    def %(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded1) = long_mod(self, __arg1)
    def &(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded1) = long_binaryand(self, __arg1)
    def |(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded1) = long_binaryor(self, __arg1)
    def ^(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded1) = long_binaryxor(self, __arg1)
    def <<(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded1) = long_shiftleft(self, __arg1)
    def >>(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded1) = long_shiftright_signed(self, __arg1)
    def >>>(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded1) = long_shiftright_unsigned(self, __arg1)
    def toInt(implicit pos: SourceContext) = long_to_int(self)
    def toDouble(implicit pos: SourceContext) = long_to_double(self)
    def toFloat(implicit pos: SourceContext) = long_to_float(self)
  }

  def long_plus(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long]
  def long_minus(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long]
  def long_times(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long]
  def long_divide(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long]

  def obj_long_max_value(implicit pos: SourceContext): Rep[Long]
  def obj_long_min_value(implicit pos: SourceContext): Rep[Long]
  def long_mod(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long]
  def long_binaryand(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long]
  def long_binaryor(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long]
  def long_binaryxor(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long]
  def long_shiftleft(lhs: Rep[Long], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Long]
  def long_shiftright_signed(lhs: Rep[Long], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Long]
  def long_shiftright_unsigned(lhs: Rep[Long], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Long]
  def long_to_int(lhs: Rep[Long])(implicit pos: SourceContext): Rep[Int]
  def long_to_float(lhs: Rep[Long])(implicit pos: SourceContext): Rep[Float]
  def long_to_double(lhs: Rep[Long])(implicit pos: SourceContext): Rep[Double]
}

trait PurePrimitiveOpsExp extends PurePrimitiveOps with BaseExp{
  this: ImplicitOpsExp =>

  /**
   * Double
   */

  case class ObjDoublePositiveInfinity() extends Def[Double]
  case class ObjDoubleNegativeInfinity() extends Def[Double]
  case class ObjDoubleMinValue() extends Def[Double]
  case class ObjDoubleMaxValue() extends Def[Double]
  case class DoubleToInt(lhs: Exp[Double]) extends Def[Int]
  case class DoubleToFloat(lhs: Exp[Double]) extends Def[Float]
  case class DoubleToLong(lhs: Exp[Double]) extends Def[Long]
  case class DoublePlus(lhs: Exp[Double], rhs: Exp[Double]) extends Def[Double]
  case class DoubleMinus(lhs: Exp[Double], rhs: Exp[Double]) extends Def[Double]
  case class DoubleTimes(lhs: Exp[Double], rhs: Exp[Double]) extends Def[Double]
  case class DoubleDivide(lhs: Exp[Double], rhs: Exp[Double]) extends Def[Double]

  def obj_double_positive_infinity(implicit pos: SourceContext) = {
    val x:Rep[Double] = ObjDoublePositiveInfinity()
    x
  }
  def obj_double_negative_infinity(implicit pos: SourceContext) = ObjDoubleNegativeInfinity()
  def obj_double_min_value(implicit pos: SourceContext) = ObjDoubleMinValue()
  def obj_double_max_value(implicit pos: SourceContext) = ObjDoubleMaxValue()
  def double_to_int(lhs: Exp[Double])(implicit pos: SourceContext) = DoubleToInt(lhs)
  def double_to_float(lhs: Exp[Double])(implicit pos: SourceContext) = DoubleToFloat(lhs)
  def double_to_long(lhs: Rep[Double])(implicit pos: SourceContext) = DoubleToLong(lhs)
  def double_plus(lhs: Exp[Double], rhs: Exp[Double])(implicit pos: SourceContext) : Exp[Double] = DoublePlus(lhs,rhs)
  def double_minus(lhs: Exp[Double], rhs: Exp[Double])(implicit pos: SourceContext) : Exp[Double] = DoubleMinus(lhs,rhs)
  def double_times(lhs: Exp[Double], rhs: Exp[Double])(implicit pos: SourceContext) : Exp[Double] = DoubleTimes(lhs,rhs)
  def double_divide(lhs: Exp[Double], rhs: Exp[Double])(implicit pos: SourceContext) : Exp[Double] = DoubleDivide(lhs,rhs)


  /**
   * Float
   */

  case class FloatToInt(lhs: Exp[Float]) extends Def[Int]
  case class FloatToDouble(lhs: Exp[Float]) extends Def[Double]
  case class FloatPlus(lhs: Exp[Float], rhs: Exp[Float]) extends Def[Float]
  case class FloatMinus(lhs: Exp[Float], rhs: Exp[Float]) extends Def[Float]
  case class FloatTimes(lhs: Exp[Float], rhs: Exp[Float]) extends Def[Float]
  case class FloatDivide(lhs: Exp[Float], rhs: Exp[Float]) extends Def[Float]

  def float_to_int(lhs: Exp[Float])(implicit pos: SourceContext) = FloatToInt(lhs)
  def float_to_double(lhs: Exp[Float])(implicit pos: SourceContext) = FloatToDouble(lhs)
  def float_plus(lhs: Exp[Float], rhs: Exp[Float])(implicit pos: SourceContext) : Exp[Float] = FloatPlus(lhs,rhs)
  def float_minus(lhs: Exp[Float], rhs: Exp[Float])(implicit pos: SourceContext) : Exp[Float] = FloatMinus(lhs,rhs)
  def float_times(lhs: Exp[Float], rhs: Exp[Float])(implicit pos: SourceContext) : Exp[Float] = FloatTimes(lhs,rhs)
  def float_divide(lhs: Exp[Float], rhs: Exp[Float])(implicit pos: SourceContext) : Exp[Float] = FloatDivide(lhs,rhs)


  /**
   * Int
   */

  case class ObjIntMaxValue() extends Def[Int]
  case class ObjIntMinValue() extends Def[Int]
  case class IntPlus(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntMinus(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntTimes(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntDivide(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntMod(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntBinaryOr(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntBinaryAnd(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntBinaryXor(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntShiftLeft(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntShiftRightArith(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntShiftRightLogical(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntBitwiseNot(lhs: Exp[Int]) extends Def[Int]
  case class IntToLong(lhs: Exp[Int]) extends Def[Long]
  case class IntToFloat(lhs: Exp[Int]) extends Def[Float]
  case class IntToDouble(lhs: Exp[Int]) extends Def[Double]

  def obj_int_max_value(implicit pos: SourceContext) = ObjIntMaxValue()
  def obj_int_min_value(implicit pos: SourceContext) = ObjIntMinValue()
  def int_plus(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext): Exp[Int] = IntPlus(lhs,rhs)
  def int_minus(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext): Exp[Int] = IntMinus(lhs, rhs)
  def int_times(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext): Exp[Int] = IntTimes(lhs, rhs)
  def int_divide(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) : Exp[Int] = IntDivide(lhs, rhs)
  def int_mod(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = IntMod(lhs, rhs)
  def int_binaryor(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = IntBinaryOr(lhs, rhs)
  def int_binaryand(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = IntBinaryAnd(lhs, rhs)
  def int_binaryxor(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = IntBinaryXor(lhs, rhs)
  def int_bitwise_not(lhs: Exp[Int])(implicit pos: SourceContext) = IntBitwiseNot(lhs)
  def int_to_long(lhs: Exp[Int])(implicit pos: SourceContext) = IntToLong(lhs)
  def int_to_float(lhs: Exp[Int])(implicit pos: SourceContext) = IntToFloat(lhs)
  def int_to_double(lhs: Exp[Int])(implicit pos: SourceContext) = IntToDouble(lhs)
  def int_leftshift(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = IntShiftLeft(lhs, rhs)
  def int_rightshiftarith(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = IntShiftRightArith(lhs, rhs)
  def int_rightshiftlogical(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = IntShiftRightLogical(lhs, rhs)


  /**
   * Long
   */

  case class ObjLongMaxValue() extends Def[Long]
  case class ObjLongMinValue() extends Def[Long]
  case class LongBinaryOr(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]
  case class LongBinaryAnd(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]
  case class LongBinaryXor(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]
  case class LongShiftLeft(lhs: Exp[Long], rhs: Exp[Int]) extends Def[Long]
  case class LongShiftRightSigned(lhs: Exp[Long], rhs: Exp[Int]) extends Def[Long]
  case class LongShiftRightUnsigned(lhs: Exp[Long], rhs: Exp[Int]) extends Def[Long]
  case class LongToInt(lhs: Exp[Long]) extends Def[Int]
  case class LongToFloat(lhs: Exp[Long]) extends Def[Float]
  case class LongToDouble(lhs: Exp[Long]) extends Def[Double]
  case class LongPlus(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]
  case class LongMinus(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]
  case class LongTimes(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]
  case class LongDivide(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]
  case class LongMod(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]

  def obj_long_max_value(implicit pos: SourceContext) = ObjLongMaxValue()
  def obj_long_min_value(implicit pos: SourceContext) = ObjLongMinValue()
  def long_mod(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext) = LongMod(lhs,rhs)
  def long_binaryor(lhs: Exp[Long], rhs: Exp[Long])(implicit pos: SourceContext) = LongBinaryOr(lhs,rhs)
  def long_binaryand(lhs: Exp[Long], rhs: Exp[Long])(implicit pos: SourceContext) = LongBinaryAnd(lhs,rhs)
  def long_binaryxor(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext) = LongBinaryXor(lhs,rhs)
  def long_shiftleft(lhs: Exp[Long], rhs: Exp[Int])(implicit pos: SourceContext) = LongShiftLeft(lhs,rhs)
  def long_shiftright_signed(lhs: Rep[Long], rhs: Rep[Int])(implicit pos: SourceContext) = LongShiftRightSigned(lhs,rhs)
  def long_shiftright_unsigned(lhs: Exp[Long], rhs: Exp[Int])(implicit pos: SourceContext) = LongShiftRightUnsigned(lhs,rhs)
  def long_to_int(lhs: Exp[Long])(implicit pos: SourceContext) = LongToInt(lhs)
  def long_to_float(lhs: Exp[Long])(implicit pos: SourceContext) = LongToFloat(lhs)
  def long_to_double(lhs: Exp[Long])(implicit pos: SourceContext) = LongToDouble(lhs)
  def long_plus(lhs: Exp[Long], rhs: Exp[Long])(implicit pos: SourceContext) : Exp[Long] = LongPlus(lhs, rhs)
  def long_minus(lhs: Exp[Long], rhs: Exp[Long])(implicit pos: SourceContext) : Exp[Long] = LongMinus(lhs, rhs)
  def long_times(lhs: Exp[Long], rhs: Exp[Long])(implicit pos: SourceContext) : Exp[Long] = LongTimes(lhs, rhs)
  def long_divide(lhs: Exp[Long], rhs: Exp[Long])(implicit pos: SourceContext) : Exp[Long] = LongDivide(lhs, rhs)


}


trait PurePrimitiveOpsExpOpt extends PurePrimitiveOpsExp with ImplicitOpsExp{
  override def int_plus(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) : Exp[Int] = (lhs,rhs) match {
    case (Const(a: Int),Const(b: Int)) => unit(a+b)
    case (Const(0),b) => b
    case (a,Const(0)) => a
    case _ => super.int_plus(lhs,rhs)
  }
  override def int_minus(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) : Exp[Int] = (lhs,rhs) match {
    case (Const(a: Int),Const(b: Int)) => unit(a-b)
    case (a,Const(0)) => a
    case (Def(IntPlus(llhs,lrhs)), rhs) if lrhs.equals(rhs) => llhs
    case _ => super.int_minus(lhs,rhs)
  }
  override def int_times(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) : Exp[Int] = (lhs,rhs) match {
    case (Const(a: Int),Const(b: Int)) => unit(a*b)
    case (Const(0),b) => Const(0)
    case (Const(1),b) => b
    case (a,Const(0)) => Const(0)
    case (a,Const(1)) => a
    case _ => super.int_times(lhs,rhs)
  }
  override def int_to_float(lhs: Rep[Int])(implicit pos: SourceContext): Rep[Float] = lhs match {
    case Const(x: Int) => Const(x.toFloat)
    case _ => super.int_to_float(lhs)
  }

  override def int_to_double(lhs: Rep[Int])(implicit pos: SourceContext): Rep[Double] = lhs match {
    case Const(x: Int) => Const(x.toDouble)
    case _ => super.int_to_double(lhs)
  }

  override def float_to_double(lhs: Rep[Float])(implicit pos: SourceContext): Rep[Double] = lhs match {
    case Const(x: Int) => Const(x.toDouble)
    case Def(IntToFloat(x)) => int_to_double(x)
    case _ => super.float_to_double(lhs)
  }

  override def double_to_int(lhs: Rep[Double])(implicit pos: SourceContext): Rep[Int] = lhs match {
    case Const(x: Int) => Const(x.toInt)
    case Def(IntToDouble(x)) => x
    case _ => super.double_to_int(lhs)
  }

  override def double_plus(lhs: Exp[Double], rhs: Exp[Double])(implicit pos: SourceContext) : Exp[Double] = (lhs, rhs) match {
    case (Const(0.0), r) => r
    case (l, Const(0.0)) => l
    case (Const(x: Double), Const(y: Double)) => Const(x+y)
    case _ => super.double_plus(lhs,rhs)
  }

  override def double_minus(lhs: Exp[Double], rhs: Exp[Double])(implicit pos: SourceContext) : Exp[Double] = (lhs, rhs) match {
    case (l, Const(0l)) => l
    case (Const(x: Double), Const(y: Double)) => Const(x-y)
    case _ => super.double_minus(lhs,rhs)
  }

  override def double_times(lhs: Exp[Double], rhs: Exp[Double])(implicit pos: SourceContext) : Exp[Double] = (lhs, rhs) match {
    case (l@Const(0.0), r) => l
    case (l, r@Const(0.0)) => r
    case (Const(1.0), r) => r
    case (l, Const(1.0)) => l
    case (Const(x: Double), Const(y: Double)) => Const(x*y)
    case _ => super.double_times(lhs,rhs)
  }

  override def long_plus(lhs: Exp[Long], rhs: Exp[Long])(implicit pos: SourceContext) : Exp[Long] = (lhs, rhs) match {
    case (Const(0l), r) => r
    case (l, Const(0l)) => l
    case (Const(x: Long), Const(y: Long)) => Const(x+y)
    case _ => super.long_plus(lhs,rhs)
  }

  override def long_minus(lhs: Exp[Long], rhs: Exp[Long])(implicit pos: SourceContext) : Exp[Long] = (lhs, rhs) match {
    case (l, Const(0l)) => l
    case (Const(x: Long), Const(y: Long)) => Const(x-y)
    case _ => super.long_minus(lhs,rhs)
  }

  override def long_times(lhs: Exp[Long], rhs: Exp[Long])(implicit pos: SourceContext) : Exp[Long] = (lhs, rhs) match {
    case (l@Const(0l), r) => l
    case (l, r@Const(0l)) => r
    case (Const(1l), r) => r
    case (l, Const(1l)) => l
    case (Const(x: Long), Const(y: Long)) => Const(x*y)
    case _ => super.long_times(lhs,rhs)
  }
}




