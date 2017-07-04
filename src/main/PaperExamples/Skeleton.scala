package PaperExamples


import org.scala_lang.virtualized.SourceContext

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._


trait Skeleton extends Sort_DSL {
  type NoRep[T] = T

  trait IRep[T[_]] extends RepBase[T] with Conditionals[T] with StagedNum[T] with Comparisons[T] with RangeFold[T] with ChooseStuff[T] with BooleanOps[T] {

    class Ops[A](lhs: T[A]) {
      def *(rhs: T[A]): T[A] = ???
      def +(rhs: T[A]): T[A] = ???
      def -(rhs: T[A]): T[A] = ???
      def %(rhs: T[A]): T[A] = ???
      def %(rhs: Int): T[A] = ???
      def *(rhs: Int): T[A] = ???
      def /(rhs: Int): T[A] = ???
      def <(rhs: Int): T[A] = ???
      def -(rhs: Int): T[A] = ???
      def toDouble(): T[Double] = ???
    }


    class DoubleOps[A](lhs: T[Double]) {
      def eq(rhs: T[Double]): T[Boolean] = ???
    }



  }

  object Implicits {
    implicit def toDoubleOps[R[_], T](x: R[Double])(implicit ev: IRep[R]): IRep[R]#DoubleOps[T] = new ev.DoubleOps(x)
    implicit def toOps[R[_], T](x: R[T])(implicit ev: IRep[R]): IRep[R]#Ops[T] = new ev.Ops(x)
    implicit def toVRayOps(x: Rep[Array[Double]]) = new VRayOps(x)
    implicit def InttoRange(x: Int) = new I2RangeOps(x)
    implicit def mktoRangeOps[T[_]: IRep](lhs: T[Int]): RangeFold[T]#toRangeOps = {
      val ev = implicitly[IRep[T]]
      new ev.toRangeOps(lhs)
    }
    implicit def mkRangeOps[T[_]: IRep](lhs: T[Range]): RangeFold[T]#RangeOps = {
      val ev = implicitly[IRep[T]]
      new ev.RangeOps(lhs)
    }
    implicit def toComparisionOps[T[_]: IRep, A: Ordering : Manifest](lhs: T[A]): Comparisons[T]#ComparisonOps[A] = {
      val ev = implicitly[IRep[T]]
      new ev.ComparisonOps(lhs)
    }
  }

  class I2RangeOps(lhs: Int){
    def until[R[_]: IRep](to: R[Int]): R[Range] = {
      val ev = implicitly[IRep[R]]
      ev.unt(ev.const(lhs),to)
    }
    def const[R[_]: IRep](to: R[Int]): R[Int] = {
      val ev = implicitly[IRep[R]]
      ev.const(lhs)
    }
  }
  class VRayOps(lhs: Rep[Array[Double]]){
    def length(): Rep[Int] = ???
    def splitAt(rhs: Rep[Int]): (Rep[Array[Double]], Rep[Array[Double]]) = ???
    def ++(rhs: Rep[Array[Double]]): Rep[Array[Double]] = ???
  }

  implicit object cRep extends IRep[Rep] with isRepBase with RepNum with RepConditionals with RepComparisons with RepRangeFold with RepChooseStuff with RepBooleanOps

  implicit object cNoRep extends IRep[NoRep] with noRepBase with NoRepNum with NoRepConditionals with NoRepComparisons with NoRepRangeFold with NoRepChooseStuff with NoRepBooleanOps


  trait RepBase[T[_]] {
    def isRep(): Boolean

    def const[A: TypeRep](x: A): T[A]

    def toRep[A: TypeRep](x: T[A]): Rep[A]

    def getRep[A](x: T[A]): Option[Rep[A]]

    def getNoRep[A](x: T[A]): Option[A]

    def fresh[A: TypeRep](): Vector[Rep[_]]

    def fetch[A: TypeRep](x: Vector[Rep[_]]): (Vector[Rep[_]], Option[T[A]])

    def sin(x: T[Double]): T[Double]
  }

  trait isRepBase extends RepBase[Rep] {
    val isRep = true

    def const[A: TypeRep](x: A): Rep[A] = Const(x)

    def toRep[A: TypeRep](x: Rep[A]): Rep[A] = x

    def getRep[A](x: Rep[A]): Some[Rep[A]] = Some(x)

    def getNoRep[A](x: Rep[A]): Option[A] = None

    def fresh[A: TypeRep](): Vector[Rep[_]] = Vector(Arg[A])

    def fetch[A: TypeRep](x: Vector[Rep[_]]): (Vector[Rep[_]], Some[Rep[A]]) = (x.tail, Some(x.head.asInstanceOf[Rep[A]]))

    def sin(x: Rep[Double]): Rep[Double] = sinus(x)

  }

  trait noRepBase extends RepBase[NoRep] {
    val isRep = false

    def const[A: TypeRep](x: A): NoRep[A] = x

    def toRep[A: TypeRep](x: NoRep[A]): Rep[A] = Const(x)

    def getRep[A](x: NoRep[A]): Option[Rep[A]] = None

    def getNoRep[A](x: NoRep[A]): Some[A] = Some(x)

    def fresh[A: TypeRep](): Vector[Rep[_]] = Vector.empty

    def fetch[A: TypeRep](x: Vector[Rep[_]]): (Vector[Rep[_]], Option[NoRep[A]]) = (x, None)

    def sin(x: NoRep[Double]): NoRep[Double] = Math.sin(x)

  }


  trait Conditionals[T[_]] {
    def _if[A](cond: T[Boolean], thenp: => A, elsep: => A)(implicit pos: SourceContext, branch: ExposeRep[A]): A
  }

  trait RepConditionals extends Conditionals[Rep] {
    def _if[A](cond: Rep[Boolean], thenp: => A, elsep: => A)(implicit pos: SourceContext, branch: ExposeRep[A]): A = myifThenElse(cond, thenp, elsep)
  }

  trait NoRepConditionals extends Conditionals[NoRep] {
    def _if[A](cond: NoRep[Boolean], thenp: => A, elsep: => A)(implicit pos: SourceContext, branch: ExposeRep[A]): A = if (cond) thenp else elsep
  }

  trait RangeFold[T[_]] {

    class toRangeOps(lhs: T[Int]) {
      def until(rhs: T[Int]): T[Range] = unt(lhs, rhs)
    }
    class RangeOps(lhs: T[Range]){
      def foldLeftx[B](ini: B)(body: (B, T[Int]) => B)(implicit exposeRep: ExposeRep[B]): B = rangefold_a2(lhs, ini, exposeRep)(body)
      def foldLeft[B](ini: B)(body: ((B, T[Int])) => B)(implicit exposeRep: ExposeRep[B]): B = rangefold(lhs, ini, exposeRep)(body)
      def map[B](body: (T[Int] => T[B]))(implicit exposeRep: ExposeRep[T[B]]): T[Array[B]] = rangemap(lhs,exposeRep, body)
    }


    def rangefold_a2[B](range: T[Range], ini: B, exposeRep: ExposeRep[B])(body: (B, T[Int]) => B): B = {
      def helper(inf: (B, T[Int]) => B): (((B, T[Int])) => B) = {
        ???
      }
      rangefold(range, ini, exposeRep)(helper(body))
    }
    def rangemap[B](range: T[Range], exposeRep: ExposeRep[T[B]],body: (T[Int] => T[B])): T[Array[B]]  = ???
    def unt(from: T[Int], to: T[Int]): T[Range]
    def rangefold[B](range: T[Range], ini: B, exposeRep: ExposeRep[B])(body: ((B, T[Int])) => B): B

  }

  trait RepRangeFold extends RangeFold[Rep] {
    def unt(from: Rep[Int], to: Rep[Int]): Rep[Range] = range_create(from, to)

    //def map[B](range: Rep[Range], exposeRep: ExposeRep[B])(body: (Rep[Int] => B)): B

    def rangefold[B](range: Rep[Range], ini: B, exposeRep: ExposeRep[B])(body: ((B, Rep[Int])) => B): B = range_foldLeft(range, ini, body)(exposeRep)


  }

  trait NoRepRangeFold extends RangeFold[NoRep] {
    def unt(from: NoRep[Int], to: NoRep[Int]): NoRep[Range] = Range(from, to)

    def rangefold[B](range: NoRep[Range], ini: B, exposeRep: ExposeRep[B])(body: ((B, NoRep[Int])) => B): B = {
      val f: (B, Int) => B = (b: B, i: Int) => body((b, i))
      range.foldLeft(ini)(f)
    }
  }


  trait BooleanOps[T[_]] {
    implicit def mkBooleanOps(lhs: T[Boolean]): Ops = new Ops(lhs)

    class Ops(lhs: T[Boolean]) {
      def ||(rhs: T[Boolean]): T[Boolean] = or(lhs, rhs)
    }

    def or(lhs: T[Boolean], rhs: T[Boolean]): T[Boolean]
  }

  trait RepBooleanOps extends BooleanOps[Rep] {
    def or(lhs: Rep[Boolean], rhs: Rep[Boolean]): Rep[Boolean] = boolean_or(lhs, rhs)
  }

  trait NoRepBooleanOps extends BooleanOps[NoRep] {
    def or(lhs: NoRep[Boolean], rhs: NoRep[Boolean]): NoRep[Boolean] = lhs || rhs
  }


  trait Comparisons[T[_]] {


    class ComparisonOps[A: Ordering : Manifest](lhs: T[A]) {
      def eq(rhs: T[A]): T[Boolean] = equiv(lhs, rhs)

      def <(rhs: T[A]): T[Boolean] = less(lhs, rhs)
    }

    def equiv[A: Ordering : Manifest](lhs: T[A], rhs: T[A]): T[Boolean]

    def less[A: Ordering : Manifest](lhs: T[A], rhs: T[A]): T[Boolean]
  }


  trait RepComparisons extends Comparisons[Rep] {
    def equiv[T: Ordering : Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[Boolean] = ordering_equiv(lhs, rhs)

    def less[T: Ordering : Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[Boolean] = ordering_lt(lhs, rhs)
  }

  trait NoRepComparisons extends Comparisons[NoRep] {
    def equiv[T: Ordering : Manifest](lhs: NoRep[T], rhs: NoRep[T]): NoRep[Boolean] = lhs == rhs

    def less[T: Ordering : Manifest](lhs: NoRep[T], rhs: NoRep[T]): NoRep[Boolean] = implicitly[Ordering[T]].lt(lhs, rhs)
  }


  trait StagedNum[T[_]] extends RepBase[T] {
    implicit def mkNumericOps(lhs: T[Int]): Ops = new Ops(lhs)

    class Ops(lhs: T[Int]) {

      def +(rhs: T[Int]) = plus(lhs, rhs)

      def -(rhs: T[Int]) = minus(lhs, rhs)

      def /(rhs: T[Int]) = div(lhs, rhs)

      def infix_max(rhs: T[Int]) = max(lhs, rhs)
    }


    def plus(lhs: T[Int], rhs: T[Int]): T[Int]

    def minus(lhs: T[Int], rhs: T[Int]): T[Int]

    def div(lhs: T[Int], rhs: T[Int]): T[Int]

    def mod(lhs: T[Int], rhs: T[Int]): T[Int]

    def max(lhs: T[Int], rhs: T[Int]): T[Int]

    def gtimes[A: Numeric : TypeRep](lhs: T[A], rhs: T[A]): T[A]

    def gplus[A: Numeric : TypeRep](lhs: T[A], rhs: T[A]): T[A]

    def gtoDouble[A: Numeric : TypeRep](lhs: T[A]): T[Double]
  }

  trait RepNum extends StagedNum[Rep] {
    def gtoDouble[A: Numeric : TypeRep](lhs: Rep[A]): Rep[Double] = toDouble(lhs)

    def gplus[T: Numeric : TypeRep](lhs: Rep[T], rhs: Rep[T]): Rep[T] = genplus(lhs, rhs)

    def gtimes[T: Numeric : TypeRep](lhs: Rep[T], rhs: Rep[T]): Rep[T] = gentimes(lhs, rhs)

    def plus(lhs: Rep[Int], rhs: Rep[Int]): Rep[Int] = int_plus(lhs, rhs)

    def minus(lhs: Rep[Int], rhs: Rep[Int]): Rep[Int] = int_minus(lhs, rhs)

    def div(lhs: Rep[Int], rhs: Rep[Int]): Rep[Int] = int_divide(lhs, rhs)

    def mod(lhs: Rep[Int], rhs: Rep[Int]): Rep[Int] = int_mod(lhs, rhs)

    def max(lhs: Rep[Int], rhs: Rep[Int]): Rep[Int] = int_max(lhs, rhs)
  }

  trait NoRepNum extends StagedNum[NoRep] {
    def gtoDouble[T: Numeric : TypeRep](lhs: NoRep[T] ): NoRep[Double] = {
      val ev = implicitly[Numeric[T]]
      ev.toDouble(lhs)
    }
    def gplus[T: Numeric : TypeRep](lhs: NoRep[T], rhs: NoRep[T]): NoRep[T] = {
      val ev = implicitly[Numeric[T]]
      ev.plus(lhs, rhs)
    }

    def gtimes[T: Numeric : TypeRep](lhs: NoRep[T], rhs: NoRep[T]): NoRep[T] = {
      val ev = implicitly[Numeric[T]]
      ev.times(lhs, rhs)
    }

    def plus(lhs: NoRep[Int], rhs: NoRep[Int]): NoRep[Int] = lhs + rhs

    def minus(lhs: NoRep[Int], rhs: NoRep[Int]): NoRep[Int] = lhs - rhs

    def div(lhs: NoRep[Int], rhs: NoRep[Int]): NoRep[Int] = lhs / rhs

    def mod(lhs: NoRep[Int], rhs: NoRep[Int]): NoRep[Int] = lhs % rhs

    def max(lhs: NoRep[Int], rhs: NoRep[Int]): NoRep[Int] = Math.max(lhs, rhs)
  }


  trait ChooseStuff[T[_]] {
    def choose_algorithm(x: T[Int]): T[Int]

    def choose_inline(x: T[Int]): T[Boolean]
  }

  trait RepChooseStuff extends ChooseStuff[Rep] {
    def choose_algorithm(x: Rep[Int]): Rep[Int] = choose_sort(x)

    def choose_inline(x: Rep[Int]): Rep[Boolean] = choose_inlinex(x)
  }

  trait NoRepChooseStuff extends ChooseStuff[NoRep] {
    def choose_algorithm(x: NoRep[Int]): NoRep[Int] = 0

    def choose_inline(x: NoRep[Int]): NoRep[Boolean] = x < 100
  }



}