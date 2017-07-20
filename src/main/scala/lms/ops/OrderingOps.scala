package scala.lms
package ops
import scala.reflect._
import scala.reflect.runtime.universe._

import java.io.PrintWriter
import org.scala_lang.virtualized.SourceContext
import scala.lms.internal.Expressions
import util.OverloadHack

trait OrderingOps extends Base with OverloadHack {
 // workaround for infix not working with implicits in PrimitiveOps
 implicit def orderingToOrderingOps[T:Ordering:ClassTag](n: T):  OrderingOpsCls[T] = new OrderingOpsCls(unit(n))
 implicit def repOrderingToOrderingOps[T:Ordering:ClassTag](n: Rep[T]):  OrderingOpsCls[T] = new OrderingOpsCls(n)
// implicit def varOrderingToOrderingOps[T:Ordering:ClassTag](n: Var[T]) = new OrderingOpsCls(readVar(n))

 class OrderingOpsCls[T:Ordering:ClassTag](lhs: Rep[T]){
  def <(rhs: Rep[T])(implicit pos: SourceContext) = ordering_lt(lhs, rhs)
  def <=(rhs: Rep[T])(implicit pos: SourceContext) = ordering_lteq(lhs, rhs)
  def >(rhs: Rep[T])(implicit pos: SourceContext) = ordering_gt(lhs, rhs)
  def >=(rhs: Rep[T])(implicit pos: SourceContext) = ordering_gteq(lhs, rhs)
  def equiv(rhs: Rep[T])(implicit pos: SourceContext) = ordering_equiv(lhs, rhs)
  def max(rhs: Rep[T])(implicit pos: SourceContext) = ordering_max(lhs, rhs)
  def min(rhs: Rep[T])(implicit pos: SourceContext) = ordering_min(lhs, rhs)
  def compare(rhs: Rep[T])(implicit pos: SourceContext) = ordering_compare(lhs, rhs)

  def <[B](rhs: B)(implicit c: B => Rep[T], pos: SourceContext) = ordering_lt(lhs, c(rhs))
  def <=[B](rhs: B)(implicit c: B => Rep[T], pos: SourceContext) = ordering_lteq(lhs, c(rhs))
  def >[B](rhs: B)(implicit c: B => Rep[T], pos: SourceContext) = ordering_gt(lhs, c(rhs))
  def >=[B](rhs: B)(implicit c: B => Rep[T], pos: SourceContext) = ordering_gteq(lhs, c(rhs))
  def equiv[B](rhs: B)(implicit c: B => Rep[T], pos: SourceContext) = ordering_equiv(lhs, c(rhs))
  def max[B](rhs: B)(implicit c: B => Rep[T], pos: SourceContext) = ordering_max(lhs, c(rhs))
  def min[B](rhs: B)(implicit c: B => Rep[T], pos: SourceContext) = ordering_min(lhs, c(rhs))
  def compare[B](rhs: B)(implicit c: B => Rep[T], pos: SourceContext) = ordering_compare(lhs, c(rhs))
 }

 //  def infix_<[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => Rep[T], mT: ClassTag[T]) = ordering_lt(lhs,c(rhs))
 //  def infix_<=[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => Rep[T], mT: ClassTag[T]) = ordering_lteq(lhs,c(rhs))
 //  def infix_>[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => Rep[T], mT: ClassTag[T]) = ordering_gt(lhs,c(rhs))
 //  def infix_>=[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => Rep[T], mT: ClassTag[T]) = ordering_gteq(lhs,c(rhs))
 //  def infix_equiv[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => Rep[T], mT: ClassTag[T]) = ordering_equiv(lhs,c(rhs))
 //  def infix_max[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => Rep[T], mT: ClassTag[T]) = ordering_max(lhs,c(rhs))
 //  def infix_min[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => Rep[T], mT: ClassTag[T]) = ordering_min(lhs,c(rhs))

 def ordering_lt[T:Ordering:ClassTag](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Boolean]
 def ordering_lteq[T:Ordering:ClassTag](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Boolean]
 def ordering_gt[T:Ordering:ClassTag](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Boolean]
 def ordering_gteq[T:Ordering:ClassTag](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Boolean]
 def ordering_equiv[T:Ordering:ClassTag](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Boolean]
 def ordering_max[T:Ordering:ClassTag](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
 def ordering_min[T:Ordering:ClassTag](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
 def ordering_compare[T:Ordering:ClassTag](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Int]
}


trait OrderingOpsExp extends OrderingOps with BaseExp{
 abstract class DefMN[T:Ordering:ClassTag,A] extends Def[A] {
  def mev = ClassTag[T]
  def aev = implicitly[Ordering[T]]
 }
 case class OrderingLT[T:Ordering:ClassTag](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,Boolean]
 case class OrderingLTEQ[T:Ordering:ClassTag](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,Boolean]
 case class OrderingGT[T:Ordering:ClassTag](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,Boolean]
 case class OrderingGTEQ[T:Ordering:ClassTag](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,Boolean]
 case class OrderingEquiv[T:Ordering:ClassTag](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,Boolean]
 case class OrderingMax[T:Ordering:ClassTag](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,T]
 case class OrderingMin[T:Ordering:ClassTag](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,T]
 case class OrderingCompare[T:Ordering:ClassTag](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,Int]

 def ordering_lt[T:Ordering:ClassTag](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[Boolean] = OrderingLT(lhs,rhs)
 def ordering_lteq[T:Ordering:ClassTag](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[Boolean] = OrderingLTEQ(lhs,rhs)
 def ordering_gt[T:Ordering:ClassTag](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[Boolean] = OrderingGT(lhs,rhs)
 def ordering_gteq[T:Ordering:ClassTag](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[Boolean] = OrderingGTEQ(lhs,rhs)
 def ordering_equiv[T:Ordering:ClassTag](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[Boolean] = OrderingEquiv(lhs,rhs)
 def ordering_max[T:Ordering:ClassTag](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[T] = OrderingMax(lhs,rhs)
 def ordering_min[T:Ordering:ClassTag](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[T] = OrderingMin(lhs,rhs)
 def ordering_compare[T:Ordering:ClassTag](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Rep[Int] = OrderingCompare(lhs,rhs)

}

