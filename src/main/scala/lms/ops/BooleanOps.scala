package scala.lms
package ops

import java.io.PrintWriter
import org.scala_lang.virtualized.SourceContext

import scala.lms.internal.FunctionsExp

trait LiftBoolean {
  this: Base =>

  implicit def boolToBoolRep(b: Boolean) = unit(b)
}

trait BooleanOps extends Base {
  //implicit def var2BooleanOps(x: Var[Boolean]) = new BooleanOps(readVar(x))
  implicit class BooleanOps(x: Rep[Boolean]) {
    def unary_!(implicit pos: SourceContext) = boolean_negate(x)
    def &&(rhs: =>Rep[Boolean])(implicit pos: SourceContext) = boolean_and(x,rhs)
    def ||(rhs: =>Rep[Boolean])(implicit pos: SourceContext) = boolean_or(x,rhs)
  }
  //def infix_unary_!(x: Rep[Boolean])(implicit pos: SourceContext) = boolean_negate(x)
  //def infix_&&(lhs: Rep[Boolean], rhs: Rep[Boolean])(implicit pos: SourceContext) = boolean_and(lhs,rhs)
  //def infix_||(lhs: Rep[Boolean], rhs: Rep[Boolean])(implicit pos: SourceContext) = boolean_or(lhs,rhs)

  def boolean_negate(lhs: Rep[Boolean])(implicit pos: SourceContext): Rep[Boolean]
  def boolean_and(lhs: Rep[Boolean], rhs: Rep[Boolean])(implicit pos: SourceContext): Rep[Boolean]
  def boolean_or(lhs: Rep[Boolean], rhs: Rep[Boolean])(implicit pos: SourceContext): Rep[Boolean]
}

trait BooleanOpsExp extends BooleanOps with BaseExp with FunctionsExp{
  case class BooleanNegate(lhs: Exp[Boolean]) extends Def[Boolean]
  case class BooleanAnd(lhs: Exp[Boolean], rhs: Exp[Boolean]) extends Def[Boolean]
  case class BooleanOr(lhs: Exp[Boolean], rhs: Exp[Boolean]) extends Def[Boolean]

  def boolean_negate(lhs: Exp[Boolean])(implicit pos: SourceContext) : Exp[Boolean] = BooleanNegate(lhs)
  def boolean_and(lhs: Exp[Boolean], rhs: Exp[Boolean])(implicit pos: SourceContext) : Exp[Boolean] = BooleanAnd(lhs,rhs)
  def boolean_or(lhs: Exp[Boolean], rhs: Exp[Boolean])(implicit pos: SourceContext) : Exp[Boolean] = BooleanOr(lhs,rhs)

  /*override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case BooleanNegate(x) => boolean_negate(f(x))
    case BooleanAnd(x,y) => boolean_and(f(x),f(y))
    case BooleanOr(x,y) => boolean_or(f(x),f(y))

    case Reflect(BooleanNegate(x), u, es) => reflectMirrored(Reflect(BooleanNegate(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(BooleanAnd(x,y), u, es) => reflectMirrored(Reflect(BooleanAnd(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(BooleanOr(x,y), u, es) => reflectMirrored(Reflect(BooleanOr(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??*/
}

trait BooleanOpsExpOpt extends BooleanOpsExp {
  override def boolean_negate(lhs: Exp[Boolean])(implicit pos: SourceContext) = lhs match {
    case Def(BooleanNegate(x)) => x
    case _ => super.boolean_negate(lhs)
  }
}

