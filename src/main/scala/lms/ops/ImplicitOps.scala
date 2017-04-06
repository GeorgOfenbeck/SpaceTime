package scala.lms
package ops

import java.io.PrintWriter
import org.scala_lang.virtualized.SourceContext

trait ImplicitOps extends Base {
  /**
   *  Implicit conversion from Rep[X] to Rep[Y]
   *
   *  As long as a conversion is in scope, it will be invoked in the generated scala code.
   *  Code-gen for other platforms should implement the conversions.
   **/
  def implicit_convert[X,Y](x: Rep[X])(implicit c: X => Y, mX: Manifest[X], mY: Manifest[Y], pos: SourceContext) : Rep[Y] // = x.asInstanceOf[Rep[Y]
}

trait ImplicitOpsExp extends ImplicitOps with BaseExp {
  case class ImplicitConvert[X,Y](x: Exp[X])(implicit val mX: Manifest[X], val mY: Manifest[Y]) extends Def[Y]

  def implicit_convert[X,Y](x: Exp[X])(implicit c: X => Y, mX: Manifest[X], mY: Manifest[Y], pos: SourceContext) : Rep[Y] = {
    if (mX == mY) x.asInstanceOf[Rep[Y]] else ImplicitConvert[X,Y](x)
  }
}
