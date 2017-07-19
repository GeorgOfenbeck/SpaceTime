package scala.lms
import scala.reflect._
import scala.reflect.runtime.universe._

/**
 * Component for abstraction over the run-time representation of types. The `TypeRep` abstraction
 * can carry additional information with the run-time type (e.g. bit width for hardware representation).
 *
 * NOTE: Parametric types must be lifted explicitly since compiler does not generate
 * TypeRep[X[T]] if implict TypeRep[T] is in scope. For example, @see LiftArrayType.
 */
trait TypeRepBase {
 trait TypeRep[T] {
  def mf: ClassTag[T]

  //def typeArguments: List[ClassTag[_]]
  //def arrayManifest: ClassTag[Array[T]]
  def runtimeClass: java.lang.Class[_]
  //def erasure: java.lang.Class[_]
  //def <:<(that: TypeRep[_]): Boolean
  def dynTags: Option[ Unit => (Vector[TypeRep[_]],Vector[TypeRep[_]])]
 }

 case class TypeExp[T](mf: ClassTag[T], dynTags: Option[ Unit => (Vector[TypeRep[_]],Vector[TypeRep[_]])] = None) extends TypeRep[T] {
  //def typeArguments: List[ClassTag[_]]   = mf.typeArguments
  //def arrayManifest: ClassTag[Array[T]] = mf.arrayManifest
  def runtimeClass: java.lang.Class[_] = mf.runtimeClass
  //def <:<(that: TypeRep[_]): Boolean = mf.<:<(that.mf)
  //def erasure: java.lang.Class[_] = mf.erasure
  override def canEqual(that: Any): Boolean = mf.canEqual(that)
  override def equals(that: Any): Boolean = mf.equals(that)
  override def hashCode = mf.hashCode
  override def toString = mf.toString
 }

 def typeRep[T](implicit tr: TypeRep[T]): TypeRep[T] = tr
 implicit def typeRepFromManifest[T](implicit mf: ClassTag[T]): TypeRep[T] = TypeExp(mf)
 implicit def convertFromManifest[T](mf: ClassTag[T]): TypeRep[T] = TypeExp(mf)
}


/**
 * The Base trait defines the type constructor Rep, which is the higher-kinded type that allows for other DSL types to be
 * polymorphically embedded.
 *
 * @since 0.1
 */
trait Base extends TypeRepBase {
 type API <: Base
 type Rep[T]
 protected def unit[T:TypeRep](x: T): Rep[T]            //TODO - why protected??

 // always lift Unit and Null (for now)
 implicit def unitToRepUnit(x: Unit): Rep[Unit] = unit(x)
 //implicit def nullToRepNull(x: Null): Rep[Null] = unit(x)
 def typeRep[T](implicit tr: TypeRep[T]): TypeRep[T]


}



import scala.lms.internal._
/**
 * This trait sets the representation to be based on AST Expression nodes.
 *
 * @since 0.1
 */
trait BaseExp extends Base with Functions with Blocks { //with Effects{
  type Rep[T] = Exp[T]
  def unit[T:TypeRep](x: T) = Const(x)


 implicit def exposeRepFromRep[T](implicit tag: TypeRep[T]): ExposeRep[Rep[T]] = new ExposeRep[Exp[T]](){
  def freshExps() = Vector(Arg[T](tag))
  def vec2t(v: Vector[Exp[_]]) = v.head.asInstanceOf[Exp[T]] //TODO: Horrible cast - get rid of it
  def t2vec(x: Rep[T]) = Vector(x)
 }
}



trait EffectExp extends BaseExp with Effects {
  /*def mapOver(t: Transformer, u: Summary) = { // TODO: move to effects class?
  u.copy(mayRead = t.onlySyms(u.mayRead), mstRead = t.onlySyms(u.mstRead),
   mayWrite = t.onlySyms(u.mayWrite), mstWrite = t.onlySyms(u.mstWrite))
 }

 override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = e match {
  case Reflect(x, u, es) => Reflect(mirrorDef(x,f), mapOver(f,u), f(es))
  case Reify(x, u, es) => Reify(f(x), mapOver(f,u), f(es))
  case _ => super.mirrorDef(e,f)
 }

 override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
  /*
      case Reflect(x, u, es) =>
        reifyEffects {
          context = f(es)
          mirror(x)
        }

  */
  //    case Reflect(Print(x), u, es) => Reflect(Print(f(x)), es map (e => f(e)))
  case Reflect(x, u, es) => reflectMirrored(mirrorDef(e,f).asInstanceOf[Reflect[A]])
  case Reify(x, u, es) => Reify(f(x), mapOver(f,u), f(es)) //TODO: u
  case _ => super.mirror(e,f)
 }*/
}

