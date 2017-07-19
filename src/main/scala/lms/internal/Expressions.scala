package scala.lms.internal
import scala.lms._


import org.scala_lang.virtualized.SourceContext
import scala.reflect._
import scala.reflect.runtime.universe._


/**
 * The Expressions trait houses common AST nodes. It also manages a list of encountered Definitions which
 * allows for common sub-expression elimination (CSE).
 *
 * @since 0.1
 */




trait Expressions extends TypeRepBase with Logging{
 //abstract class Exp[+T:TypeRep]
 case class Exp[+T:TypeRep](val id: Int){
   var sourceContexts: Vector[SourceContext] = Vector.empty
   def pos = sourceContexts
   def withPos(pos: Vector[SourceContext]) = { sourceContexts :+ pos; this }
   def tp : TypeRep[_] = {
     getTP(this) match {
       case Some(ftp) => ftp.tag
       case None => assert(false, "tried to get TypeRep of a symbol which apparently does not have an associated TP"); ???
     }
   }
 }

 case class ConstDef[T:TypeRep](x: T) extends Def[T]
 case class ArgDef[T:TypeRep](id: Int) extends Def[T] //used in PureFunctions
 /*def Const[T:TypeRep](x: T) = {
   toAtom(ConstDef(x))
 }*/
 object Const{
  def apply[T:TypeRep](x: T): Exp[T] = {
   toAtom(ConstDef(x))
  }


  def unapply(exp: Exp[_]) = getTP(exp) match {
   case Some(x) => x.rhs match {
    case ConstDef(y) => Some(y)
    case _ => scala.None
   }
   case None => scala.None
  }
  def unapply(rhs: Def[_]) = getTP(rhs) match {
   case Some(x) => x.rhs match {
    case ConstDef(y) => Some(y)
    case _ => scala.None
   }
   case None => scala.None
  }
 }

 case class Variable[+T](val e: Exp[Variable[T]]) // TODO: decide whether it should stay here ... FIXME: should be invariant

 abstract class Def[+T] extends Product { // operations (composite)
  //override final lazy val hashCode = scala.runtime.ScalaRunTime._hashCode(this.asInstanceOf[Product])
 }

 case class TP[T](val sym: Exp[T], val rhs: Def[T], val tag: TypeRep[T])

 var nVars = 0
  var nArgs = 0

  def Arg[T:TypeRep] = {
    val r = toAtom( ArgDef(nArgs))
    nArgs += 1
    r
  }

  def fresh[T:TypeRep]: Exp[T] = Exp[T] {
    nVars += 1;  if (nVars%1000 == 0) printlog("nVars="+nVars);  nVars -1 }
  def fresh[T:TypeRep](pos: Vector[SourceContext]): Exp[T] = fresh[T].withPos(pos)

  def quotePos(e: Exp[_]): String = {
    if(e.pos.isEmpty)
      "<unknown>"
    else
    {
      def all(cs: SourceContext): Vector[SourceContext] = cs.parent match {
        case None => Vector(cs)
        case Some(p) => cs +: all(p)
      }
      e.pos.reverse.map(c => all(c).reverse.map(c => c.fileName.split("/").last + ":" + c.line).mkString("//")).mkString(";")
    }
  }
 var exp2tp: Map[Exp[_], TP[_]] = Map.empty
 var def2tp: Map[Def[_], TP[_]] = Map.empty
 var id2tp: Map[Int, TP[_]] = Map.empty




 def reifySubGraph[T](b: =>T): (T, Vector[TP[T]]) = {
  val r = b
  ???
 }


 def storeTP(tp: TP[_]): Unit = {
  def2tp = def2tp + (tp.rhs -> tp)
  exp2tp = exp2tp + (tp.sym -> tp)
  id2tp = id2tp + (tp.sym.id -> tp)
 }

 def getTP[T](d: Def[T]): Option[TP[T]] = {
  def2tp.get(d).asInstanceOf[Option[TP[T]]] //TODO - get rid of the type cast
 }

 def getTP[T](exp: Exp[T]): Option[TP[T]] = {
  exp2tp.get(exp).asInstanceOf[Option[TP[T]]] //TODO - get rid of the type cast
 }

  def reflectSubGraph(tps: Vector[TP[_]]): Unit = tps.map(reflectSubGraph)

 def reflectSubGraph(tp: TP[_]): Unit = {
  assert(getTP(tp.rhs).isEmpty, "already defined" + tp)
  storeTP(tp)
 }

 def getTypeRep(s: Exp[_]): TypeRep[_] = getTP(s).get.tag //can this fail?

 def findDefinition[T](s: Exp[T]): Option[TP[T]] = getTP(s)

 def findDefinition[T](d: Def[T]): Option[TP[T]] = getTP(d)

 def findOrCreateDefinition[T:TypeRep](d: Def[T], pos: Vector[SourceContext]): TP[T] = getTP(d).getOrElse(createDefinition(fresh[T](pos), d))

 def findOrCreateDefinitionExp[T:TypeRep](d: Def[T], pos: Vector[SourceContext]): Exp[T] = findOrCreateDefinition(d,pos).sym

 def createDefinition[T](s: Exp[T], d: Def[T]) (implicit tag: TypeRep[T]): TP[T] = {
  val f = TP(s, d, tag)
  reflectSubGraph(f)
  f
 }

 protected implicit def toAtom[T:TypeRep](d: Def[T])(implicit pos: SourceContext): Exp[T] = {
   val t = findOrCreateDefinitionExp(d, Vector(pos))
   t
 }



 // regular data (and effect) dependencies
 def syms(e: Any): Vector[Exp[_]] = e match {
  case s: Exp[Any] => Vector(s)
  case ss: Iterable[Any] => ss.toVector.flatMap(syms(_))
  // All case classes extend Product!
  case p: Product =>
   import scala.collection.mutable.ListBuffer
   //return p.productIterator.toList.flatMap(syms(_))
   /* performance hotspot */
   val iter = p.productIterator
   val out = new ListBuffer[Exp[Any]]
   while (iter.hasNext) {
    val e = iter.next()
    out ++= syms(e)
   }
   out.result.toVector
  case _ => Vector.empty
 }
  // symbols which are bound in a definition
  def boundSyms(e: Any): Vector[Exp[_]] = e match {
    case ss: Iterable[Any] => ss.toVector.flatMap(boundSyms(_))
    case p: Product => p.productIterator.toVector.flatMap(boundSyms(_))
    case _ => Vector.empty
  }

  // symbols which are bound in a definition, but also defined elsewhere
  def tunnelSyms(e: Any): Vector[Exp[_]] = e match {
    case ss: Iterable[Any] => ss.toVector.flatMap(tunnelSyms(_))
    case p: Product => p.productIterator.toVector.flatMap(tunnelSyms(_))
    case _ => Vector.empty
  }

  // symbols of effectful components of a definition
  def effectSyms(x: Any): Vector[Exp[_]] = x match {
    case ss: Iterable[Any] => ss.toVector.flatMap(effectSyms(_))
    case p: Product => p.productIterator.toVector.flatMap(effectSyms(_))
    case _ => Vector.empty
  }

  // soft dependencies: they are not required but if they occur,
  // they must be scheduled before
  def softSyms(e: Any): Vector[Exp[_]] = e match {
    // empty by default
    //case s: Sym[Any] => List(s)
    case ss: Iterable[Any] => ss.toVector.flatMap(softSyms(_))
    case p: Product => p.productIterator.toVector.flatMap(softSyms(_))
    case _ => Vector.empty
  }


  def rsyms[T](e: Any)(f: Any=>Vector[T]): Vector[T] = e match {
    case s: Exp[Any] => f(s)
    case ss: Iterable[Any] => ss.toVector.flatMap(f)
    case p: Product => p.productIterator.toVector.flatMap(f)
    case _ => Vector.empty
  }

  // frequency information for dependencies: used/computed
  // often (hot) or not often (cold). used to drive code motion.
  def symsFreq(e: Any): Vector[(Exp[_], Double)] = e match {
    case s: Exp[_] => Vector((s,1.0))
    case ss: Iterable[Any] => ss.toVector.flatMap(symsFreq(_))
    case p: Product => p.productIterator.toVector.flatMap(symsFreq(_))
    //case _ => rsyms(e)(symsFreq)
    case _ => Vector.empty
  }

  def freqNormal(e: Any) = symsFreq(e)
  def freqHot(e: Any) = symsFreq(e).map(p=>(p._1,p._2*1000.0))
  def freqCold(e: Any) = symsFreq(e).map(p=>(p._1,p._2*0.5))


 // graph construction state
 object Def {
  def unapply[T: TypeRep](e: Exp[T]): Option[Def[T]] = e match { // really need to test for sym?
   case s @ Exp(_) => {
     val t = exp2tp.get(s)
     val t1: Option[Def[_]] = t.map(e => e.rhs)
     t1.asInstanceOf[Option[Def[T]]] //RF! get rid of cast if possible (or at least check the tag)
   }


    //findDefinition(s).flatMap(_.defines(s))
    //findDefinition(s).map( x => x.rhs.asInstanceOf[Def[T]]) //get rid of the cast

   case _ =>
    None
  }
 }

 def reset() =  {
  if (!id2tp.isEmpty)
  {
    println("WARNING! You are resetting the IR and it contained already symbols - that might not be intended")
  }
  nVars = 0
  nArgs = 0
  exp2tp = Map.empty
  def2tp = Map.empty
  id2tp = Map.empty

 }

}
