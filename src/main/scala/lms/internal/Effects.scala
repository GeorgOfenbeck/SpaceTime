package scala.lms.internal

import org.scala_lang.virtualized.SourceContext
import scala.collection.mutable
import scala.reflect._
import scala.reflect.runtime.universe._


trait EffectSummary extends Expressions {

  case class Summary(
                      val maySimple: Boolean,
                      val mstSimple: Boolean,
                      val mayGlobal: Boolean,
                      val mstGlobal: Boolean,
                      val resAlloc: Boolean,
                      val control: Boolean,
                      val mayRead: Vector[Exp[_]],
                      val mstRead: Vector[Exp[_]],
                      val mayWrite: Vector[Exp[_]],
                      val mstWrite: Vector[Exp[_]]) {

    def orElse(v: Summary) = infix_orElse(this, v)

    def andAlso(v: Summary) = infix_andAlso(this, v)

    def andThen(v: Summary) = infix_andThen(this, v)

    def star = infix_star(this)

    def withoutControl = infix_withoutControl(this)
  }


  def infix_orElse(u: Summary, v: Summary): Summary = new Summary(
    u.maySimple || v.maySimple, u.mstSimple && v.mstSimple,
    u.mayGlobal || v.mayGlobal, u.mstGlobal && v.mstGlobal,
    false, //u.resAlloc && v.resAlloc, <--- if/then/else will not be mutable!
    u.control || v.control,
    (u.mayRead ++ v.mayRead).distinct, (u.mstRead intersect v.mstRead),
    (u.mayWrite ++ v.mayWrite).distinct, (u.mstWrite intersect v.mstWrite)
  )

  def infix_andAlso(u: Summary, v: Summary): Summary = new Summary(
    u.maySimple || v.maySimple, u.mstSimple || v.mstSimple,
    u.mayGlobal || v.mayGlobal, u.mstGlobal || v.mstGlobal,
    u.resAlloc || v.resAlloc,
    u.control || v.control,
    (u.mayRead ++ v.mayRead).distinct, (u.mstRead ++ v.mstRead).distinct,
    (u.mayWrite ++ v.mayWrite).distinct, (u.mstWrite ++ v.mstWrite).distinct
  )

  def infix_andThen(u: Summary, v: Summary): Summary = new Summary(
    u.maySimple || v.maySimple, u.mstSimple || v.mstSimple,
    u.mayGlobal || v.mayGlobal, u.mstGlobal || v.mstGlobal,
    v.resAlloc,
    u.control || v.control,
    (u.mayRead ++ v.mayRead).distinct, (u.mstRead ++ v.mstRead).distinct,
    (u.mayWrite ++ v.mayWrite).distinct, (u.mstWrite ++ v.mstWrite).distinct
  )

  def infix_star(u: Summary): Summary = Pure() orElse u // any number of repetitions, including 0

  def infix_withoutControl(u: Summary): Summary = new Summary(
    u.maySimple, u.mstSimple,
    u.mayGlobal, u.mstGlobal,
    u.resAlloc,
    false,
    u.mayRead, u.mstRead,
    u.mayWrite, u.mstWrite
  )

  def Pure() = new Summary(false, false, false, false, false, false, Vector.empty, Vector.empty, Vector.empty, Vector.empty)

  def Simple() = new Summary(true, true, false, false, false, false, Vector.empty, Vector.empty, Vector.empty, Vector.empty)

  def Global() = new Summary(false, false, true, true, false, false, Vector.empty, Vector.empty, Vector.empty, Vector.empty)

  def Alloc() = new Summary(false, false, false, false, true, false, Vector.empty, Vector.empty, Vector.empty, Vector.empty)

  def Control() = new Summary(false, false, false, false, false, true, Vector.empty, Vector.empty, Vector.empty, Vector.empty)

  def Read(v: Vector[Exp[_]]) = new Summary(false, false, false, false, false, false, v.distinct, v.distinct, Vector.empty, Vector.empty)

  def Write(v: Vector[Exp[_]]) = new Summary(false, false, false, false, false, false, Vector.empty, Vector.empty, v.distinct, v.distinct)

  def mayRead(u: Summary, a: Vector[Exp[_]]): Boolean = u.mayGlobal || a.exists(u.mayRead contains _)

  def mayWrite(u: Summary, a: Vector[Exp[_]]): Boolean = u.mayGlobal || a.exists(u.mayWrite contains _)

  def maySimple(u: Summary): Boolean = u.mayGlobal || u.maySimple

  def mustMutable(u: Summary): Boolean = u.resAlloc

  def mustPure(u: Summary): Boolean = u == Pure()

  def mustOnlyRead(u: Summary): Boolean = u == Pure().copy(mayRead = u.mayRead, mstRead = u.mstRead)

  // only reads allowed
  def mustIdempotent(u: Summary): Boolean = mustOnlyRead(u) // currently only reads are treated as idempotent
}


trait Effects extends Functions with Blocks with Logging {


  type State = Vector[(Exp[_], Reflect[_])]
  // TODO: maybe use TP instead to save lookup
  var context: State = Vector.empty


  // --- class defs

  case class Reflect[A](x: Def[A], summary: Summary, deps: Vector[Exp[_]]) extends Def[A]

  //case class Reify(x: Vector[Exp[_]], summary: Summary, effects: Vector[Exp[_]]) extends Def[Any]

  // --- summary


  def reflectMutable[A: TypeTag](d: Def[A])(implicit pos: SourceContext): Exp[A] = {
    val z = reflectEffect(d, Alloc())
    val mutableAliases = mutableTransitiveAliases(d)
    checkIllegalSharing(z, mutableAliases)
    z
  }

  def checkIllegalSharing(z: Exp[_], mutableAliases: Vector[Exp[_]]): Unit =  {
    if (mutableAliases.nonEmpty) {
      val zd = z match {
        case Def(zd) => zd
      }
      printerr("error: illegal sharing of mutable objects " + mutableAliases.mkString(", "))
      printerr("at " + z + "=" + zd)
      printsrc("in " + quotePos(z))
    }
  }


  // TODO possible optimization: a mutable object never aliases another mutable object, so its inputs need not be followed
  def mutableTransitiveAliases(s: Any) = {
    val aliases = allAliases(s)
    val bareMutableSyms = aliases filter { o => globalMutableSyms.contains(o) }
    val definedMutableSyms = utilLoadStms(aliases) collect { case TP(s2, Reflect(_, u, _), tag) if mustMutable(u) => s2 }
    bareMutableSyms ++ definedMutableSyms
  }

  def allAliases(start: Any): Vector[Exp[_]] = {
    val r = (shallowAliases(start) ++ deepAliases(start)).distinct
    //printdbg("all aliases of " + start + ": " + r.mkString(", "))
    r
  }

  def shallowAliases(start: Any): Vector[Exp[_]] = {
    val alias = noPrim(aliasSyms(start)) flatMap { a => a +: shallowAliasCache.getOrElseUpdate(a, shallowAliases(utilLoadSym(a))) }
    val extract = noPrim(extractSyms(start)) flatMap { a => deepAliasCache.getOrElseUpdate(a, deepAliases(utilLoadSym(a))) }
    //println("shallowAliases("+start+") = "+alias+" ++ "+extract)
    (alias ++ extract).distinct
  }

  def deepAliases(start: Any): Vector[Exp[_]] = {
    val alias = noPrim(aliasSyms(start)) flatMap { a => deepAliasCache.getOrElseUpdate(a, deepAliases(utilLoadSym(a))) }
    val copy = noPrim(copySyms(start)) flatMap { a => deepAliasCache.getOrElseUpdate(a, deepAliases(utilLoadSym(a))) }
    val contain = noPrim(containSyms(start)) flatMap { a => a +: allAliasCache.getOrElseUpdate(a, allAliases(utilLoadSym(a))) }
    //println("aliasSyms("+start+") = "+aliasSyms(start) + "/" + noPrim(aliasSyms(start)))
    //println("copySyms("+start+") = "+copySyms(start) + "/" + noPrim(copySyms(start)))
    //println("containSyms("+start+") = "+containSyms(start) + "/" + noPrim(containSyms(start)))
    //println("deepAliases("+start+") = "+alias+" ++ "+copy+" ++ "+contain)
    (alias ++ copy ++ contain).distinct
  }

  def pruneContext(ctx: Vector[Exp[_]]) = ctx // TODO this doesn't work yet (because of loops!): filterNot { case Def(Reflect(_,u,_)) => mustOnlyRead(u) }


  def calculateDependencies(u: Summary): State = calculateDependencies(context, u, true)

  def calculateDependencies(scope: State, u: Summary, mayPrune: Boolean): State = {
    if (u.mayGlobal) scope
    else {
      val read = u.mayRead
      val write = u.mayWrite

      // TODO: in order to reduce the number of deps (need to traverse all those!)
      // we should only store those that are not transitively implied.
      // For simple effects, take the last one (implemented).
      // For mutations, take the last write to a particular mutable sym (TODO).

      def canonic(xs: State) = xs // TODO
      def canonicLinear(xs: State) = if (mayPrune) xs.takeRight(1) else xs

      // the mayPrune flag is for test8-speculative4: with pruning on, the 'previous iteration'
      // dummy is moved out of the loop. this is not per se a problem -- need to look some more into it.

      /*val readDeps = if (read.isEmpty) Vector.empty else scope filter { case e@Def(Reflect(_, u, _)) => mayWrite(u, read) || read.contains(e) }
      val softWriteDeps = if (write.isEmpty) Vector.empty else scope filter { case e@Def(Reflect(_, u, _)) => mayRead(u, write) }
      val writeDeps = if (write.isEmpty) Vector.empty else scope filter { case e@Def(Reflect(_, u, _)) => mayWrite(u, write) || write.contains(e) }
      val simpleDeps = if (!u.maySimple) Vector.empty else scope filter { case e@Def(Reflect(_, u, _)) => u.maySimple }
      val controlDeps = if (!u.control) Vector.empty else scope filter { case e@Def(Reflect(_, u, _)) => u.control }
      val globalDeps = scope filter { case e@Def(Reflect(_, u, _)) => u.mayGlobal }*/


      val readDeps = if (read.isEmpty) Vector.empty else scope.filter(p => mayWrite(p._2.summary, read) || read.contains(p._2))
      val softWriteDeps = if (write.isEmpty) Vector.empty else scope.filter(p => mayRead(p._2.summary, write))
      val writeDeps = if (write.isEmpty) Vector.empty else scope.filter(p => mayWrite(p._2.summary, write) || write.contains(p._2))
      val simpleDeps = if (!u.maySimple) Vector.empty else scope.filter(p => p._2.summary.maySimple)
      val controlDeps = if (!u.control) Vector.empty else scope.filter(p => p._2.summary.control)
      val globalDeps = scope.filter(p => p._2.summary.mayGlobal)



      // TODO: write-on-read deps should be weak
      // TODO: optimize!!
      val allDeps = canonic(readDeps ++ softWriteDeps ++ writeDeps ++ canonicLinear(simpleDeps) ++ canonicLinear(controlDeps) ++ canonicLinear(globalDeps))
      scope filter (allDeps contains _)
    }
  }

  def isWritableSym[A](w: Exp[A]): Boolean = {
    findDefinition(w) match {
      case Some(TP(_, Reflect(_, u, _), _)) if mustMutable(u) => true // ok
      case o => globalMutableSyms.contains(w)
    }
  }

  //This does not exist any more since only functions can be staged now - therefore there will always be a block!
  /*def checkContext() {
   ???
   /*if (context == null)
    sys.error("uninitialized effect context: effectful statements may only be used within a reifyEffects { .. } block")*/
  }*/

  def reflectEffectInternal[A: TypeRep](x: Def[A], u: Summary)(implicit pos: SourceContext): Exp[A] = {
    if (mustPure(u)) super.toAtom(x)
    else {
      // NOTE: reflecting mutable stuff *during mirroring* doesn't work right now.
      // FIXME: Reflect(Reflect(ObjectUnsafeImmutable(..))) on delite
      assert(!x.isInstanceOf[Reflect[_]], x)
      val deps = calculateDependencies(u)
      val depexps = deps.map(e => e._1)
      val zd = Reflect(x, u, depexps)
      if (mustIdempotent(u)) {
        ??? //Check this!
        context find { case Def(d) => d == zd } map {
          _.asInstanceOf[Exp[A]]
        } getOrElse {
          //        findDefinition(zd) map (_.sym) filter (context contains _) getOrElse { // local cse TODO: turn around and look at context first??
          val z = fresh[A](Vector(pos))
          if (!x.toString.startsWith("ReadVar")) {
            // supress output for ReadVar
            printlog("promoting to effect: " + z + "=" + zd)
            for (w <- u.mayRead)
              printlog("depends on  " + w)
          }
          createReflectDefinition(z, zd)
        }
      } else {
        val z = fresh[A](Vector(pos))
        // make sure all writes go to allocs
        for (w <- u.mayWrite if !isWritableSym(w)) {
          printerr("error: write to non-mutable " + w + " -> " + findDefinition(w))
          printerr("at " + z + "=" + zd)
          printsrc("in " + quotePos(z))
        }
        // prevent sharing between mutable objects / disallow mutable escape for non read-only operations
        // make sure no mutable object becomes part of mutable result (in case of allocation)
        // or is written to another mutable object (in case of write)
        /*
          val a = mzeros(100)
          val b = zeros(100)
          val c = if (..) {
            a.update
            b
          } else {
            a
          }

          PROBLEM: the whole if expr has summary mayWrite=List(a), mstWrite=Vector.empty and allAliases=List(a,b)

          what is the right thing?
          - mutableAliases \ mstWrite <-- first try, but maybe to restrictive?
          - mutableAliases \ mayWrite <-- too permissive?
          - something else?

        */
        createReflectDefinition(z, zd)
      }
    }
  }

  def createReflectDefinition[A](s: Exp[A], x: Reflect[A])(implicit tag: TypeRep[A]): Exp[A] = {
    createDefinition(s, x)(tag)
    val t: (Exp[_], Reflect[_]) = (s, x)
    context = context :+ t
    s
  }


  def summarizeContext(): Summary = {
    // compute an *external* summary for a seq of nodes
    // don't report any reads/writes on data allocated within the block
    var u = Pure()
    var ux = u
    var allocs: Vector[Exp[_]] = Vector.empty
    def clean(xs: Vector[Exp[_]]) = xs.filterNot(allocs contains _)
    for ((s, ref) <- context) {
      val u2 = ref.summary
      if (mustMutable(u2)) allocs :+= s
      u = u andThen (u2.copy(mayRead = clean(u2.mayRead), mstRead = clean(u2.mstRead),
        mayWrite = clean(u2.mayWrite), mstWrite = clean(u2.mstWrite)))
      ux = ux andThen u2
    }
    //if (ux != u) printdbg("** effect summary reduced from "+ux+" to" + u)
    u

  }

  def reflectEffect[A: TypeRep](x: Def[A])(implicit pos: SourceContext): Exp[A] = reflectEffect(x, Simple()) // simple effect (serialized with respect to other simples)

  def reflectEffect[A: TypeRep](d: Def[A], u: Summary)(implicit pos: SourceContext): Exp[A] = {
    // are we depending on a variable? then we need to be serialized -> effect
    val mutableInputs = readMutableData(d)
    reflectEffectInternal(d, u andAlso Read(mutableInputs)) // will call super.toAtom if mutableInput.isEmpty
  }

  def readMutableData[A](d: Def[A]) = {
    val bound = boundSyms(d)
    mutableTransitiveAliases(getActuallyReadSyms(d)) filterNot (bound contains _)
  }

  def aliasSyms(e: Any): Vector[Exp[_]] = e match {
    case Reflect(x, u, es) => aliasSyms(x)
    //case Reify(x, u, es) => syms(x)
    case s: Exp[_] => Vector(s)
    case p: Product => p.productIterator.toVector.flatMap(aliasSyms(_))
    case _ => Vector.empty
  }


  def extractSyms(e: Any): Vector[Exp[_]] = e match {
    case Reflect(x, u, es) => extractSyms(x)
    //case Reify(x, u, es) => Vector.empty
    case s: Exp[_] => Vector.empty
    case p: Product => p.productIterator.toVector.flatMap(extractSyms(_))
    case _ => Vector.empty
  }

  def copySyms(e: Any): Vector[Exp[_]] = e match {
    case Reflect(x, u, es) => copySyms(x)
    //case Reify(x, u, es) => Vector.empty
    case s: Exp[_] => Vector.empty
    case p: Product => p.productIterator.toVector.flatMap(copySyms(_))
    case _ => Vector.empty
  }


  def readSyms(e: Any): Vector[Exp[_]] = e match {
    case Reflect(x, u, es) => readSyms(x) // ignore effect deps (they are not read!)
    /*case Reify(x, u, es) =>
     // in general: the result of a block is not read but passed through.
     // FIXME this piece of logic is not clear. is it a special case for unit??
     // it looks like this was introduced to prevent the Reify to be reflected
     // if x is a mutable object defined within the block.
     // TODO the globalMutableSyms part was added later (June 2012) -- make sure it does the right thing
     if ((es contains x) || (globalMutableSyms contains x)) Vector.empty
     else readSyms(x)*/
    case s: Exp[_] => Vector(s)
    case p: Product => p.productIterator.toVector.flatMap(readSyms(_))
    case _ => Vector.empty
  }

  /*

  the methods below define the sharing relation between the
  result of an operation and its arguments.

  how do i use them? what do i need to return?

  assume an operation foo:

  y = Foo(x)

  x should be returned in the following cases:

  x in aliasSyms(y)      if y = x      // if then else
  x in containSyms(y)    if *y = x     // array update
  x in extractSyms(y)    if y = *x     // array apply
  x in copySyms(y)       if *y = *x    // array clone

  y = x is to be understood as "y may be equal to x"
  *y = x as "dereferencing y (at some index) may return x"
  etc.

  */


  def containSyms(e: Any): Vector[Exp[_]] = e match {
    case Reflect(x, u, es) => containSyms(x)
    //case Reify(x, u, es) => Vector.empty
    case s: Exp[_] => Vector.empty
    case p: Product => p.productIterator.toVector.flatMap(containSyms(_))
    case _ => Vector.empty
  }


  def noPrim(sm: Vector[Exp[_]]): Vector[Exp[_]] = sm.filterNot(s => isPrimitiveType(s.tp))

  def isPrimitiveType[T](m: TypeRep[T]) = m.toString match {
    case "Byte" | "Char" | "Short" | "Int" | "Long" | "Float" | "Double" | "Boolean" | "Unit" => true
    case _ => false
  }

  /*
   TODO: switch back to graph based formulation -- this will not work for circular deps
  */
  val shallowAliasCache = new mutable.HashMap[Exp[_], Vector[Exp[_]]]
  val deepAliasCache = new mutable.HashMap[Exp[_], Vector[Exp[_]]]
  val allAliasCache = new mutable.HashMap[Exp[_], Vector[Exp[_]]]
  var globalMutableSyms: Vector[Exp[_]] = Vector.empty

  def utilLoadStm(s: Exp[_]) = {
    if (!isPrimitiveType(s.tp)) {
      /*globalDefs.filter{e => e.lhs contains s}*/ findDefinition(s).toVector
    }
    else Vector.empty
  }

  def utilLoadStms(s: Vector[Exp[_]]) = s.flatMap(a => {
    val b: Vector[TP[_]] = utilLoadStm(a)
    b
  })

  def utilLoadSym[T](s: Exp[T]) = utilLoadStm(s).map(_.rhs)


  def getActuallyReadSyms[A](d: Def[A]) = {
    val bound = boundSyms(d)
    //val r = readSyms(d).map{case Def(Reify(x,_,_)) => x case x => x} filterNot (bound contains _) //before RF
    val r = readSyms(d) //RF!!!

    //if (d.isInstanceOf[Reify[Any]] && r.nonEmpty) {
    //  println("** actually read: "+readSyms(d)+"\\"+bound+"="+r)
    //  println("** transitive shallow: " + shallowAliases(r))
    //  println("** transitive deep: " + deepAliases(r))
    //}
    //r
    Vector.empty
  }


  def reflectWrite[A: Manifest](write0: Exp[Any]*)(d: Def[A])(implicit pos: SourceContext): Exp[A] = {
    val write = write0.toVector.asInstanceOf[Vector[Exp[_]]] // should check...
    val z = reflectEffect(d, Write(write))
    val mutableAliases = mutableTransitiveAliases(d) filterNot (write contains _)
    checkIllegalSharing(z, mutableAliases)
    z
  }

  override def reset() = {
    context = Vector.empty
    super.reset()
  }

}

/* // TODO: transform over Summary currently lives in common/Base.scala. move it here?
 // --- context


/*



*/

 //var conditionalScope = false // used to construct Control nodes






 def summarizeEffects(e: Block): Summary = e.res match {
  case Vector(Def(Reify(_,u,_))) => u //RF: correct me after the block change
  case _ => Pure()
 }

 // --- reflect helpers
 def controlDep(x: Exp[_]): Boolean = x match {
  case Def(Reflect(y,u,es)) if u == Control() => true
  case _ => false
 }

 // performance hotspot
 def nonControlSyms[R](es: Vector[Exp[_]], ss: Any => Vector[R]): Vector[R] = {
  import scala.collection.mutable
  // es.filterNot(controlDep).flatMap(syms)
  val out = new mutable.ListBuffer[R]
  val it = es.iterator
  while (it.hasNext) {
   val e = it.next()
   if (!controlDep(e)) out ++= ss(e)
  }
  out.result.toVector
 }

 override def syms(e: Any): Vector[Exp[_]] = e match {
  case s: Summary => Vector.empty // don't count effect summaries as dependencies!
  // enable DCE of reflect nodes if they are only control dependencies
  case Reflect(x,u,es) if addControlDeps => syms(x) ++ nonControlSyms(es, syms)
  case Reify(x,u,es) if addControlDeps => syms(x) ++ nonControlSyms(es, syms)
  case _ => super.syms(e)
 }

 override def rsyms[T](e: Any)(f: Any => Vector[T]): Vector[T] = e match { // stack overflow ...
  case s: Summary => Vector.empty // don't count effect summaries as dependencies!
  case _ => super.rsyms(e)(f)
 }

 override def symsFreq(e: Any): Vector[(Exp[_], Double)] = e match {
  case s: Summary => Vector.empty // don't count effect summaries as dependencies!
  // enable DCE of reflect nodes if they are only control dependencies
  case Reflect(x,u,es) if addControlDeps => symsFreq(x) ++ nonControlSyms(es, symsFreq)
  case Reify(x,u,es) if addControlDeps => symsFreq(x) ++ nonControlSyms(es, symsFreq)
  case _ => super.symsFreq(e)
 }

 override def effectSyms(x: Any): Vector[Exp[_]] = x match {
  case Def(Reify(y, u, es)) => es
  case _ => super.effectSyms(x)
 }



 /*
   decisions to be made:
   1) does alias imply read? or are they separate?
   2) use a data structure to track transitive aliasing or recompute always?
 */










 import scala.collection._
 /*
   def allTransitiveAliases(start: Any): List[TP[Any]] = {
     def deps(st: Vector[Exp[_]]): List[TP[Any]] = {
       val st1 = st filterNot (s => isPrimitiveType(s.tp))
       globalDefs.filter(st1 contains _.sym)
     }
     GraphUtil.stronglyConnectedComponents[TP[Any]](deps(aliasSyms(start)), t => deps(aliasSyms(t.rhs))).flatten.reverse
   }
 */












 //def allTransitiveAliases(start: Any): List[Stm] = utilLoadStms(allAliases(start))
 //def transitiveAliases(start: Vector[Exp[_]]): List[Stm] = start.flatMap(utilLoadSymTP)








 // --- reflect

 // REMARK: making toAtom context-dependent is quite a departure from the
 // earlier design. there are a number of implications especially for mirroring.

 /*
   wrapping reads in a reflect can also have an unfortunate effect on rewritings.
   consider
     val a = ...       // mutable
     val b = a.foo     // usually Foo(a) but now Reflect(Foo(a))
     val c = b.costly  // costly(Foo(a)) would simplify to Cheap(a),
                       // but this ends up as Reflect(Costly(Reflect(Foo(a)))) instead of Reflect(Cheap(a))

   of course this is unsafe in general but there might be cases that are definitely save.
 */

 protected override implicit def toAtom[T:TypeRep](d: Def[T])(implicit pos: SourceContext): Exp[T] = {
  /*
      are we depending on a variable or mutable object? then we need to be serialized -> effect

      the call chain goes like this:

        toAtom
        reflectEffect(Pure())      // figure out dependencies on mutable objects
        reflectEffectInternal(u)   // extended summary Pure() -> u
          super.toAtom             // if summary is still pure
          createReflectDefinition  // if summary is not pure
  */
  // warn if type is Any. TODO: make optional, sometimes Exp[Any] is fine

  if (typeRep[T] == typeRep[Any]) printlog("warning: possible missing mtype call - toAtom with Def of type Any " + d)

  // AKS NOTE: this was removed on 6/27/12, but it is still a problem in OptiML apps without it,
  // so I'm putting it back until we can get it resolved properly.
  d match {
   case Reify(x,_,_) =>
    // aks: this became a problem after adding global mutable vars to the read deps list. what is the proper way of handling this?
    // specifically, if we return the reified version of a mutable bound var, we get a Reflect(Reify(..)) error, e.g. mutable Sum
    // printlog("ignoring read of Reify(): " + d)
    super.toAtom(d)
   //case _ if conditionalScope && addControlDeps => reflectEffect(d, Control()) //GO: RF!
   case _ => reflectEffect(d, Pure())
  }
  // reflectEffect(d, Pure())
 }

 def reflectMirrored[A:Manifest](zd: Reflect[A])(implicit pos: SourceContext): Exp[A] = ??? /*{
  checkContext()
  // warn if type is Any. TODO: make optional, sometimes Exp[Any] is fine
  if (manifest[A] == manifest[Any]) printlog("warning: possible missing mtype call - reflectMirrored with Def of type Any: " + zd)
  context.filter { case Def(d) if d == zd => true case _ => false }.reverse match {
   //case z::_ => z.asInstanceOf[Exp[A]]  -- unsafe: we don't have a tight context, so we might pick one from a flattened subcontext
   case _ => createReflectDefinition(fresh[A].withPos(Vector(pos)), zd)
  }
 }*/







 def reflectMutableExp[A](s: Exp[A]): Exp[A] = {
  assert(findDefinition(s).isEmpty)
  globalMutableSyms = globalMutableSyms :+ s
  s
 }













 // --- reify


 def pruneContext(ctx: Vector[Exp[_]]): Vector[Exp[_]] = ??? // ctx // TODO this doesn't work yet (because of loops!): filterNot { case Def(Reflect(_,u,_)) => mustOnlyRead(u) }

 // reify the effects of an isolated block.
 // no assumptions about the current context remain valid.
 def reifyEffects[A:Manifest](block: => Exp[A], controlScope: Boolean = false): Block = {
  val (result, defs) = reifySubGraph(block)
  reflectSubGraph(defs)
  val deps: Vector[Exp[_]] = ???  //this should be all expressions that got created during reify and reflect
  val summary = summarizeAll(deps)
  if (deps.isEmpty && mustPure(summary)) Block(Vector(result)) else Block(Vector(Reify(result, summary, pruneContext(deps)))) // calls toAtom...
 }

 // reify the effects of a block that is executed 'here' (if it is executed at all).
 // all assumptions about the current context carry over unchanged.
 def reifyEffectsHere[A:TypeRep](block: => Exp[A], controlScope: Boolean = false): Block = {
  ???
  /*val save = context
  if (save eq null)
   context = Vector.empty

  val saveControl = conditionalScope
  conditionalScope = controlScope

  val (result, defs) = reifySubGraph(block)
  reflectSubGraph(defs)

  conditionalScope = saveControl

  if ((save ne null) && context.take(save.length) != save) // TODO: use splitAt
   printerr("error: 'here' effects must leave outer information intact: " + save + " is not a prefix of " + context)

  val deps = if (save eq null) context else context.drop(save.length)

  val summary = summarizeAll(deps)
  context = save

  if (deps.isEmpty && mustPure(summary)) Block(Vector(result)) else Block(Vector(Reify(result, summary, pruneContext(deps)))) // calls toAtom...*/
 }

 // --- bookkeping

 override def reset = {
  shallowAliasCache.clear()
  deepAliasCache.clear()
  allAliasCache.clear()
  globalMutableSyms = Vector.empty
  super.reset
 }*/

