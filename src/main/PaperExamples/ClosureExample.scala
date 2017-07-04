package PaperExamples

import org.scala_lang.virtualized.SourceContext

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._

class ClosureExample extends CloseHeader {
  self =>

  val emitGraph = new GraphVizExport {
    override val IR: self.type = self
  }
  override val codegen = new ScalaCodegen with EmitHeadInternalFunctionAsClass with ScalaGenPrimitivOps with ScalaGenSort_DSL with ScalaGenBooleanOps with ScalaGenIfThenElse {
    val IR: self.type = self
  }

  import Implicits._

  def split(a: Vector[Double]) = a.splitAt(a.length / 2)

  def scale(a: Vector[Double], s: Double) = {
    val lrange = 0 until a.length
    for (idx <- lrange) yield {
      a(idx) * Math.sin((idx + a.length) % 10) * s
    }
    a.zipWithIndex.map(p => {
      val (ele, idx) = p
      ele * Math.sin((idx + a.length) % 10) * s
    })
  }

  def sum(a: Vector[Double]) = {
    (0 until a.length - 1).foldLeft(Vector.empty[Double]) {
      (acc, i) => acc :+ (a(i) + a(i + 1))
    }
  }

  def recursion(a: Vector[Double], s: Double): Vector[Double] = {
    val size = a.length
    if (size < 2) a
    else {
      val scaled = scale(a, s)
      val sumed = sum(scaled)
      val (l, r) = split(sumed)
      recursion(l, s) ++ recursion(r, s)
    }
  }


  //-------------------- staged version
  type SV = Rep[Array[Double]]

  def staged_split(a: SV): (SV, SV) = ???

  def staged_scale(a: SV, s: Rep[Double]): SV = ???

  def staged_sum(a: SV): SV = ???

  def staged_recursion(a: SV, s: Rep[Double]): SV = {
    val size = a.length
    if (true) a
    //if (size < Const(2)) a
    else {
      val scaled = staged_scale(a, s)
      val sumed = staged_sum(scaled)
      val (l, r) = staged_split(sumed)
      staged_recursion(l, s) ++ staged_recursion(r, s)
    }
  }

  //------------------- unrolling
  def scale_unroll[R[_]](a: SV, l: R[Int], s: Rep[Double])(implicit ev: IRep[R]) = {
    /*val lrange: R[Range] = 0 until l
    lrange.map(idx => {
      val t: R[Int] = (idx + l)
      val m: R[Int] = t % 10
      val d: R[Double] = m.toDouble()
      val sx: R[Double] = ev.sin(d)
      val ridx: Rep[Int] = ev.toRep(idx)
      val ax: Rep[Double] = a(ridx)
      ax * s
    }) */
    import ev._
    /*val t = (0 until l).map(i => {
      val t: R[Double] = sin(((i + l) % 10).toDouble())
      a(toRep(i)) * s * toRep(t)
    })*/
  }


  //-------------------- return functions
  def f_staged_split: (SV => (SV, SV)) = staged_split

  def f_staged_scale: (SV, Rep[Double]) => SV = staged_scale

  def f_staged_sum: SV => SV = staged_sum

  def f_staged_recursion: (SV, Rep[Double]) => SV = {
    def inner(a: SV, s: Rep[Double]): SV = {
      val size = a.length
      if (true) a
      //if (size < Const(2)) a
      else {
        val scaled = f_staged_scale(a, s)
        val sumed = f_staged_sum(scaled)
        val (l, r) = f_staged_split(sumed)
        staged_recursion(l, s) ++ staged_recursion(r, s)
      }
    }
    inner
  }
  //-------------------- return maylines
  def mf_staged_sum(inline: Boolean): SV => SV = staged_sum
  case class MayInline[A, R](f: Either[Rep[A] => Rep[R], Rep[A => R]]) {
    class FOp(x: Rep[A => R]){ def apply(arg: Rep[A]): Rep[R] = ???}
    implicit def toapply(x: Rep[A => R]) = new FOp(x)
    def apply(arg: Rep[A]): Rep[R] = f.fold(fa => fa(arg), fb => fb(arg))
  }

  //------------------ abstract type
  abstract class AC[R[_]: IRep,T]{
    val ev = implicitly[IRep[R]]
    import ev._
    def apply(i: R[Int]): T
    def apply_plus1(i: R[Int]) = apply(i + const(1))
    def update(i: R[Int], y: T)
    def length(): R[Int]
    def isscalarized(): Boolean

    def scalarize(size: Int): MetaArrayofScalars
  }
  class StagedArray extends
    AC[Rep,Rep[Double]]{
    val data: Rep[Array[Double]] = ???
    def apply(i: Rep[Int]): Rep[Double] = data(i)
    def update(i: Rep[Int], y: Rep[Double])= { data(i) = y}
    def length(): Rep[Int] = ???
    val isscalarized = false

    def scalarize(size: Int): MetaArrayofScalars = {
      val scalars = new MetaArrayofScalars {
        override val data = new Array[Rep[Double]](length)
      }
      for (i <- 0 until size) scalars(i) = data(Const(i))
      scalars
    }
  }

  class MetaArrayofScalars
    extends AC[NoRep,Rep[Double]] {
    val data: Array[Rep[Double]] = ???
    def apply(i: Int) = data(i)
    def update(i: Int, y: Rep[Double])= { data(i) = y}
    def length(): NoRep[Int] = data.length
    val isscalarized = true
    def scalarize(size: Int) = this
  }

 def staged_grecursion[R[_]: IRep](a: AC[R,Rep[Double]], length: Int, s: Rep[Double]): StagedArray = {
    if (length < 64)
      staged_grecursion[NoRep](a.scalarize(length),length,s)
    else{ ??? }//as before
  }


  //-------------------------------------

  def specialize_recursion[R[_]: IRep, S[_]: IRep](a: AC[R,Rep[Double]], length: Int, s: S[Double]): Rep[Int] = {
    val rev = implicitly[IRep[R]]
    val ev = implicitly[IRep[S]]
    if (ev.isRep()) {

      //
      val bb: S[Boolean] = ev.equiv(s, ev.const(1.0d))
      ev._if(bb, {
        specialize_recursion[R,NoRep](a,length,cNoRep.const(1.0d))(rev,cNoRep)
        Const(1)
      }, { Const(1) }) //return to main
    } else Const(1) //return to main
  }



  def unfold[R[_]: IRep](x: R[Int]): Rep[Int] = {
    val ev = implicitly[IRep[R]]
    if (ev.isRep())
      ev._if(ev.equiv(x,ev.const(64)), {
        binsearch(ev.toRep(x),0,64)(ev)
      }, {
        alg(x)
      })
    else alg(x)
  }
  def alg[R[_]: IRep](x: R[Int]): Rep[Int] = ???



  def binsearch[R[_]: IRep](check: Rep[Int], low: Int, high: Int): Rep[Int] = {
    val ev = implicitly[IRep[R]]
    val mid = low + (high - low)
    if ((high - low) == 0) {
      //we found the base case size
      unfold[NoRep](mid)
    }
    else {
      myifThenElse(check < Const(mid), {
        binsearch(check, low, mid)(ev)
      }, {
        binsearch(check, mid, high)(ev)
      })
    }
  }


  def dncalg[R[_]: IRep](x: R[Int], a: Rep[Array[Int]]): Rep[Array[Int]] = {
    val ev = implicitly[IRep[R]]
    if (ev.isRep)
      ev._if(ev.equiv(x,ev.const(64)), {
        val size = binsearch(ev.toRep(x),0,64)(ev)

        a
      }, {
        //do something use full
        ???
      })
    else
      ???
  }



  def poly_function(stat: StatHeader): StagedFunction[DynHeader, Rep[Int]] = {
    val exposarg: ExposeRep[DynHeader] = exposeDynHeader(stat)
    implicit val exposeret = exposeRepFromRep[Int]
    val stageme: (DynHeader => Rep[Int]) = (dyn: DynHeader) => {
      val mix = MixHeader(stat, dyn)
      import mix._

      val newmix = new MixHeader(b,c,a)
      val (newstatic, newdynamic) = mix.split()
      val recursive_f = poly_function(newstatic)
      recursive_f(newdynamic)
    }
    val function_signature = stat.genSig()
    val t: StagedFunction[DynHeader, Rep[Int]] = doGlobalLambda(stageme, Some("ClosureExample" + stat.genSig()), Some("ClosureExample" + stat.genSig()))(exposarg, exposeret)
    t
  }


}
