package PaperExamples

import org.scala_lang.virtualized.SourceContext
import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._

class Section3 extends Section3Headers {
  self =>

  val emitGraph = new GraphVizExport {
    override val IR: self.type = self
  }
  override val codegen = new ScalaCodegen with EmitHeadInternalFunctionAsClass with ScalaGenPrimitivOps with ScalaGenSort_DSL with ScalaGenBooleanOps with ScalaGenIfThenElse {
    val IR: self.type = self
  }

  import Implicits._


  val tiling = 8


  def binsearch[R[_] : IRep](mix: MixHeader[R], check: Rep[Int], low: Int, high: Int): AC[R] = {
    val ev = implicitly[IRep[R]]
    val mid = low + (high - low)
    if ((high - low) == 0) {
      //we found the base case size
      val scalarized = mix.a.scalarize(mid)
      val nmix = mix.cpy(a = scalarized)
      val (nstat, ndyn) = nmix.split()
      val rf = recurse(nstat)
      val result_scalars = rf(ndyn)
      val data: Array[Rep[Double]] = result_scalars.scalarize(result_scalars.length()).data
      mix.a.fromScalars(data)
    }
    else {
      implicit val expose = mix.a.expose
      myifThenElse(check < Const(mid), {
        binsearch(mix, check, low, mid)(ev)
      }, {
        binsearch(mix, check, mid, high)(ev)
      })
    }
  }

  def sizecheck[R[_] : IRep](stat: StatHeader[R]): StagedFunction[DynHeader[R], AC[R]] = {
    val exposarg: ExposeRep[DynHeader[R]] = exposeDynHeader(stat)
    implicit val exposeret = stat.a.expose
    val stageme: (DynHeader[R] => AC[R]) = (dyn: DynHeader[R]) => {
      val mix = MixHeader[R](stat, dyn)
      import mix._
      val ev = implicitly[IRep[R]]

      if (ev.isRep) {
        //only check if its target value
        val bo: R[Boolean] = a.length() < ev.const(64)
        ev._if(bo, {
          binsearch(mix, ev.toRep(a.length()), 0, 64)(ev)
        }, {
          val (nstat, ndyn) = mix.split()
          val rf = recurse(nstat)
          rf(ndyn)
        })
      }
      else {
        val (nstat, ndyn) = mix.split()
        val rf = recurse(nstat)
        rf(ndyn)
      }
    }
    val function_signature = stat.genSig()
    val t: StagedFunction[DynHeader[R], AC[R]] = doGlobalLambda(stageme, Some("SizeCheck" + stat.genSig()), Some("SizeCheck" + stat.genSig()))(exposarg, exposeret)
    t
  }
  
  type Dyn[R[X]] = DynHeader[R]
  type Stat[R[X]] = StatHeader[R]
  type Mix[R[X]] = MixHeader[R]
  
  def recurse[R[_] : IRep](stat: Stat[R]): StagedFunction[Dyn[R], AC[R]] = {
    val (exparg,expret): (ExposeRep[Dyn[R]], ExposeRep[AC[R]]) = ???
    val exposarg: ExposeRep[Dyn[R]] = exposeDynHeader(stat)
    implicit val exposeret = stat.a.expose
    val stageme: (Dyn[R] => AC[R]) = (dyn: Dyn[R]) => {
      val mix = MixHeader[R](stat, dyn)
      import mix._
      val ev = implicitly[IRep[R]]

      //do the scaling
      val scaled = (0 until mix.a.length()).foldLeftx(mix.a)(
        (acc, i) => {
          val t = ev.sin(((i + mix.a.length()) % 10).toDouble())
          acc(i) = mix.a(i) * ev.toRep(t) * mix.scaling.ev.toRep(mix.scaling.a)
          acc
        })

      //tile the suming
      val size = a.length()
      val tiled = (size / tiling) - 1 //border issues
      (0 until tiled).foldLeftx(scaled)(
        (acc, idx_poly) => (0 until tiling).foldLeft(acc)(
          (acc2, idx_meta) => {
            val idx = idx_poly * ev.const(tiling) + ev.const(idx_meta)
            val t1 = acc2(idx)
            val t2 = acc2(idx + ev.const(1))
            acc2(idx) = t1 + t2
            acc2
          }
        )
      )
      //border
      val tiledx = ((tiled * tiling) until size-1).foldLeftx(scaled)(
        (acc, idx) => {
            val t1 = acc(idx)
            val t2 = acc(idx + ev.const(1))
            acc(idx) = t1 + t2
            acc
          }
        )


      val (l, r) = a.splitAt(a.length() / 2)
      val lres = {
        val nmix = mix.cpy(a = l)
        val (nstat, ndyn) = nmix.split()
        val rf = recurse(nstat)
        rf(ndyn)
      }
      val rres = {
        val nmix = mix.cpy(a = r)
        val (nstat, ndyn) = nmix.split()
        val rf = recurse(nstat)
        rf(ndyn)
      }
      lres.concatx(rres)
    }
    val function_signature = stat.genSig()
    val t: StagedFunction[Dyn[R], AC[R]] = doGlobalLambda(stageme, Some("ClosureExample" + stat.genSig()), Some("ClosureExample" + stat.genSig()))(exposarg, exposeret)
    t
  }

  def ini[R[_] : IRep](stat: StatHeader[R]): (DynHeader[R] => AC[R]) = {
    val exposarg: ExposeRep[DynHeader[R]] = exposeDynHeader(stat)
    implicit val exposeret = stat.a.expose
    val stageme: (DynHeader[R] => AC[R]) = (dyn: DynHeader[R]) => {
      val mix = MixHeader[R](stat, dyn)
      import mix._
      val ev = mix.scaling.ev
      val boo = ev.equiv(mix.scaling.a, ev.const(1.0))
      ev._if(boo, {
        val nmix = mix.cpy(a = mix.a, scaling = mix.scaling.cpy[Double, NoRep](1.0d))
        val (nstat, ndyn) = nmix.split()
        val rf = recurse(nstat)
        rf(ndyn)
      }, {
        val (nstat, ndyn) = mix.split()
        val rf = recurse(nstat)
        rf(ndyn)
      })
    }
    stageme
  }
}

