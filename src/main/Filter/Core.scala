

package Filter

import org.scala_lang.virtualized.SourceContext

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._
import Filter2._


class Core extends FilterHeader {
  self =>
  val emitGraph = new GraphVizExport {
    override val IR: self.type = self
  }
  override val codegen = new ScalaCodegen with EmitHeadInternalFunctionAsClass with ScalaGenPrimitivOps with Filter2.ScalaGenSort_DSL with ScalaGenBooleanOps with ScalaGenIfThenElse {
    val IR: self.type = self
  }



  case class MaybeSFunction[T: Numeric : TypeRep, A[_] : IRep, B[_] : IRep, C[_] : IRep, D[_] : IRep, E[_] : IRep, F[_] : IRep, G[_] : IRep, H[_] : IRep, I[_] : IRep]
  (f: Either[StagedFunction[DynFilterHeader[T, A, B, C, D, E, F, G, H, I], Rep[ImageH]], DynFilterHeader[T, A, B, C, D, E, F, G, H, I] => Rep[ImageH]]) {
    def apply(dyn: DynFilterHeader[T, A, B, C, D, E, F, G, H, I]): Rep[ImageH] = f.fold(fa => fa(dyn), fb => fb(dyn))
  }

  object MaybeSFunction {
    def apply[T: Numeric : TypeRep, A[_] : IRep, B[_] : IRep, C[_] : IRep, D[_] : IRep, E[_] : IRep, F[_] : IRep, G[_] : IRep, H[_] : IRep, I[_] : IRep](f: StagedFunction[DynFilterHeader[T, A, B, C, D, E, F, G, H, I], Rep[ImageH]]): MaybeSFunction[T, A, B, C, D, E, F, G, H, I] = MaybeSFunction(Left(f))

    def apply[T: Numeric : TypeRep, A[_] : IRep, B[_] : IRep, C[_] : IRep, D[_] : IRep, E[_] : IRep, F[_] : IRep, G[_] : IRep, H[_] : IRep, I[_] : IRep](f: (DynFilterHeader[T, A, B, C, D, E, F, G, H, I] => Rep[ImageH])): MaybeSFunction[T, A, B, C, D, E, F, G, H, I] = MaybeSFunction(Right(f))
  }

  def multiplya[T: Numeric : TypeRep, A[_] : IRep, B[_] : IRep, C[_] : IRep, D[_] : IRep, E[_] : IRep, F[_] : IRep, G[_] : IRep, H[_] : IRep, I[_] : IRep](stat: StatFilterHeader[T, A, B, C, D, E, F, G, H, I]): (DynFilterHeader[T, A, B, C, D, E, F, G, H, I] => Rep[ImageH]) = {
    val stageme: (DynFilterHeader[T, A, B, C, D, E, F, G, H, I] => Rep[ImageH]) = (dyn: DynFilterHeader[T, A, B, C, D, E, F, G, H, I]) => {
      val f = multiply(stat)
      val t = f(dyn)
      t
    }
    stageme
  }


  def multiply[T: Numeric : TypeRep, A[_] : IRep, B[_] : IRep, C[_] : IRep, D[_] : IRep, E[_] : IRep, F[_] : IRep, G[_] : IRep, H[_] : IRep, I[_] : IRep](stat: StatFilterHeader[T, A, B, C, D, E, F, G, H, I]): MaybeSFunction[T, A, B, C, D, E, F, G, H, I] = {
    val exposarg: ExposeRep[DynFilterHeader[T, A, B, C, D, E, F, G, H, I]] = exposeDynHeader(stat)
    implicit val exposeret = exposeRepFromRep[ImageH]
    val stageme: (DynFilterHeader[T, A, B, C, D, E, F, G, H, I] => Rep[ImageH]) = (dyn: DynFilterHeader[T, A, B, C, D, E, F, G, H, I]) => {

      val mix = MixFilterHeader(stat, dyn)
      import mix._

      val evnum = implicitly[Numeric[T]]
      val ttr = implicitly[TypeRep[T]]
      implicit val tmf = ttr.mf
/*

      if (inline.specialize) {
        inline.spezialize_done match {
          case 0 =>{
            val eva = implicitly[IRep[A]]
            val one = eva.const(evnum.fromInt(1))
            val cond1: A[Boolean] = eva.equiv(a, one)
            eva._if(cond1, {
              val newmix: MixFilterHeader[T, NoRep, B, C, D, E, F, G, H, I] = new MixFilterHeader[T, NoRep, B, C, D, E, F, G, H, I](image_in, image_out, evnum.fromInt(1), b, c, d, e, f, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            }, {
              val cond: A[Boolean] = eva.equiv(a, eva.const[T](evnum.zero))
              eva._if(cond, {
                val newmix: MixFilterHeader[T, NoRep, B, C, D, E, F, G, H, I] = new MixFilterHeader[T, NoRep, B, C, D, E, F, G, H, I](image_in, image_out, evnum.zero, b, c, d, e, f, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
                val (nstat, ndyn) = newmix.split()
                val fs = multiply(nstat)
                fs(ndyn)
              }, {
                val newmix: MixFilterHeader[T, A, B, C, D, E, F, G, H, I] = new MixFilterHeader[T, A, B, C, D, E, F, G, H, I](image_in, image_out, a, b, c, d, e, f, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
                val (nstat, ndyn) = newmix.split()
                val fs = multiply(nstat)
                fs(ndyn)
              })
            })
          }
          case 1 => {
            val eva = implicitly[IRep[B]]
            val cond: B[Boolean] = eva.equiv(b, eva.const[T](evnum.zero))
            eva._if(cond, {
              val newmix: MixFilterHeader[T, A, NoRep, C, D, E, F, G, H, I] = new MixFilterHeader[T, A, NoRep, C, D, E, F, G, H, I](image_in, image_out, a, evnum.zero, c, d, e, f, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            }, {
              val newmix: MixFilterHeader[T, A, B, C, D, E, F, G, H, I] = new MixFilterHeader[T, A, B, C, D, E, F, G, H, I](image_in, image_out, a, b, c, d, e, f, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            })
          }
          case 2 => {
            val eva = implicitly[IRep[C]]
            val cond: C[Boolean] = eva.equiv(c, eva.const[T](evnum.zero))
            eva._if(cond, {
              val newmix: MixFilterHeader[T, A, B, NoRep, D, E, F, G, H, I] = new MixFilterHeader[T, A, B, NoRep, D, E, F, G, H, I](image_in, image_out, a, b, evnum.zero, d, e, f, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            }, {
              val newmix: MixFilterHeader[T, A, B, C, D, E, F, G, H, I] = new MixFilterHeader[T, A, B, C, D, E, F, G, H, I](image_in, image_out, a, b, c, d, e, f, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            })
          }
          case 3 => {
            val eva = implicitly[IRep[D]]
            val cond: D[Boolean] = eva.equiv(d, eva.const[T](evnum.zero))
            eva._if(cond, {
              val newmix: MixFilterHeader[T, A, B, C, NoRep, E, F, G, H, I] = new MixFilterHeader[T, A, B, C, NoRep, E, F, G, H, I](image_in, image_out, a, b, c, evnum.zero, e, f, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            }, {
              val newmix: MixFilterHeader[T, A, B, C, D, E, F, G, H, I] = new MixFilterHeader[T, A, B, C, D, E, F, G, H, I](image_in, image_out, a, b, c, d, e, f, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            })
          }
          case 4 => {
            val eva = implicitly[IRep[E]]
            val cond: E[Boolean] = eva.equiv(e, eva.const[T](evnum.zero))
            eva._if(cond, {
              val newmix: MixFilterHeader[T, A, B, C, D, NoRep, F, G, H, I] = new MixFilterHeader[T, A, B, C, D, NoRep, F, G, H, I](image_in, image_out, a, b, c, d, evnum.zero, f, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            }, {
              val newmix: MixFilterHeader[T, A, B, C, D, E, F, G, H, I] = new MixFilterHeader[T, A, B, C, D, E, F, G, H, I](image_in, image_out, a, b, c, d, e, f, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            })
          }
          case 5 => {
            val eva = implicitly[IRep[F]]
            val cond: F[Boolean] = eva.equiv(f, eva.const[T](evnum.zero))
            eva._if(cond, {
              val newmix: MixFilterHeader[T, A, B, C, D, E, NoRep, G, H, I] = new MixFilterHeader[T, A, B, C, D, E, NoRep, G, H, I](image_in, image_out, a, b, c, d, e, evnum.zero, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            }, {
              val newmix: MixFilterHeader[T, A, B, C, D, E, F, G, H, I] = new MixFilterHeader[T, A, B, C, D, E, F, G, H, I](image_in, image_out, a, b, c, d, e, f, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            })
          }
          case 6 => {
            val eva = implicitly[IRep[G]]
            val cond: G[Boolean] = eva.equiv(g, eva.const[T](evnum.zero))
            eva._if(cond, {
              val newmix: MixFilterHeader[T, A, B, C, D, E, F, NoRep, H, I] = new MixFilterHeader[T, A, B, C, D, E, F, NoRep, H, I](image_in, image_out, a, b, c, d, e, f, evnum.zero, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            }, {
              val newmix: MixFilterHeader[T, A, B, C, D, E, F, G, H, I] = new MixFilterHeader[T, A, B, C, D, E, F, G, H, I](image_in, image_out, a, b, c, d, e, f, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            })
          }
          case 7 => {
            val eva = implicitly[IRep[H]]
            val cond: H[Boolean] = eva.equiv(h, eva.const[T](evnum.zero))
            eva._if(cond, {
              val newmix: MixFilterHeader[T, A, B, C, D, E, F, G, NoRep, I] = new MixFilterHeader[T, A, B, C, D, E, F, G, NoRep, I](image_in, image_out, a, b, c, d, e, f, g, evnum.zero, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            }, {
              val newmix: MixFilterHeader[T, A, B, C, D, E, F, G, H, I] = new MixFilterHeader[T, A, B, C, D, E, F, G, H, I](image_in, image_out, a, b, c, d, e, f, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            })
          }
          case 8 => {
            val eva = implicitly[IRep[I]]
            val cond: I[Boolean] = eva.equiv(i, eva.const[T](evnum.zero))
            eva._if(cond, {
              val newmix: MixFilterHeader[T, A, B, C, D, E, F, G, H, NoRep] = new MixFilterHeader[T, A, B, C, D, E, F, G, H, NoRep](image_in, image_out, a, b, c, d, e, f, g, h, evnum.zero, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            }, {
              val newmix: MixFilterHeader[T, A, B, C, D, E, F, G, H, I] = new MixFilterHeader[T, A, B, C, D, E, F, G, H, I](image_in, image_out, a, b, c, d, e, f, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            })
          }
          case _ => {
            val newmix: MixFilterHeader[T, A, B, C, D, E, F, G, H, I] = new MixFilterHeader[T, A, B, C, D, E, F, G, H, I](image_in, image_out, a, b, c, d, e, f, g, h, i, inline.copy(specialize = false))
            val (nstat, ndyn) = newmix.split()
            val fs = multiply(nstat)
            fs(ndyn)
          }
        }

      } else {
        val ii: Rep[Int] = Const(0)
        val jj: Rep[Int] = Const(0)

        val ina = getImage(image_in, ii, jj)
        val inb = getImage(image_in, ii + 1, jj)
        val inc = getImage(image_in, ii + 2, jj)
        val ind = getImage(image_in, ii, jj + 1)
        val ine = getImage(image_in, ii + 1, jj + 1)
        val inf = getImage(image_in, ii + 2, jj + 1)
        val ing = getImage(image_in, ii, jj + 2)
        val inh = getImage(image_in, ii + 1, jj + 2)
        val ini = getImage(image_in, ii + 2, jj + 2)

        val a1 = gentimes(implicitly[IRep[A]].toRep(a), ina)
        val b1 = gentimes(implicitly[IRep[B]].toRep(b), inb)
        val c1 = gentimes(implicitly[IRep[C]].toRep(c), inc)
        val d1 = gentimes(implicitly[IRep[D]].toRep(d), ind)
        val e1 = gentimes(implicitly[IRep[E]].toRep(e), ine)
        val f1 = gentimes(implicitly[IRep[F]].toRep(f), inf)
        val g1 = gentimes(implicitly[IRep[G]].toRep(g), ing)
        val h1 = gentimes(implicitly[IRep[H]].toRep(h), inh)
        val i1 = gentimes(implicitly[IRep[I]].toRep(i), ini)

        val sum = Vector(a1, b1, c1, d1, e1, f1, g1, h1, i1).foldLeft(Const(evnum.zero))((acc, ele) => genplus[T](acc, ele))
        val out = setImage(image_out, ii, jj, sum)

        out
      }
*/
      mix.image_in


    }
    if (stat.inline.inline) {
      MaybeSFunction(stageme)
    } else {
      val t: StagedFunction[DynFilterHeader[T, A, B, C, D, E, F, G, H, I], Rep[ImageH]] = doGlobalLambda(stageme, Some("Filter" + stat.genSig()), Some("Filter" + stat.genSig()))(exposarg, exposeret)
      MaybeSFunction(t)
    }
  }

  /****
    *
    * @param stat
    * @tparam T
    * @tparam A
    * @tparam B
    * @tparam C
    * @tparam D
    * @tparam E
    * @tparam F
    * @tparam G
    * @tparam H
    * @tparam I
    * @return
    */

  def multiply_dec[T: Numeric : TypeRep, A[_] : IRep, B[_] : IRep, C[_] : IRep, D[_] : IRep, E[_] : IRep, F[_] : IRep, G[_] : IRep, H[_] : IRep, I[_] : IRep](stat: StatFilterHeader[T, A, B, C, D, E, F, G, H, I]): MaybeSFunction[T, A, B, C, D, E, F, G, H, I] = {
    val exposarg: ExposeRep[DynFilterHeader[T, A, B, C, D, E, F, G, H, I]] = exposeDynHeader(stat)
    implicit val exposeret = exposeRepFromRep[ImageH]
    val stageme: (DynFilterHeader[T, A, B, C, D, E, F, G, H, I] => Rep[ImageH]) = (dyn: DynFilterHeader[T, A, B, C, D, E, F, G, H, I]) => {

      val mix = MixFilterHeader(stat, dyn)
      import mix._

      val evnum = implicitly[Numeric[T]]
      val ttr = implicitly[TypeRep[T]]
      implicit val tmf = ttr.mf

      if (inline.specialize) {
        inline.spezialize_done match {
          case 0 => {
            val eva = implicitly[IRep[A]]
            val one = eva.const(evnum.fromInt(1))
            val cond1: A[Boolean] = eva.equiv(a, one)
            eva._if(cond1, {
              val newmix: MixFilterHeader[T, NoRep, B, C, D, E, F, G, H, I] = new MixFilterHeader[T, NoRep, B, C, D, E, F, G, H, I](image_in, image_out, evnum.fromInt(1), b, c, d, e, f, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            }, {
            val cond: A[Boolean] = eva.equiv(a, eva.const[T](evnum.zero))
            eva._if(cond, {
              val newmix: MixFilterHeader[T, NoRep, B, C, D, E, F, G, H, I] = new MixFilterHeader[T, NoRep, B, C, D, E, F, G, H, I](image_in, image_out, evnum.zero, b, c, d, e, f, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            }, {
              val newmix: MixFilterHeader[T, A, B, C, D, E, F, G, H, I] = new MixFilterHeader[T, A, B, C, D, E, F, G, H, I](image_in, image_out, a, b, c, d, e, f, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            })
            })
          }
          case 1 => {
            val eva = implicitly[IRep[B]]
            val cond: B[Boolean] = eva.equiv(b, eva.const[T](evnum.zero))
            eva._if(cond, {
              val newmix: MixFilterHeader[T, A, NoRep, C, D, E, F, G, H, I] = new MixFilterHeader[T, A, NoRep, C, D, E, F, G, H, I](image_in, image_out, a, evnum.zero, c, d, e, f, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            }, {
              val newmix: MixFilterHeader[T, A, B, C, D, E, F, G, H, I] = new MixFilterHeader[T, A, B, C, D, E, F, G, H, I](image_in, image_out, a, b, c, d, e, f, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            })
          }
          case 2 => {
            val eva = implicitly[IRep[C]]
            val cond: C[Boolean] = eva.equiv(c, eva.const[T](evnum.zero))
            eva._if(cond, {
              val newmix: MixFilterHeader[T, A, B, NoRep, D, E, F, G, H, I] = new MixFilterHeader[T, A, B, NoRep, D, E, F, G, H, I](image_in, image_out, a, b, evnum.zero, d, e, f, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            }, {
              val newmix: MixFilterHeader[T, A, B, C, D, E, F, G, H, I] = new MixFilterHeader[T, A, B, C, D, E, F, G, H, I](image_in, image_out, a, b, c, d, e, f, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            })
          }
          case 3 => {
            val eva = implicitly[IRep[D]]
            val cond: D[Boolean] = eva.equiv(d, eva.const[T](evnum.zero))
            eva._if(cond, {
              val newmix: MixFilterHeader[T, A, B, C, NoRep, E, F, G, H, I] = new MixFilterHeader[T, A, B, C, NoRep, E, F, G, H, I](image_in, image_out, a, b, c, evnum.zero, e, f, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            }, {
              val newmix: MixFilterHeader[T, A, B, C, D, E, F, G, H, I] = new MixFilterHeader[T, A, B, C, D, E, F, G, H, I](image_in, image_out, a, b, c, d, e, f, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            })
          }
          case 4 => {
            val eva = implicitly[IRep[E]]
            val cond: E[Boolean] = eva.equiv(e, eva.const[T](evnum.zero))
            eva._if(cond, {
              val newmix: MixFilterHeader[T, A, B, C, D, NoRep, F, G, H, I] = new MixFilterHeader[T, A, B, C, D, NoRep, F, G, H, I](image_in, image_out, a, b, c, d, evnum.zero, f, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            }, {
              val newmix: MixFilterHeader[T, A, B, C, D, E, F, G, H, I] = new MixFilterHeader[T, A, B, C, D, E, F, G, H, I](image_in, image_out, a, b, c, d, e, f, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            })
          }
          case 5 => {
            val eva = implicitly[IRep[F]]
            val cond: F[Boolean] = eva.equiv(f, eva.const[T](evnum.zero))
            eva._if(cond, {
              val newmix: MixFilterHeader[T, A, B, C, D, E, NoRep, G, H, I] = new MixFilterHeader[T, A, B, C, D, E, NoRep, G, H, I](image_in, image_out, a, b, c, d, e, evnum.zero, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            }, {
              val newmix: MixFilterHeader[T, A, B, C, D, E, F, G, H, I] = new MixFilterHeader[T, A, B, C, D, E, F, G, H, I](image_in, image_out, a, b, c, d, e, f, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            })
          }
          case 6 => {
            val eva = implicitly[IRep[G]]
            val cond: G[Boolean] = eva.equiv(g, eva.const[T](evnum.zero))
            eva._if(cond, {
              val newmix: MixFilterHeader[T, A, B, C, D, E, F, NoRep, H, I] = new MixFilterHeader[T, A, B, C, D, E, F, NoRep, H, I](image_in, image_out, a, b, c, d, e, f, evnum.zero, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            }, {
              val newmix: MixFilterHeader[T, A, B, C, D, E, F, G, H, I] = new MixFilterHeader[T, A, B, C, D, E, F, G, H, I](image_in, image_out, a, b, c, d, e, f, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            })
          }
          case 7 => {
            val eva = implicitly[IRep[H]]
            val cond: H[Boolean] = eva.equiv(h, eva.const[T](evnum.zero))
            eva._if(cond, {
              val newmix: MixFilterHeader[T, A, B, C, D, E, F, G, NoRep, I] = new MixFilterHeader[T, A, B, C, D, E, F, G, NoRep, I](image_in, image_out, a, b, c, d, e, f, g, evnum.zero, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            }, {
              val newmix: MixFilterHeader[T, A, B, C, D, E, F, G, H, I] = new MixFilterHeader[T, A, B, C, D, E, F, G, H, I](image_in, image_out, a, b, c, d, e, f, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            })
          }
          case 8 => {
            val eva = implicitly[IRep[I]]
            val cond: I[Boolean] = eva.equiv(i, eva.const[T](evnum.zero))
            eva._if(cond, {
              val newmix: MixFilterHeader[T, A, B, C, D, E, F, G, H, NoRep] = new MixFilterHeader[T, A, B, C, D, E, F, G, H, NoRep](image_in, image_out, a, b, c, d, e, f, g, h, evnum.zero, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            }, {
              val newmix: MixFilterHeader[T, A, B, C, D, E, F, G, H, I] = new MixFilterHeader[T, A, B, C, D, E, F, G, H, I](image_in, image_out, a, b, c, d, e, f, g, h, i, inline.copy(spezialize_done = inline.spezialize_done + 1))
              val (nstat, ndyn) = newmix.split()
              val fs = multiply(nstat)
              fs(ndyn)
            })
          }
          case _ => {
            val newmix: MixFilterHeader[T, A, B, C, D, E, F, G, H, I] = new MixFilterHeader[T, A, B, C, D, E, F, G, H, I](image_in, image_out, a, b, c, d, e, f, g, h, i, inline.copy(specialize = false))
            val (nstat, ndyn) = newmix.split()
            val fs = multiply(nstat)
            fs(ndyn)
          }
        }

      } else {
        val ii: Rep[Int] = Const(0)
        val jj: Rep[Int] = Const(0)

        val ina = getImage(image_in, ii, jj)
        val inb = getImage(image_in, ii + 1, jj)
        val inc = getImage(image_in, ii + 2, jj)
        val ind = getImage(image_in, ii, jj + 1)
        val ine = getImage(image_in, ii + 1, jj + 1)
        val inf = getImage(image_in, ii + 2, jj + 1)
        val ing = getImage(image_in, ii, jj + 2)
        val inh = getImage(image_in, ii + 1, jj + 2)
        val ini = getImage(image_in, ii + 2, jj + 2)

        val a1 = gentimes(implicitly[IRep[A]].toRep(a), ina)
        val b1 = gentimes(implicitly[IRep[B]].toRep(b), inb)
        val c1 = gentimes(implicitly[IRep[C]].toRep(c), inc)
        val d1 = gentimes(implicitly[IRep[D]].toRep(d), ind)
        val e1 = gentimes(implicitly[IRep[E]].toRep(e), ine)
        val f1 = gentimes(implicitly[IRep[F]].toRep(f), inf)
        val g1 = gentimes(implicitly[IRep[G]].toRep(g), ing)
        val h1 = gentimes(implicitly[IRep[H]].toRep(h), inh)
        val i1 = gentimes(implicitly[IRep[I]].toRep(i), ini)

        val sum = Vector(a1, b1, c1, d1, e1, f1, g1, h1, i1).foldLeft(Const(evnum.zero))((acc, ele) => genplus[T](acc, ele))
        val out = setImage(image_out, ii, jj, sum)

        out
      }


    }
    if (stat.inline.inline) {
      MaybeSFunction(stageme)
    } else {
      val t: StagedFunction[DynFilterHeader[T, A, B, C, D, E, F, G, H, I], Rep[ImageH]] = doGlobalLambda(stageme, Some("Filter" + stat.genSig()), Some("Filter" + stat.genSig()))(exposarg, exposeret)
      MaybeSFunction(t)
    }
  }



  def codeexport() = {
    val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\TestFilter.scala"))
    stream2.println(codefrag)
    val inline = InlineInfo(false, 10, true, false, true, 0)
    val ini = StatFilterHeader[Int, Rep, Rep, Rep, Rep, Rep, Rep, Rep, Rep, Rep](Const(-1), Const(-1), Const(-1), Const(-1), Const(-1), Const(-1), Const(-1), Const(-1), Const(-1), inline)
    //val ini: StatKaratsubaHeader[Rep,Rep,Rep,Rep,Rep,Rep] = StatKaratsubaHeader[Rep,Rep,Rep,Rep,Rep,Rep](Const(-1), Const(-1), Const(-1),Const(-1))
    val esc = codegen.emitSource(multiplya(ini), "testClass", stream2)(exposeDynHeader(ini), exposeRepFromRep[ImageH])
    stream2.println("\n}\n")
    stream2.flush()
    stream2.close()
  }

  val codefrag: String = "\npackage Filter\n\nclass Image(val ele: Int){\n  def get(x: Int, y: Int): Int = ele\n  def set(x: Int, y: Int, p: Int) = this\n}\n"

}



