package Filter
import Filter2._

/**
  * Created by rayda on 27-Oct-16.
  */
trait FilterHeader extends Filter2.Skeleton {

  case class InlineInfo(inline: Boolean, maxfunctions: Int, compareinline: Boolean, consider_inline: Boolean, specialize: Boolean, spezialize_done: Int)

  abstract class Base[T: Numeric : TypeRep, A[_] : IRep, B[_] : IRep, C[_] : IRep, D[_] : IRep, E[_] : IRep, F[_] : IRep, G[_] : IRep, H[_] : IRep, I[_] : IRep](
                                                                                                                                                                  a: A[T], b: B[T], c: C[T], d: D[T], e: E[T], f: F[T], g: G[T], h: H[T], i: I[T])

  /*eva: IRep[A], evb: IRep[B], evc: IRep[C], evd: IRep[D], eve: IRep[E], evf: IRep[F],
  evg: IRep[G], evh: IRep[H], evi: IRep[I])*/

  abstract class FilterHeader[T: Numeric : TypeRep, A[_] : IRep, B[_] : IRep, C[_] : IRep, D[_] : IRep, E[_] : IRep, F[_] : IRep, G[_] : IRep, H[_] : IRep, I[_] : IRep](a: A[T], b: B[T], c: C[T], d: D[T], e: E[T], f: F[T], g: G[T], h: H[T], i: I[T]) extends Base(a, b, c, d, e, f, g, h, i) with RepSelector {

    def a(): Option[A[T]] = repselect(a, implicitly[IRep[A]])

    def b(): Option[B[T]] = repselect(b, implicitly[IRep[B]])

    def c(): Option[C[T]] = repselect(c, implicitly[IRep[C]])

    def d(): Option[D[T]] = repselect(d, implicitly[IRep[D]])

    def e(): Option[E[T]] = repselect(e, implicitly[IRep[E]])

    def f(): Option[F[T]] = repselect(f, implicitly[IRep[F]])

    def g(): Option[G[T]] = repselect(g, implicitly[IRep[G]])

    def h(): Option[H[T]] = repselect(h, implicitly[IRep[H]])

    def i(): Option[I[T]] = repselect(i, implicitly[IRep[I]])

  }

  class DynFilterHeader[T: Numeric : TypeRep, A[_] : IRep, B[_] : IRep, C[_] : IRep, D[_] : IRep, E[_] : IRep, F[_] : IRep, G[_] : IRep, H[_] : IRep, I[_] : IRep](val image_in: Rep[ImageH], val image_out: Rep[ImageH], a: A[T], b: B[T], c: C[T], d: D[T], e: E[T], f: F[T], g: G[T], h: H[T], i: I[T]) extends FilterHeader(a, b, c, d, e, f, g, h, i) with DynSelector

  class StatFilterHeader[T: Numeric : TypeRep, A[_] : IRep, B[_] : IRep, C[_] : IRep, D[_] : IRep, E[_] : IRep, F[_] : IRep, G[_] : IRep, H[_] : IRep, I[_] : IRep](a: A[T], b: B[T], c: C[T], d: D[T], e: E[T], f: F[T], g: G[T], h: H[T], i: I[T], val inline: InlineInfo) extends FilterHeader(a, b, c, d, e, f, g, h, i) with StatSelector {
    def genSig(): String = {
      val evnum = implicitly[Numeric[T]]
      val as = a() match {
        case Some(x: NoRep[T]) => evnum.toLong(x).toString
        case _ => ""
      }
      val bs = b() match {
        case Some(x: NoRep[T]) => evnum.toLong(x).toString
        case _ => ""
      }
      val cs = c() match {
        case Some(x: NoRep[T]) => evnum.toLong(x).toString
        case _ => ""
      }
      val ds = d() match {
        case Some(x: NoRep[T]) => evnum.toLong(x).toString
        case _ => ""
      }
      val es = e() match {
        case Some(x: NoRep[T]) => evnum.toLong(x).toString
        case _ => ""
      }
      val fs = f() match {
        case Some(x: NoRep[T]) => evnum.toLong(x).toString
        case _ => ""
      }
      val gs = g() match {
        case Some(x: NoRep[T]) => evnum.toLong(x).toString
        case _ => ""
      }
      val hs = h() match {
        case Some(x: NoRep[T]) => evnum.toLong(x).toString
        case _ => ""
      }
      val is = i() match {
        case Some(x: NoRep[T]) => evnum.toLong(x).toString
        case _ => ""
      }
      s"_a${as}b${bs}c${cs}d${ds}e${es}f${fs}g${gs}h${hs}i${is}inline${inline.specialize}${inline.spezialize_done}"
    }
  }

  object StatFilterHeader {
    def apply[T: Numeric : TypeRep, A[_] : IRep, B[_] : IRep, C[_] : IRep, D[_] : IRep, E[_] : IRep, F[_] : IRep, G[_] : IRep, H[_] : IRep, I[_] : IRep](a: A[T], b: B[T], c: C[T], d: D[T], e: E[T], f: F[T], g: G[T], h: H[T], i: I[T], inline: InlineInfo): StatFilterHeader[T, A, B, C, D, E, F, G, H, I] = new StatFilterHeader(a, b, c, d, e, f, g, h, i, inline)
  }

  case class MixFilterHeader[T: Numeric : TypeRep, A[_] : IRep, B[_] : IRep, C[_] : IRep, D[_] : IRep, E[_] : IRep, F[_] : IRep, G[_] : IRep, H[_] : IRep, I[_] : IRep](image_in: Rep[ImageH] , image_out: Rep[ImageH], a: A[T], b: B[T], c: C[T], d: D[T], e: E[T], f: F[T], g: G[T], h: H[T], i: I[T], inline: InlineInfo) extends Base(a, b, c, d, e, f, g, h, i) {
    def getDynHeader(): DynFilterHeader[T, A, B, C, D, E, F, G, H, I] = new DynFilterHeader[T, A, B, C, D, E, F, G, H, I](image_in, image_out,a, b, c, d, e, f, g, h, i)

    def getStatHeader(): StatFilterHeader[T, A, B, C, D, E, F, G, H, I] = new StatFilterHeader[T, A, B, C, D, E, F, G, H, I](a, b, c, d, e, f, g, h, i, inline)

    def split(): (StatFilterHeader[T, A, B, C, D, E, F, G, H, I], DynFilterHeader[T, A, B, C, D, E, F, G, H, I]) = (getStatHeader(), getDynHeader())
  }

  object MixFilterHeader {
    private def choose[A[_], T](a: Option[A[T]], b: Option[A[T]], ev: IRep[A]): A[T] = if (ev.isRep()) b.get else a.get

    def apply[T: Numeric : TypeRep, A[_] : IRep, B[_] : IRep, C[_] : IRep, D[_] : IRep, E[_] : IRep, F[_] : IRep, G[_] : IRep, H[_] : IRep, I[_] : IRep](
                                                                                                                                                          hs: StatFilterHeader[T, A, B, C, D, E, F, G, H, I], hd: DynFilterHeader[T, A, B, C, D, E, F, G, H, I]): MixFilterHeader[T, A, B, C, D, E, F, G, H, I] = {
      val a: A[T] = choose(hs.a(), hd.a(), implicitly[IRep[A]])
      val b: B[T] = choose(hs.b(), hd.b(), implicitly[IRep[B]])
      val c: C[T] = choose(hs.c(), hd.c(), implicitly[IRep[C]])
      val d: D[T] = choose(hs.d(), hd.d(), implicitly[IRep[D]])
      val e: E[T] = choose(hs.e(), hd.e(), implicitly[IRep[E]])
      val f: F[T] = choose(hs.f(), hd.f(), implicitly[IRep[F]])
      val g: G[T] = choose(hs.g(), hd.g(), implicitly[IRep[G]])
      val h: H[T] = choose(hs.h(), hd.h(), implicitly[IRep[H]])
      val i: I[T] = choose(hs.i(), hd.i(), implicitly[IRep[I]])
      new MixFilterHeader[T, A, B, C, D, E, F, G, H, I](hd.image_in,hd.image_out,a, b, c, d, e, f, g, h, i, hs.inline)
    }
  }

  implicit def exposeDynHeader[T: Numeric : TypeRep, A[_] : IRep, B[_] : IRep, C[_] : IRep, D[_] : IRep, E[_] : IRep, F[_] : IRep, G[_] : IRep, H[_] : IRep, I[_] : IRep](stat: StatFilterHeader[T, A, B, C, D, E, F, G, H, I]): ExposeRep[DynFilterHeader[T, A, B, C, D, E, F, G, H, I]] = new ExposeRep[DynFilterHeader[T, A, B, C, D, E, F, G, H, I]]{
    val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => {
      Vector(Arg[ImageH]) ++ Vector(Arg[ImageH]) ++
      implicitly[IRep[A]].fresh[T]() ++
        implicitly[IRep[B]].fresh[T]() ++
        implicitly[IRep[C]].fresh[T]() ++
        implicitly[IRep[D]].fresh[T]() ++
        implicitly[IRep[E]].fresh[T]() ++
        implicitly[IRep[F]].fresh[T]() ++
        implicitly[IRep[G]].fresh[T]() ++
        implicitly[IRep[H]].fresh[T]() ++
        implicitly[IRep[I]].fresh[T]()
    }
    val t2vec: DynFilterHeader[T, A, B, C, D, E, F, G, H, I] => Vector[Exp[_]] = (in: DynFilterHeader[T, A, B, C, D, E, F, G, H, I]) => {
      def help[T[_], A](ele: Option[T[A]], ev: IRep[T]): Vector[Exp[_]] = {
        ele.map(p => ev.getRep(p).map(o => Vector(o)).getOrElse(Vector.empty)).getOrElse(Vector.empty)
      }
      Vector(in.image_in) ++ Vector(in.image_out) ++
      help(in.a(), implicitly[IRep[A]]) ++
        help(in.b(), implicitly[IRep[B]]) ++
        help(in.c(), implicitly[IRep[C]]) ++
        help(in.d(), implicitly[IRep[D]]) ++
        help(in.e(), implicitly[IRep[E]]) ++
        help(in.f(), implicitly[IRep[F]]) ++
        help(in.g(), implicitly[IRep[G]]) ++
        help(in.h(), implicitly[IRep[H]]) ++
        help(in.i(), implicitly[IRep[I]])
    }
    val vec2t: Vector[Exp[_]] => DynFilterHeader[T, A, B, C, D, E, F, G, H, I] = (in: Vector[Exp[_]]) => {
      def help[T[_], A: TypeRep](in: Vector[Rep[_]], statele: Option[T[A]], ev: IRep[T]): (Vector[Rep[_]], T[A]) = {
        val (vecafter, ele) = ev.fetch[A](in)
        val res: T[A] = ele.getOrElse(statele.get)
        (vecafter, res)
      }
      val image_in = in.head.asInstanceOf[Rep[ImageH]]
      val image_out = in.tail.head.asInstanceOf[Rep[ImageH]]
      val (oa, a) = help(in.tail.tail, stat.a, implicitly[IRep[A]])
      val (ob, b) = help(oa, stat.b, implicitly[IRep[B]])
      val (oc, c) = help(ob, stat.c, implicitly[IRep[C]])
      val (od, d) = help(oc, stat.d, implicitly[IRep[D]])
      val (oe, e) = help(od, stat.e, implicitly[IRep[E]])
      val (of, f) = help(oe, stat.f, implicitly[IRep[F]])
      val (og, g) = help(of, stat.g, implicitly[IRep[G]])
      val (oh, h) = help(og, stat.h, implicitly[IRep[H]])
      val (oi, i) = help(oh, stat.i, implicitly[IRep[I]])

      val t: DynFilterHeader[T, A, B, C, D, E, F, G, H, I] =  new DynFilterHeader[T, A, B, C, D, E, F, G, H, I](image_in,image_out,a,b,c,d,e,f,g,h,i)
      t
    }

  }

}
