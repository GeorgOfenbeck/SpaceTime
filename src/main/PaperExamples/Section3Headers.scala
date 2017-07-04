package PaperExamples

trait Section3Headers extends Section3AbstractData{


  abstract class EY {
    self =>
    type T
    type A[_]
    val a: A[T]
    val ev: IRep[A]
    val evnum: Numeric[T]
    val evtyp: TypeRep[T]

    def cpy[T1: Numeric, A1[_] : IRep](n: A1[T1]) = {
      new EY {
        override type A[X] = A1[X]
        override type T = T1
        override val evnum: Numeric[T1] = ???
        override val ev: IRep[A] = ???
        override val evtyp: TypeRep[T] = ???
        override val a: this.A[this.T] = n
      }
    }

    def makeSome(me: EY): OptionalEntry = {
      new OptionalEntry {
        override type A[X] = me.A[X]
        override type T = me.T
        override val evnum: Numeric[T] = me.evnum
        override val ev: IRep[A] = me.ev
        override val a: Option[A[T]] = Some(me.a)
        override val evtyp = me.evtyp
      }
    }

    def makeNone(me: EY): OptionalEntry = {
      new OptionalEntry {
        override type A[X] = me.A[X]
        override type T = me.T
        override val evnum: Numeric[T] = me.evnum
        override val ev: IRep[A] = me.ev
        override val a: Option[A[T]] = None
        override val evtyp = me.evtyp
      }
    }
  }

  trait RepSelector2 {
    val rrep: Boolean

    def repselect(one: EY): OptionalEntry =
      if (rrep) {
        if (one.ev.isRep()) one.makeSome(one) else one.makeNone(one)
      } else if (one.ev.isRep()) one.makeNone(one) else one.makeSome(one)
  }

  trait StatSelector2 extends RepSelector2 {
    val rrep: Boolean = false
  }

  trait DynSelector2 extends RepSelector2 {
    val rrep: Boolean = true
  }

  abstract class OptionalEntry {
    self =>
    type T
    type A[_]
    val a: Option[A[T]]
    val ev: IRep[A]
    val evnum: Numeric[T]
    val evtyp: TypeRep[T]

    def toOneEntry(): Option[EY] = {
      if (a.isDefined)
        Some(new EY {
          override type A[X] = self.A[X]
          override type T = self.T
          override val evnum: Numeric[T] = self.evnum
          override val ev: IRep[A] = self.ev
          override val a: A[T] = self.a.get
          override val evtyp = self.evtyp
        })
      else None
    }
  }
  type ST[X] = EY {type T = X}

  abstract class Base[R[_]: IRep](a: AC[R], length: ST[Int], scaling: ST[Double])

  abstract class Header[R[_]: IRep](val a: AC[R], val length: ST[Int], val scaling: ST[Double]) extends Base(a, length, scaling) with RepSelector2 {
    def gb() = repselect(length)

    def gc() = repselect(scaling)
  }

  class StatHeader[R[_]: IRep](a: AC[R], length: ST[Int], scaling: ST[Double]) extends Header(a, length, scaling) with StatSelector2 {
    private def help(oneEntry: EY): String = {
      val t = repselect(oneEntry)
      t.a match {
        case Some(x: NoRep[t.T]) => t.evnum.toLong(x).toString
        case _ => ""
      }
    }

    def genSig(): String = {
      val as = a.isscalarized()
      val bs = help(length)
      val cs = help(scaling)
      s"a${as}length${bs}scaling${cs}"
    }
  }
  class DynHeader[R[_]: IRep](a: AC[R], length: ST[Int], scaling: ST[Double]) extends Header(a, length, scaling) with DynSelector2

  case class MixHeader[R[_]: IRep](a: AC[R], length: ST[Int], scaling: ST[Double]) extends Base(a, length, scaling) {
    self =>
    def getDynHeader(): DynHeader[R] = new DynHeader(a, length, scaling)

    def getStatHeader(): StatHeader[R] = new StatHeader(a, length, scaling)

    def split(): (StatHeader[R], DynHeader[R]) = (getStatHeader(), getDynHeader())

    def cpy[R1[_]: IRep](a: AC[R1], length: ST[Int] = self.length, scaling: ST[Double] = self.scaling ): MixHeader[R1] = {
      MixHeader[R1](a,length,scaling)
    }
  }

  object MixHeader {
    private def choose[A[_], T](a: Option[A[T]], b: Option[A[T]], ev: IRep[A]): A[T] = if (ev.isRep()) b.get else a.get
    private def choose[A[_], T](a: Option[A[T]], b: Option[A[T]]) = {
      if (a.isDefined) a.get else b.get
    }
    private def choose[T1, A1[_]](a: EY, b: EY): A1[T1] = {
      if (a.ev.isRep()) a.a else b.a
      ???
    }


    def apply[R[_]: IRep](hs: StatHeader[R], hd: DynHeader[R]): MixHeader[R] = {
      val a: AC[R] = ???
      val b = hs.gb().toOneEntry().getOrElse(hd.gb().toOneEntry().get)
      val c = hs.gc().toOneEntry().getOrElse(hd.gc().toOneEntry().get)
      new MixHeader(a, b.asInstanceOf[ST[Int]], c.asInstanceOf[ST[Double]])
    }
  }

  implicit def exposeDynHeader[R[_]: IRep](stat: StatHeader[R]): ExposeRep[DynHeader[R]] = new ExposeRep[DynHeader[R]] {
    val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => {
      stat.length.ev.fresh[stat.length.T]()(stat.length.evtyp) ++
        stat.scaling.ev.fresh[stat.scaling.T]()(stat.scaling.evtyp) ++
        stat.a.expose.freshExps()

    }
    val t2vec: DynHeader[R] => Vector[Exp[_]] = (in: DynHeader[R]) => {
      def help(ele: Option[EY]): Vector[Exp[_]] = {
        ele.map(p => p.ev.getRep(p.a).map(o => Vector(o)).getOrElse(Vector.empty)).getOrElse(Vector.empty)
      }
      val t = help(in.gb().toOneEntry()) ++
        help(in.gc().toOneEntry()) ++
        in.a.t2vec()
      t
    }
    val vec2t: Vector[Exp[_]] => DynHeader[R] = (in: Vector[Exp[_]]) => {
      def help[T[_], A: TypeRep](in: Vector[Rep[_]], statele: OptionalEntry): (Vector[Rep[_]], EY) = {
        val (vecafter, ele) = statele.ev.fetch[statele.T](in)(statele.evtyp)
        val res = ele.getOrElse(statele.a.get)
        val or = new EY {
          override type A[X] = statele.A[X]
          override type T = statele.T
          override val evnum: Numeric[T] = statele.evnum
          override val ev: IRep[A] = statele.ev
          override val a: A[T] = res
          override val evtyp = statele.evtyp
        }
        (vecafter, or)
      }
      val (ol,l) = help(in,stat.gb)
      val (os,s) = help(in,stat.gc)
      val a = stat.a.vec2t(os)
      new DynHeader[R](a,l.asInstanceOf[EY {type T = Int}],s.asInstanceOf[EY {type T = Double}])
    }
  }







}
