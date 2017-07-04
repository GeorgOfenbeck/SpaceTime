package PaperExamples

/**
  * Created by rayda on 13-Nov-16.
  */
trait CloseHeader extends Skeleton{


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

  abstract class Base(a: ST[Int], b: ST[Int], c: ST[Int])

  abstract class Header(val a: ST[Int], val b: ST[Int], val c: ST[Int]) extends Base(a, b, c) with RepSelector2 {
    def ga() = repselect(a)

    def gb() = repselect(b)

    def gc() = repselect(c)
  }

  class StatHeader(a: ST[Int], b: ST[Int], c: ST[Int]) extends Header(a, b, c) with StatSelector2 {
    private def help(oneEntry: EY): String = {
      val t = repselect(oneEntry)
      t.a match {
        case Some(x: NoRep[t.T]) => t.evnum.toLong(x).toString
        case _ => ""
      }
    }

    def genSig(): String = {
      val as = help(a)
      val bs = help(b)
      val cs = help(c)
      s"a${as}b${bs}c${cs}"
    }
  }

  class DynHeader(a: ST[Int], b: ST[Int], c: ST[Int]) extends Header(a, b, c) with DynSelector2

  case class MixHeader(a: ST[Int], b: ST[Int], c: ST[Int]) extends Base(a, b, c) {
    def getDynHeader(): DynHeader = new DynHeader(a, b, c)

    def getStatHeader(): StatHeader = new StatHeader(a, b, c)

    def split(): (StatHeader, DynHeader) = (getStatHeader(), getDynHeader())
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


    def apply(hs: StatHeader, hd: DynHeader): MixHeader = {
      val a = hs.ga().toOneEntry().getOrElse(hd.ga().toOneEntry().get)
      val b = hs.gb().toOneEntry().getOrElse(hd.gb().toOneEntry().get)
      val c = hs.gc().toOneEntry().getOrElse(hd.gc().toOneEntry().get)
      new MixHeader(a.asInstanceOf[ST[Int]], b.asInstanceOf[ST[Int]], c.asInstanceOf[ST[Int]])
    }
  }

  implicit def exposeDynHeader(stat: StatHeader): ExposeRep[DynHeader] = new ExposeRep[DynHeader] {
    val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => {
      val t = stat.a.ev.fresh[stat.a.T]()(stat.a.evtyp) ++
        stat.b.ev.fresh[stat.b.T]()(stat.b.evtyp) ++
        stat.c.ev.fresh[stat.c.T]()(stat.c.evtyp)
      t
    }
    val t2vec: DynHeader => Vector[Exp[_]] = (in: DynHeader) => ???
    val vec2t: Vector[Exp[_]] => DynHeader = (in: Vector[Exp[_]]) => ???
  }

}
