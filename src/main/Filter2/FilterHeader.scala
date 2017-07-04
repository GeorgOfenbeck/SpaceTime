package Filter2
class ImageH


trait FilterHeader extends Skeleton {

  abstract class Matrix {
    val r1: Row
    val r2: Row
    val r3: Row

    def cpy(m: Matrix, a: Row = null, b: Row = null, c: Row = null): Matrix = {
      val a1: Row = if (a == null) m.r1 else a
      val a2: Row = if (b == null) m.r2 else b
      val a3: Row = if (c == null) m.r3 else c
      new Matrix {
        val r1 = a1
        val r2 = a2
        val r3 = a3
      }
    }
    def cpy(m: Matrix, i: Int, o: OneEntry): Matrix = {
      i match {
        case 0 => cpy (m, a = m.r1.cpy (m.r1, a = o) )
        case 1 => cpy (m, a = m.r1.cpy (m.r1, b = o) )
        case 2 => cpy (m, a = m.r1.cpy (m.r1, c = o) )
        case 3 => cpy (m, b = m.r2.cpy (m.r2, a = o) )
        case 4 => cpy (m, b = m.r2.cpy (m.r2, b = o) )
        case 5 => cpy (m, b = m.r2.cpy (m.r2, c = o) )
        case 6 => cpy (m, c = m.r3.cpy (m.r3, a = o) )
        case 7 => cpy (m, c = m.r3.cpy (m.r3, b = o) )
        case 8 => cpy (m, c = m.r3.cpy (m.r3, c = o) )
      }
    }
  }

  abstract class Row {
    val c1: OneEntry
    val c2: OneEntry
    val c3: OneEntry

    def cpy(r: Row, a: OneEntry = null, b: OneEntry = null, c: OneEntry = null): Row = {
      val a1 = if (a == null) r.c1 else a
      val a2 = if (b == null) r.c2 else b
      val a3 = if (c == null) r.c3 else c
      new Row {
        val c1 = a1
        val c2 = a2
        val c3 = a3
      }
    }
  }


  abstract class OneEntry {
    self =>
    type T
    type A[_]
    val a: A[T]
    val ev: IRep[A]
    val evnum: Numeric[T]
    val evtyp: TypeRep[T]

    def cpy[T1: Numeric, A1[_] : IRep](n: A1[T1]) = {
      new OneEntry {
        override type A[X] = A1[X]
        override type T = T1
        override val evnum: Numeric[T1] = ???
        override val ev: IRep[A] = ???
        override val evtyp: TypeRep[T] = ???
        override val a: this.A[this.T] = n
      }
    }

    def makeSome(me: OneEntry): OptionalEntry = {
      new OptionalEntry {
        override type A[X] = me.A[X]
        override type T = me.T
        override val evnum: Numeric[T] = me.evnum
        override val ev: IRep[A] = me.ev
        override val a: Option[A[T]] = Some(me.a)
        override val evtyp = me.evtyp
      }
    }

    def makeNone(me: OneEntry): OptionalEntry = {
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

    def toOneEntry(): Option[OneEntry] = {
      if (a.isDefined)
        Some(new OneEntry {
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

  abstract class Image {
    val xsize: OneEntry{ type T = Int}
    val ysize: OneEntry{ type T = Int}
  }


  trait RepSelector2 {
    val rrep: Boolean

    def repselect(one: OneEntry): OptionalEntry =
      if (rrep) {
        if (one.ev.isRep()) one.makeSome(one) else one.makeNone(one)
      } else if (one.ev.isRep()) one.makeNone(one) else one.makeSome(one)

  }

  trait DynSelector2 extends RepSelector2 {
    val rrep: Boolean = true
  }

  trait StatSelector2 extends RepSelector2 {
    val rrep: Boolean = false
  }

  abstract class Base(image: Image, matrix: Matrix)

  abstract class Header(val image: Image, val matrix: Matrix) extends Base(image, matrix) with RepSelector2 {
    def a() = repselect(matrix.r1.c1)

    def b() = repselect(matrix.r1.c2)

    def c() = repselect(matrix.r1.c3)

    def d() = repselect(matrix.r2.c1)

    def e() = repselect(matrix.r2.c2)

    def f() = repselect(matrix.r2.c3)

    def g() = repselect(matrix.r3.c1)

    def h() = repselect(matrix.r3.c2)

    def i() = repselect(matrix.r3.c3)

    def x() = repselect(image.xsize)

    def y() = repselect(image.ysize)

  }

  case class InlineInfo(inline: Boolean, maxfunctions: Int, compareinline: Boolean, consider_inline: Boolean, specialize: Boolean, spezialize_done: Int)

  case class Blocking(val blockingx: Int, val blockingy: Int, val unrollx: Int, val unrolly: Int)

  case class Symetries(val valuesym: Map[Int,Vector[Int]])

  class StatFilterHeader(image: Image, matrix: Matrix, val inline: InlineInfo, val blocking: Blocking, val sym: Symetries) extends Header(image, matrix) with StatSelector2 {
    private def help(oneEntry: OneEntry): String = {
      val t = repselect(oneEntry)
      t.a match {
        case Some(x: NoRep[t.T]) => t.evnum.toLong(x).toString
        case _ => ""
      }
    }

    def genSig(): String = {
      val ix = help(image.xsize)
      val iy = help(image.ysize)
      val as = help(matrix.r1.c1)
      val bs = help(matrix.r1.c2)
      val cs = help(matrix.r1.c3)
      val ds = help(matrix.r2.c1)
      val es = help(matrix.r2.c2)
      val fs = help(matrix.r2.c3)
      val gs = help(matrix.r3.c1)
      val hs = help(matrix.r3.c2)
      val is = help(matrix.r3.c3)
      s"_ix${ix}iy${iy}a${as}b${bs}c${cs}d${ds}e${es}f${fs}g${gs}h${hs}i${is}inline${inline.specialize}${inline.spezialize_done}"
    }


  }

  class DynFilterHeader(val image_in: Rep[ImageH], val image_out: Rep[ImageH], image: Image, matrix: Matrix) extends Header(image, matrix) with DynSelector2


  object StatFilterHeader {
    def apply(image: Image, matrix: Matrix, inlineInfo: InlineInfo, blocking: Blocking, sym: Symetries) = new StatFilterHeader(image, matrix, inlineInfo, blocking, sym)

    def apply[A: Numeric : TypeRep,B: Numeric : TypeRep,C: Numeric : TypeRep,D: Numeric : TypeRep,E: Numeric : TypeRep,F: Numeric : TypeRep,G: Numeric : TypeRep,H: Numeric : TypeRep,I: Numeric : TypeRep](a: Option[A] = None, b: Option[B] = None, c: Option[C] = None, d: Option[D] = None, e: Option[E] = None, f: Option[F] = None, g: Option[G]= None, h: Option[H] = None, i: Option[I] = None, x: Option[Int] = None, y: Option[Int] = None, inlineInfo: InlineInfo = InlineInfo(false, 10, true, false, true, 0), blocking: Blocking = Blocking(16,16,1,1), sym: Symetries = Symetries(Map(1->Vector(1), 2-> Vector(2), 3->Vector(3), 4->Vector( 4), 5->Vector( 5), 6->Vector(6), 7->Vector(7), 8->Vector(8), 9->Vector(9)) )): StatFilterHeader = {

      def help[K: Numeric: TypeRep](o: Option[K]): OneEntry = {
        if (o.isDefined)
        new OneEntry {
          override type A[X] = NoRep[X]
          override type T = K
          override val evnum: Numeric[T] = implicitly[Numeric[K]]
          override val ev: IRep[A] = cNoRep
          override val a: NoRep[K] = o.get
          override val evtyp: TypeRep[K] = implicitly[TypeRep[K]]
        } else {
          val default = new OneEntry {
            override type A[X] = Rep[X]
            override type T = Int
            override val evnum: Numeric[T] = implicitly[Numeric[T]]
            override val ev: IRep[A] = cRep
            override val a: Rep[Int] = Const(-1)
            override val evtyp: TypeRep[Int] = implicitly[TypeRep[Int]]
          }
          default
        }
      }
      val nm = new Matrix {
        val r1 = new Row {
          val c1 = help(a)
          val c2 = help(b)
          val c3 = help(c)
        }
        val r2 = new Row {
          val c1 = help(d)
          val c2 = help(e)
          val c3 = help(f)
        }
        val r3 = new Row {
          val c1 = help(g)
          val c2 = help(h)
          val c3 = help(i)
        }
      }
      val im = new Image {
        override val xsize: OneEntry {type T = Int} = help(x).asInstanceOf[OneEntry {type T = Int}]
        override val ysize: OneEntry {type T = Int} = help(y).asInstanceOf[OneEntry {type T = Int}]
      }

      StatFilterHeader(im,nm,inlineInfo, blocking, sym)
    }
  }



  case class MixFilterHeader(image_in: Rep[ImageH], image_out: Rep[ImageH], image: Image, matrix: Matrix, inlineInfo: InlineInfo, blocking: Blocking, sym: Symetries) extends Base(image, matrix) {
    self =>
    def getDynHeader(): DynFilterHeader = new DynFilterHeader(image_in, image_out, image, matrix)

    def getStatHeader(): StatFilterHeader = new StatFilterHeader(image, matrix, inlineInfo, blocking,sym)

    def split(): (StatFilterHeader, DynFilterHeader) = (getStatHeader(), getDynHeader())

    def cpy(image_in: Rep[ImageH] = self.image_in, image_out: Rep[ImageH] = self.image_out, a: Option[self.matrix.r1.c1.T] = None, b: Option[self.matrix.r1.c2.T] = None, c: Option[self.matrix.r1.c3.T] = None, d: Option[self.matrix.r2.c1.T] = None, e: Option[self.matrix.r2.c2.T] = None, f: Option[self.matrix.r2.c3.T] = None, g: Option[self.matrix.r3.c1.T]= None, h: Option[self.matrix.r3.c2.T] = None, i: Option[self.matrix.r3.c3.T] = None, x: Option[Int] = None, y: Option[Int] = None, inlineInfo: InlineInfo = self.inlineInfo, blocking: Blocking = self.blocking, sym: Symetries = self.sym): MixFilterHeader = {

      def help(sofar: OneEntry)(ele: Option[sofar.T]): Option[OneEntry] = {
        if (ele.isDefined)
          Some(new OneEntry {
            override type A[X] = NoRep[X]
            override type T = sofar.T
            override val evnum: Numeric[T] = sofar.evnum
            override val ev: IRep[A] = cNoRep
            override val a: NoRep[T] = ele.get
            override val evtyp: TypeRep[T] = sofar.evtyp
          } )
        else None
      }
      val nm = new Matrix {
        val r1 = new Row {
          val c1 = help(self.matrix.r1.c1)(a).getOrElse(self.matrix.r1.c1)
          val c2 = help(self.matrix.r1.c2)(b).getOrElse(self.matrix.r1.c2)
          val c3 = help(self.matrix.r1.c3)(c).getOrElse(self.matrix.r1.c3)
        }
        val r2 = new Row {
          val c1 = help(self.matrix.r2.c1)(d).getOrElse(self.matrix.r2.c1)
          val c2 = help(self.matrix.r2.c2)(e).getOrElse(self.matrix.r2.c2)
          val c3 = help(self.matrix.r2.c3)(f).getOrElse(self.matrix.r2.c3)
        }
        val r3 = new Row {
          val c1 = help(self.matrix.r3.c1)(g).getOrElse(self.matrix.r3.c1)
          val c2 = help(self.matrix.r3.c2)(h).getOrElse(self.matrix.r3.c2)
          val c3 = help(self.matrix.r3.c3)(i).getOrElse(self.matrix.r3.c3)
        }
      }
      val im = new Image {
        override val xsize: OneEntry {type T = Int} = {
          val t = help(self.image.xsize)(x).getOrElse(self.image.xsize)
          t.asInstanceOf[OneEntry{ type T = Int}]
        }
        override val ysize: OneEntry {type T = Int} = {
          val t = help(self.image.ysize)(y).getOrElse(self.image.ysize)
          t.asInstanceOf[OneEntry{ type T = Int}]
        }
      }
      val nimin = image_in
      val nimout = image_out

      MixFilterHeader(nimin,nimout,im,nm,inlineInfo,blocking, sym)
    }
  }

  object MixFilterHeader {

    private def choose[A[_], T](a: Option[A[T]], b: Option[A[T]], ev: IRep[A]): A[T] = if (ev.isRep()) b.get else a.get

    private def choose[A[_], T](a: Option[A[T]], b: Option[A[T]]) = {
      if (a.isDefined) a.get else b.get
    }

    private def choose[T1, A1[_]](a: OneEntry, b: OneEntry): A1[T1] = {
      if (a.ev.isRep()) a.a else b.a

      ???
    }


    def apply(hs: StatFilterHeader, hd: DynFilterHeader): MixFilterHeader = {
      val a = hs.a().toOneEntry().getOrElse(hd.a().toOneEntry().get)
      val b = hs.b().toOneEntry().getOrElse(hd.b().toOneEntry().get)
      val c = hs.c().toOneEntry().getOrElse(hd.c().toOneEntry().get)

      val d = hs.d().toOneEntry().getOrElse(hd.d().toOneEntry().get)
      val e = hs.e().toOneEntry().getOrElse(hd.e().toOneEntry().get)
      val f = hs.f().toOneEntry().getOrElse(hd.f().toOneEntry().get)

      val g = hs.g().toOneEntry().getOrElse(hd.g().toOneEntry().get)
      val h = hs.h().toOneEntry().getOrElse(hd.h().toOneEntry().get)
      val i = hs.i().toOneEntry().getOrElse(hd.i().toOneEntry().get)

      val nm = new Matrix {
        val r1 = new Row {
          val c1 = a
          val c2 = b
          val c3 = c
        }
        val r2 = new Row {
          val c1 = d
          val c2 = e
          val c3 = f
        }
        val r3 = new Row {
          val c1 = g
          val c2 = h
          val c3 = i
        }
      }

      new MixFilterHeader(hd.image_in, hd.image_out, hd.image, nm, hs.inline, hs.blocking, hs.sym)
    }



  }

  implicit def exposeDynHeader(stat: StatFilterHeader): ExposeRep[DynFilterHeader] = new ExposeRep[DynFilterHeader] {
    val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => {
      val t = Vector(Arg[ImageH]) ++ Vector(Arg[ImageH]) ++
        stat.image.xsize.ev.fresh[Int]() ++
        stat.image.ysize.ev.fresh[Int]() ++
        stat.matrix.r1.c1.ev.fresh[stat.matrix.r1.c1.T]()(stat.matrix.r1.c1.evtyp) ++
        stat.matrix.r1.c2.ev.fresh[stat.matrix.r1.c2.T]()(stat.matrix.r1.c2.evtyp) ++
        stat.matrix.r1.c3.ev.fresh[stat.matrix.r1.c3.T]()(stat.matrix.r1.c3.evtyp) ++
        stat.matrix.r2.c1.ev.fresh[stat.matrix.r2.c1.T]()(stat.matrix.r2.c1.evtyp) ++
        stat.matrix.r2.c2.ev.fresh[stat.matrix.r2.c2.T]()(stat.matrix.r2.c2.evtyp) ++
        stat.matrix.r2.c3.ev.fresh[stat.matrix.r2.c3.T]()(stat.matrix.r2.c3.evtyp) ++
        stat.matrix.r3.c1.ev.fresh[stat.matrix.r3.c1.T]()(stat.matrix.r3.c1.evtyp) ++
        stat.matrix.r3.c2.ev.fresh[stat.matrix.r3.c2.T]()(stat.matrix.r3.c2.evtyp) ++
        stat.matrix.r3.c3.ev.fresh[stat.matrix.r3.c3.T]()(stat.matrix.r3.c3.evtyp)
      t
      //need to add image dimensions
    }
    val t2vec: DynFilterHeader => Vector[Exp[_]] = (in: DynFilterHeader) => {
      def help(ele: Option[OneEntry]): Vector[Exp[_]] = {
        ele.map(p => p.ev.getRep(p.a).map(o => Vector(o)).getOrElse(Vector.empty)).getOrElse(Vector.empty)
        //ele.map(p => ev.getRep(p).map(o => Vector(o)).getOrElse(Vector.empty)).getOrElse(Vector.empty)
      }
      val t = Vector(in.image_in) ++ Vector(in.image_out) ++
        help(in.x().toOneEntry()) ++
        help(in.y().toOneEntry()) ++
        help(in.a().toOneEntry()) ++
        help(in.b().toOneEntry()) ++
        help(in.c().toOneEntry()) ++
        help(in.d().toOneEntry()) ++
        help(in.e().toOneEntry()) ++
        help(in.f().toOneEntry()) ++
        help(in.g().toOneEntry()) ++
        help(in.h().toOneEntry()) ++
        help(in.i().toOneEntry())
      t
    }

    val vec2t: Vector[Exp[_]] => DynFilterHeader = (in: Vector[Exp[_]]) => {
      def help[T[_], A: TypeRep](in: Vector[Rep[_]], statele: OptionalEntry): (Vector[Rep[_]], OneEntry) = {


        val (vecafter, ele) = statele.ev.fetch[statele.T](in)(statele.evtyp)

        val res = ele.getOrElse(statele.a.get)


        val or = new OneEntry {
          override type A[X] = statele.A[X]
          override type T = statele.T
          override val evnum: Numeric[T] = statele.evnum
          override val ev: IRep[A] = statele.ev
          override val a: A[T] = res
          override val evtyp = statele.evtyp
        }
        (vecafter, or)

      }
      val image_in = in.head.asInstanceOf[Rep[ImageH]]
      val image_out = in.tail.head.asInstanceOf[Rep[ImageH]]
      val (ox, x) = help(in.tail.tail, stat.x())
      val (oy, y) = help(ox, stat.y())

      val (oa, a) = help(oy, stat.a())
      val (ob, b) = help(oa, stat.b())
      val (oc, c) = help(ob, stat.c())
      val (od, d) = help(oc, stat.d())
      val (oe, e) = help(od, stat.e())
      val (of, f) = help(oe, stat.f())
      val (og, g) = help(of, stat.g())
      val (oh, h) = help(og, stat.h())
      val (oi, i) = help(oh, stat.i())


      val nm = new Matrix {
        val r1 = new Row {
          val c1 = a
          val c2 = b
          val c3 = c
        }
        val r2 = new Row {
          val c1 = d
          val c2 = e
          val c3 = f
        }
        val r3 = new Row {
          val c1 = g
          val c2 = h
          val c3 = i
        }
      }

      val ni = new Image
      {
        override val xsize: OneEntry {type T = Int} = x.asInstanceOf[OneEntry {type T = Int}]
        override val ysize: OneEntry {type T = Int} = y.asInstanceOf[OneEntry {type T = Int}]
      }

      val t: DynFilterHeader = new DynFilterHeader(image_in, image_out, ni, nm)
      t

    }

  }


}

