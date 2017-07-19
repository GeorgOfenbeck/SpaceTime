package SpiralSThesis

import org.scala_lang.virtualized.SourceContext

trait Header extends Skeleton {

  var graphname: Boolean = false

  //just to rename
  def ifThenElse[A](cond: Rep[Boolean], thenp: => A, elsep: => A)(implicit pos: SourceContext, branch: ExposeRep[A]): A = myifThenElse[A](cond,thenp,elsep)

  implicit def mkDataEleOops(lhs: DataEle): DataEleOps = new DataEleOps(lhs)

  class DataEleOps(lhs: DataEle) {
    def +(rhs: DataEle) = lhs.dplus(lhs, rhs)

    def -(rhs: DataEle) = lhs.dminus(lhs, rhs)

    def *(rhs: DataEle) = lhs.dtimes(lhs, rhs)

    def /(rhs: DataEle) = lhs.ddiv(lhs, rhs)
  }


  val exposeComplexVector = new ExposeRep[Data]() {
    val freshExps = (u: Unit) => Vector(Arg[ComplexVector])
    val vec2t: Vector[Exp[_]] => SComplexVector = (in: Vector[Exp[_]]) => SComplexVector(in.head.asInstanceOf[Rep[ComplexVector]])
    val t2vec: Data => Vector[Exp[_]] = (in: Data) => in match {
      case v: SComplexVector => Vector(v.d)
      case _ => ???
    }
  }
  val exposeInterleavedComplexVector = new ExposeRep[Data]() {
    val freshExps = (u: Unit) => Vector(Arg[Array[Double]])
    val vec2t: Vector[Exp[_]] => InterleavedComplexVector = (in: Vector[Exp[_]]) => InterleavedComplexVector(in.head.asInstanceOf[Rep[Array[Double]]])
    val t2vec: Data => Vector[Exp[_]] = (in: Data) => in match {
      case v: InterleavedComplexVector => Vector(v.d)
      case _ => ???
    }
  }

  abstract class DataEle {

    def im(): Exp[Double]

    def re(): Exp[Double]


    def create(re: Exp[Double], im: Exp[Double]): DataEle

    def dplus(x1: DataEle, y1: DataEle): DataEle = (x1, y1) match {
      case (x: SComplex, y: SComplex) => x.dplus(x, y)
      case (x: InterleavedComplex, y: InterleavedComplex) => x.dplus(x, y)
      case _ => ???
    }

    def dminus(x1: DataEle, y1: DataEle): DataEle = (x1, y1) match {
      case (x: SComplex, y: SComplex) => x.dminus(x, y)
      case (x: InterleavedComplex, y: InterleavedComplex) => x.dminus(x, y)
      case _ => ???
    }

    def dtimes(x1: DataEle, y1: DataEle): DataEle = (x1, y1) match {
      case (x: SComplex, y: SComplex) => x.dtimes(x, y)
      case (x: InterleavedComplex, y: InterleavedComplex) => x.dtimes(x, y)
      case _ => ???
    }

    def ddiv(x1: DataEle, y1: DataEle): DataEle = (x1, y1) match {
      case (x: SComplex, y: SComplex) => x.ddiv(x, y)
      case (x: InterleavedComplex, y: InterleavedComplex) => x.ddiv(x, y)
      case _ => ???
    }
  }

  abstract class Data {
    def getdata(): Exp[_]

    def create(n: AInt): Data = ???

    def apply(i: AInt): DataEle

    def update(i: AInt, y: DataEle): Data = (this, y) match {
      case (me: SComplexVector, e: SComplex) => me.updatex(i, e)
      case (me: InterleavedComplexVector, e: InterleavedComplex) => me.updatex(i, e)
      case (me: ScalarVector, e: InterleavedComplex) => me.updatex(i, e)
      case (me: SComplexVector, e: InterleavedComplex) => me.updatex(i, new SComplex(compcreate(e.re, e.im)))
      case (me: ScalarVector, e: SComplex) => me.update(i, new InterleavedComplex(e.re(),e.im()))
      case _ => {
        ???
      }
    }

    def t2vec(): Vector[Exp[_]]


  }


  case class SComplex(d: Exp[Complex]) extends DataEle {

    def re(): Exp[Double] = compre(d)

    def im(): Exp[Double] = compim(d)

    def create(re: Exp[Double], im: Exp[Double]): SComplex = new SComplex(compcreate(re, im))

    def dplus(x: SComplex, y: SComplex): SComplex = SComplex(plus(x.d, y.d))

    def dminus(x: SComplex, y: SComplex): SComplex = SComplex(minus(x.d, y.d))

    def dtimes(x: SComplex, y: SComplex): SComplex = SComplex(times(x.d, y.d))
  }

  case class SComplexVector(d: Exp[ComplexVector]) extends Data {


    override def create(n: AInt): SComplexVector = SComplexVector(veccreate(n.ev.toRep(n.a)))

    override def getdata() = d

    def apply(i: AInt): SComplex = {
      val t = i.ev.toRep(i.a)
      SComplex(vecapply(d, t))
    }

    def updatex(i: AInt, y: SComplex): Data = {
      val t = i.ev.toRep(i.a)
      SComplexVector(vecupdate(d, t, y.d))
    }

    def t2vec(): Vector[Exp[_]] = Vector(d)
  }


  case class InterleavedComplex(re: Exp[Double], im: Exp[Double]) extends DataEle {



    def create(re: Exp[Double], im: Exp[Double]): InterleavedComplex = InterleavedComplex(re, im)

    def dplus(x: InterleavedComplex, y: InterleavedComplex): InterleavedComplex = InterleavedComplex(x.re + y.re, x.im + y.im)

    def dminus(x: InterleavedComplex, y: InterleavedComplex): InterleavedComplex = InterleavedComplex(x.re - y.re, x.im - y.im)

    def dtimes(x: InterleavedComplex, y: InterleavedComplex): InterleavedComplex = InterleavedComplex(x.re * y.re - x.im * y.im, x.re * y.im + x.im * y.re)
  }

  case class InterleavedComplexVector(d: Exp[Array[Double]]) extends Data {

    override def create(n: AInt): InterleavedComplexVector = InterleavedComplexVector(dveccreate(n.ev.toRep(n.a)))

    override def getdata() = d

    def apply(i: AInt): InterleavedComplex = {
      val t = i.ev.toRep(i.a)
      val re = dvecapply(d, 2 * t)
      val im = dvecapply(d, (2 * t) + 1)
      InterleavedComplex(re, im)
    }

    def updatex(i: AInt, y: InterleavedComplex): Data = {
      val t = i.ev.toRep(i.a)
      val re = dvecupdate(d, (2 * t), y.re)
      val im = dvecupdate(re, (2 * t + 1), y.im)
      InterleavedComplexVector(im)
    }

    def t2vec(): Vector[Exp[_]] = Vector(d)
  }


  case class ScalarVector(d: Array[Exp[Double]]) extends Data {


    override def create(n: AInt): ScalarVector = n.ev.fold[Int, ScalarVector](n.a,
      fa => {
        ??? /* this should not occur */
      }, fb => {
        ScalarVector(new Array[Exp[Double]](fb * 2))
      })

    override def getdata() = ???

    def apply(n: AInt): InterleavedComplex =
      n.ev.fold[Int, InterleavedComplex](n.a,
        fa => {
          ??? /* this should not occur */
        }, i => {
          val t = i
          val re = d(2 * t)
          val im = d((2 * t) + 1)
          InterleavedComplex(re, im)
        })

    def updatex(i: AInt, y: InterleavedComplex): Data =
      i.ev.fold[Int, Data](i.a,
        fa => {
          ??? /* this should not occur */
        }, fb => {
          val t = fb
          val re = d.update((2 * t), y.re)
          val im = d.update((2 * t + 1), y.im)
          this
        })


    def t2vec(): Vector[Exp[_]] = d.toVector
  }


  case class mf[A,R](f: Either[Rep[A => R], Rep[A] => Rep[R]])

  case class MaybeSFunction(f: Either[StagedFunction[Dyn, Data], Dyn => Data]) {
    def apply(dyn: Dyn): Data = f.fold(fa => fa(dyn), fb => fb(dyn))

    def mkfun(stat: Stat, dyn: Dyn): Data = f.fold(fa => fa(dyn), fb => {
      val expose = exposeDyn(stat)
      val t = doGlobalLambda(fb, Some("Base" + stat.toSig()), Some("Base" + stat.toSig()))(expose, stat.expdata)
      t(dyn)
    })
  }

  object MaybeSFunction {
    def apply(f: StagedFunction[Dyn, Data]): MaybeSFunction = MaybeSFunction(Left(f))

    def apply(f: (Dyn => Data)): MaybeSFunction = MaybeSFunction(Right(f))
  }

  def OO2Exp(oo: OptionalEntry): Vector[Exp[_]] = {
    oo.toOneEntry().map(p => p.ev.getRep(p.a).map(o => Vector(o)).getOrElse(Vector.empty)).getOrElse(Vector.empty)
  }

  implicit def toOE[X: Numeric : TypeRep](x: X): OneEntry {type T = X} = {
    new OneEntry {
      override type A[Y] = NoRep[Y]
      override type T = X
      override val evnum: scala.Numeric[X] = implicitly[Numeric[X]]
      override val ev: Header.this.IRep[A] = cNoRep
      override val a: this.A[this.T] = x
      override val evtyp: Header.this.TypeRep[this.T] = implicitly[TypeRep[X]]
    }
  }

  def toOEL[X: TypeRep](x: X): OneEntry {type T = X} = {
    new OneEntry {
      override type A[Y] = NoRep[Y]
      override type T = X
      override val evnum: scala.Numeric[X] = null
      //ugly!!!!
      override val ev: Header.this.IRep[A] = cNoRep
      override val a: this.A[this.T] = x
      override val evtyp: Header.this.TypeRep[this.T] = implicitly[TypeRep[X]]
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

    def makeSome(me: OneEntry) = {
      new OptionalEntry {
        override type A[X] = me.A[X]
        override type T = me.T
        override val evnum: Numeric[T] = me.evnum
        override val ev: IRep[A] = me.ev
        override val a: Option[A[T]] = Some(me.a)
        override val evtyp = me.evtyp
      }
    }

    def makeNone(me: OneEntry) = {
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

    def toSig(): String = a.fold("")(fb => fb match {
      case lis: Int => if (lis < 0) s"neg${Math.abs(lis)}" else lis.toString
      case _ => fb.toString
    }
    )


    def toOneEntry(): Option[OneEntry {type T = self.T}] = {
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

  def sph(): AInt = {
    new OneEntry {
      override type T = Int
      override type A[X] = Exp[X]
      override val evnum: Numeric[Int] = implicitly[Numeric[Int]]
      override val ev: IRep[A] = cRep
      override val evtyp: TypeRep[T] = manifest[Int]
      override val a: this.A[this.T] = fresh[Int]
    }
  }

  def sph2(): LInt = {
    new OneEntry {
      override type T = List[Int]
      override type A[X] = Exp[X]
      override val evnum: Numeric[T] = null
      //ugly!!!!
      override val ev: IRep[A] = cRep
      override val evtyp: TypeRep[T] = manifest[List[Int]]
      override val a: this.A[this.T] = fresh[List[Int]]
    }
  }

  def R2AInt(x: Exp[Int]): AInt = {
    new OneEntry {
      override type T = Int
      override type A[X] = Exp[X]
      override val evnum: Numeric[Int] = implicitly[Numeric[Int]]
      override val ev: IRep[A] = cRep
      override val evtyp: TypeRep[T] = manifest[Int]
      override val a: this.A[this.T] = x
    }
  }

  def R2LInt(x: Exp[List[Int]]): LInt = {
    new OneEntry {
      override type T = List[Int]
      override type A[X] = Exp[X]
      override val evnum: Numeric[T] = null
      //ugly!!!!!
      override val ev: IRep[A] = cRep
      override val evtyp: TypeRep[T] = manifest[List[Int]]
      override val a: this.A[this.T] = x
    }
  }

  type AInt = OneEntry {type T = Int}


  type LInt = OneEntry {type T = List[Int]}

  class AIntOps(lhs: AInt) {
    def +(rhs: AInt): AInt = {
      if (!(lhs.ev.isRep() && rhs.ev.isRep())) {
        (lhs.a, rhs.a) match {
          case (x: Int, y: Int) => toOE(x + y)
          case _ => (R2AInt(int_plus(lhs.ev.toRep(lhs.a), rhs.ev.toRep(rhs.a))))
        }
      } else {
        (R2AInt(int_plus(lhs.ev.toRep(lhs.a), rhs.ev.toRep(rhs.a))))
      }
    }

    def -(rhs: AInt): AInt = {
      if (!(lhs.ev.isRep() && rhs.ev.isRep())) {
        (lhs.a, rhs.a) match {
          case (x: Int, y: Int) => toOE(x - y)
          case _ => (R2AInt(int_minus(lhs.ev.toRep(lhs.a), rhs.ev.toRep(rhs.a))))
        }
      } else {
        (R2AInt(int_minus(lhs.ev.toRep(lhs.a), rhs.ev.toRep(rhs.a))))
      }
    }

    def *(rhs: AInt): AInt = {
      if (!(lhs.ev.isRep() && rhs.ev.isRep())) {
        (lhs.a, rhs.a) match {
          case (x: Int, y: Int) => toOE(x * y)
          case _ => (R2AInt(int_times(lhs.ev.toRep(lhs.a), rhs.ev.toRep(rhs.a))))
        }
      } else {
        (R2AInt(int_times(lhs.ev.toRep(lhs.a), rhs.ev.toRep(rhs.a))))
      }
    }

    def /(rhs: AInt): AInt = {
      if (!(lhs.ev.isRep() && rhs.ev.isRep())) {
        (lhs.a, rhs.a) match {
          case (x: Int, y: Int) => toOE(x / y)
          case _ => (R2AInt(int_divide(lhs.ev.toRep(lhs.a), rhs.ev.toRep(rhs.a))))
        }
      } else {
        (R2AInt(int_divide(lhs.ev.toRep(lhs.a), rhs.ev.toRep(rhs.a))))
      }
    }
  }

  implicit def toAIntOps(lhs: AInt):AIntOps = new AIntOps(lhs)


  trait RepSelector2 {
    val rrep: Boolean

    def repselect[X](one: OneEntry {type T = X}): OptionalEntry {type T = X} =
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

  abstract class IMHBase(val base: AInt, val s0: AInt, val s1: AInt)

  abstract class IMHHeader(base: AInt, s0: AInt, s1: AInt) extends IMHBase(base, s0, s1) with RepSelector2 {
    def getbase() = repselect(base)

    def gets0() = repselect(s0)

    def gets1() = repselect(s1)
  }

  class StatIMH(base: AInt, s0: AInt, s1: AInt) extends IMHHeader(base, s0, s1) with StatSelector2 {
    def freshExps(): Vector[Exp[_]] = base.ev.fresh()(base.evtyp) ++ s0.ev.fresh()(s0.evtyp) ++ s1.ev.fresh()(s1.evtyp)

    def vec2t(v: Vector[Exp[_]]): (DynIMH, Vector[Exp[_]]) = {
      def help(a: AInt, v: Vector[Exp[_]]): (AInt, Vector[Exp[_]]) = if (a.ev.isRep()) (R2AInt(v.head.asInstanceOf[Exp[Int]]), v.tail) else (toOE(-1), v)

      val (nb, ab) = help(base, v)
      val (ns0, as0) = help(s0, ab)
      val (ns1, as1) = help(s1, as0)
      (new DynIMH(nb, ns0, ns1), as1)
    }

    def genSig(): String = "b" + repselect(base).toSig() + "s0" + repselect(s0).toSig() + "s1" + repselect(s1).toSig()
  }

  class DynIMH(base: AInt, s0: AInt, s1: AInt) extends IMHHeader(base, s0, s1) with DynSelector2 {
    def t2vec(): Vector[Exp[_]] = OO2Exp(repselect(base)) ++ OO2Exp(repselect(s0)) ++ OO2Exp(repselect(s1))
  }

  object IMH {
    def apply(stat: StatIMH, dyn: DynIMH): IMH = {
      val b = stat.getbase().toOneEntry().getOrElse(dyn.getbase().toOneEntry().get)
      val s0 = stat.gets0().toOneEntry().getOrElse(dyn.gets0().toOneEntry().get)
      val s1 = stat.gets1().toOneEntry().getOrElse(dyn.gets1().toOneEntry().get)
      new IMH(b, s0, s1)
    }
  }

  case class IMH(override val base: AInt, override val s0: AInt, override val s1: AInt) extends IMHBase(base, s0, s1) {
    def getDynIMH(): DynIMH = new DynIMH(base, s0, s1)

    def getStatIMH(): StatIMH = new StatIMH(base, s0, s1)
  }

  object IM {
    def apply(statx: StatIM, dynx: DynIM): IMFull = {
      (statx, dynx) match {
        case (stat: Stat_GT_IM, dyn: Dyn_GT_IM) => apply(stat, dyn)
        case (stat: Stat_GTI_IM, dyn: Dyn_GTI_IM) => apply(stat, dyn)
        case (stat: Stat_GTT_IM, dyn: Dyn_GTT_IM) => apply(stat, dyn)
        case _ => ???
      }
    }

    def apply(stat: Stat_GT_IM, dyn: Dyn_GT_IM): GT_IM = {
      val g = IMH(stat.g, dyn.g)
      val s = IMH(stat.s, dyn.s)
      new GT_IM(g, s)
    }

    def apply(stat: Stat_GTI_IM, dyn: Dyn_GTI_IM): GTI_IM = {
      val im = IMH(stat.im, dyn.im)
      val tw = IMH(stat.twim, dyn.twim)
      new GTI_IM(im, tw)
    }

    def apply(stat: Stat_GTT_IM, dyn: Dyn_GTT_IM): GTT_IM = {
      val g = IMH(stat.g, dyn.g)
      val s = IMH(stat.s, dyn.s)
      val tw = IMH(stat.twim, dyn.twim)
      new GTT_IM(g, s, tw)
    }
  }


  abstract class IM {
    def gather(): IMHBase

    def scatter(): IMHBase
  }

  abstract class IMFull extends IM {
    def getDynIM(): DynIM

    def getStatIM(): StatIM
  }


  case class GT_IM(g: IMH, s: IMH) extends IMFull {
    def getDynIM(): Dyn_GT_IM = new Dyn_GT_IM(g.getDynIMH(), s.getDynIMH())

    def getStatIM(): Stat_GT_IM = new Stat_GT_IM(g.getStatIMH(), s.getStatIMH())

    def gather() = g

    def scatter() = s
  }

  case class GTI_IM(im: IMH, twim: IMH) extends IMFull {
    def getDynIM(): Dyn_GTI_IM = new Dyn_GTI_IM(im.getDynIMH(), twim.getDynIMH())

    def getStatIM(): Stat_GTI_IM = new Stat_GTI_IM(im.getStatIMH(), twim.getStatIMH())

    def gather() = im

    def scatter() = im
  }

  case class GTT_IM(g: IMH, s: IMH, twim: IMH) extends IMFull {
    def getDynIM(): Dyn_GTT_IM = new Dyn_GTT_IM(g.getDynIMH(), s.getDynIMH(), twim.getDynIMH())

    def getStatIM(): Stat_GTT_IM = new Stat_GTT_IM(g.getStatIMH(), s.getStatIMH(), twim.getStatIMH())

    def gather() = g

    def scatter() = s
  }

  abstract class StatIM extends IM {
    def freshExps(): Vector[Exp[_]]

    def vec2t(v: Vector[Exp[_]]): (DynIM, Vector[Exp[_]])

    def toSig() = "g" + gather().genSig() + "s" + scatter().genSig()

    def gather(): StatIMH

    def scatter(): StatIMH
  }

  case class Stat_GT_IM(g: StatIMH, s: StatIMH) extends StatIM {
    def freshExps(): Vector[Exp[_]] = g.freshExps() ++ s.freshExps()

    def vec2t(v: Vector[Exp[_]]): (DynIM, Vector[Exp[_]]) = {
      val (a, b) = g.vec2t(v)
      val (c, d) = s.vec2t(b)
      (new Dyn_GT_IM(a, c), d)
    }

    def gather() = g

    def scatter() = s
  }

  case class Stat_GTI_IM(im: StatIMH, twim: StatIMH) extends StatIM {
    override def toSig() = "im" + im.genSig() + "tw" + twim.genSig()

    def freshExps(): Vector[Exp[_]] = im.freshExps() ++ twim.freshExps()

    def vec2t(v: Vector[Exp[_]]): (DynIM, Vector[Exp[_]]) = {
      val (a, b) = im.vec2t(v)
      val (c, d) = twim.vec2t(b)
      (new Dyn_GTI_IM(a, c), d)
    }

    def gather() = im

    def scatter() = im
  }

  case class Stat_GTT_IM(g: StatIMH, s: StatIMH, twim: StatIMH) extends StatIM {
    override def toSig() = "g" + g.genSig() + "s" + s.genSig() + "tw" + twim.genSig()

    def freshExps(): Vector[Exp[_]] = g.freshExps() ++ s.freshExps() ++ twim.freshExps()

    def vec2t(v: Vector[Exp[_]]): (DynIM, Vector[Exp[_]]) = {
      val (a, b) = g.vec2t(v)
      val (c, d) = s.vec2t(b)
      val (e, f) = twim.vec2t(d)
      (new Dyn_GTT_IM(a, c, e), f)
    }

    def gather() = g

    def scatter() = s
  }


  abstract class DynIM extends IM {
    def t2vec(): Vector[Exp[_]]
  }

  case class Dyn_GT_IM(g: DynIMH, s: DynIMH) extends DynIM {
    def t2vec(): Vector[Exp[_]] = g.t2vec() ++ s.t2vec()

    def gather() = g

    def scatter() = s
  }

  case class Dyn_GTI_IM(im: DynIMH, twim: DynIMH) extends DynIM {
    def t2vec(): Vector[Exp[_]] = im.t2vec() ++ twim.t2vec()

    def gather() = im

    def scatter() = im
  }

  case class Dyn_GTT_IM(g: DynIMH, s: DynIMH, twim: DynIMH) extends DynIM {
    def t2vec(): Vector[Exp[_]] = g.t2vec() ++ s.t2vec() ++ twim.t2vec()

    def gather() = g

    def scatter() = s
  }


  abstract class Base(n: AInt, lb: AInt, im: IM, tw: Option[TwidBase])

  abstract class Header(n: AInt, lb: AInt, im: IM, tw: Option[TwidHeader]) extends Base(n, lb, im,  tw) with RepSelector2 {
    def getn() = repselect(n)

    def getlb() = repselect(lb)

    def getim(): IM = im

    def gettw(): Option[TwidHeader] = tw
  }

  class Stat(val n: AInt, val lb: AInt, val im: StatIM, val tw: Option[StatTwiddleScaling], val par: Option[Int], val precompute: Boolean, val expdata: ExposeRep[Data], val scalars: Boolean) extends Header(n, lb, im,  tw) with StatSelector2 {
    //(x: Data, y: Data, n: AInt, lb: AInt, im: IMFull, v: AInt, tw: Option[TwiddleScaling], par: Option[Int], precompute: Boolean, expdata: ExposeRep[Data], scalars: Boolean)
    def toName(): String = {
      if (graphname){

        val ssize: (String,String) = n.ev.fold[Int, (String,String)](n.a, fa => (",size: Int",""), fb=> (("",s", size = ${n.a}")))
        val slb: (String,String) = lb.ev.fold[Int, (String,String)](lb.a, fa => (",lb: Int",""), fb=> (("",s", lb = ${lb.a}")))

        def IM2S(prefix: String, imh: StatIMH): (String,String) = {
          /*val b: (String,String) = imh.base.ev.fold[Int, (String,String)](imh.base.a, fa => (s",${prefix}_b: Int",""), fb=> (("",s", ${prefix}_b = ${imh.base.a}")))
          val s0: (String,String) = imh.s0.ev.fold[Int, (String,String)](imh.s0.a, fa => (s",${prefix}_s0: Int",""), fb=> (("",s", ${prefix}_s0 = ${imh.s0.a}")))
          val s1: (String,String) = imh.s1.ev.fold[Int, (String,String)](imh.s1.a, fa => (s",${prefix}_s1: Int",""), fb=> (("",s", ${prefix}_s1 = ${imh.s1.a}")))*/
          val b: (String,String) = imh.base.ev.fold[Int, (String,String)](imh.base.a, fa => (s",${prefix}_b: Int",""), fb=> (("",s"${imh.base.a}")))
          val s0: (String,String) = imh.s0.ev.fold[Int, (String,String)](imh.s0.a, fa => (s",${prefix}_s0: Int",""), fb=> (("",s"${imh.s0.a}")))
          val s1: (String,String) = imh.s1.ev.fold[Int, (String,String)](imh.s1.a, fa => (s",${prefix}_s1: Int",""), fb=> (("",s"${imh.s1.a}")))
          //(b._1 ++ s0._1 ++ s1._1, b._2 ++ s0._2 ++ s1._2 )
          val scnd = if (s"${b._2},${s0._2},${s1._2}" == ",,") "" else prefix + s"<SUB> ${b._2},${s0._2},${s1._2} </SUB>) "
          (b._1 ++ s0._1 ++ s1._1, scnd)
        }
        val sim: (String,String) = im match {
          case gttim: Stat_GTT_IM => {
            val gs = IM2S("G(h",gttim.g)
            val ss = IM2S("S(h",gttim.s)
            val ts = IM2S("Tdiag(h",gttim.twim)
            (gs._1 ++ ss._1 ++ ts._1, gs._2 ++ ss._2 ++ ts._2 )
          }
          case gttim: Stat_GT_IM => {
            val gs = IM2S("G(h",gttim.g)
            val ss = IM2S("S(h",gttim.s)
            (gs._1 ++ ss._1 , gs._2 ++ ss._2 )
          }
          case gttim: Stat_GTI_IM => {
            val gs = IM2S("G(h",gttim.im)
            val ss = IM2S("S(h",gttim.im)
            val ts = IM2S("TI(h",gttim.twim)
            (gs._1 ++ ss._1 ++ ts._1, gs._2 ++ ss._2 ++ ts._2 )
          }
        }
        val ts: (String, String) = tw.fold(("",""))(tw => {
          val sn: (String,String) = tw.n.ev.fold[Int, (String,String)](tw.n.a, fa => (",Tn: Int",""), fb=> (("",s"${tw.n.a}")))
          val sd: (String, String) = tw.d.ev.fold[Int, (String,String)](tw.d.a, fa => (",Td: Int",""), fb=> (("",s" ${tw.d.a}")))
          val sk: (String, String)= tw.k.ev.fold[Int, (String,String)](tw.k.a, fa => (",Tk: Int",""), fb=> (("",s"${tw.k.a}")))
          val scnd = if (s"${sn._2},${sd._2},${sk._2}" == "T") "" else  s"T<SUP> ${sn._2}</SUP><SUB>${sd._2},${sk._2}</SUB>)"
          (sn._1 ++ sd._1 ++ sk._1, scnd)//sn._2 ++ sd._2 ++ sk._2 )
        })
        //par: Option[Int], precompute: Boolean, expdata: ExposeRep[Data], scalars: Boolean)
        val spar = par.fold("")(f => f.toString)
        val sscalar = if(scalars) "scalar = true" else ""
        val rest = s"$spar $sscalar"
        val dyns = s"(in: Data, out: Data ${ssize._1} ${slb._1} ${sim._1} ${ts._1})"
        val stats = "[" + s"${ssize._2} ${slb._2} ${sim._2} ${ts._2} $rest]".trim.stripPrefix(",") //.dropWhile(s => s != ',' && s != ']')

        //dyns + stats
        stats
      }
      else toSig()
    }
    def toSig(): String = {
      val t = "n" + repselect(n).toSig() + "lb" + repselect(lb).toSig() + im.toSig() + "tw" + tw.fold("")(t => t.genSig()) + "par" + par.fold("")(p => p.toString) + "scalars" + scalars
      t
    }

    override def getim(): StatIM = im

    override def gettw(): Option[StatTwiddleScaling] = tw
  }

  class Dyn(val x: Data, val y: Data, n: AInt, lb: AInt, im: DynIM, tw: Option[DynTwiddleScaling]) extends Header(n, lb, im, tw) with DynSelector2 {
    override def getim(): DynIM = im

    override def gettw(): Option[DynTwiddleScaling] = tw
  }


  object Mix {
    def apply(stat: Stat, dyn: Dyn): Mix = {
      val n: AInt = stat.getn().toOneEntry().getOrElse(dyn.getn().toOneEntry().get)
      val lb = stat.getlb().toOneEntry().getOrElse(dyn.getlb().toOneEntry().get)
      val g = stat.getim()
      val s = dyn.getim()
      val im = IM(g, s)
      val tw = TwiddleScaling(stat.gettw(), dyn.gettw())
      Mix(dyn.x, dyn.y, n, lb, im, tw, stat.par, stat.precompute, stat.expdata, stat.scalars)
    }
  }


  case class Mix(x: Data, y: Data, n: AInt, lb: AInt, im: IMFull, tw: Option[TwiddleScaling], par: Option[Int], precompute: Boolean, expdata: ExposeRep[Data], scalars: Boolean) extends Base(n, lb, im,  tw) {
    def getDyn(): Dyn = new Dyn(x, y, n, lb, im.getDynIM(), tw.fold[Option[DynTwiddleScaling]](None)(fb => Some(fb.getDynTwiddleScaling())))

    def getStat(): Stat = new Stat(n, lb, im.getStatIM(), tw.fold[Option[StatTwiddleScaling]](None)(fb => Some(fb.getStatTwiddleScaling())), par, precompute, expdata, scalars)
  }

  def OR2AInt[T](op: Option[T]): AInt = op match {
    case Some(x: Rep[Int]) => R2AInt(x)
    case _ => toOE(-1)
  }

  def OR2LInt[T](op: Option[T]): LInt = op match {
    case Some(x: Rep[List[Int]]) => R2LInt(x)
    case _ => toOEL(List.empty)
  }

  implicit def exposeDyn(stat: Stat): ExposeRep[Dyn] = {
    new ExposeRep[Dyn]() {
      val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => {
        val t = stat.expdata.freshExps() ++ stat.expdata.freshExps() ++ stat.n.ev.fresh()(stat.n.evtyp) ++ stat.lb.ev.fresh()(stat.lb.evtyp) ++
         stat.im.freshExps() ++ stat.tw.fold[Vector[Exp[_]]](Vector.empty)(fb => fb.freshExps())
        t
      }
      val vec2t: Vector[Exp[_]] => Dyn = (in: Vector[Exp[_]]) => {
        def removeData(v: Vector[Exp[_]]): (Data, Vector[Exp[_]]) = {
          val d = stat.expdata.vec2t(v)
          val me = stat.expdata.t2vec(d)
          (d, v.drop(me.size))
        }

        val (x, ax) = removeData(in)
        val (y, ay) = removeData(ax)
        val (an, on) = stat.n.ev.fetch[Int](ay)
        val (alb, olb) = stat.lb.ev.fetch[Int](an)
        //val (av, ov) = stat.v.ev.fetch[Int](alb)
        val (im, aim) = stat.im.vec2t(alb)

        val (atw, otw) = stat.tw.fold[(Vector[Exp[_]], Option[DynTwiddleScaling])]((aim, None))(fb => {
          val (t0, t1) = fb.vec2t(aim)
          (t1, Some(t0))
        })
        val n: AInt = OR2AInt(on)
        val lb: AInt = OR2AInt(olb)
        new Dyn(x, y, n, lb, im, otw)
      }
      val t2vec: Dyn => Vector[Exp[_]] = (in: Dyn) => {
        in.x.t2vec() ++ in.y.t2vec() ++
          OO2Exp(in.getn()) ++
          OO2Exp(in.getlb()) ++
          in.getim().t2vec() ++
          in.gettw().fold[Vector[Exp[_]]](Vector.empty)(fb => fb.t2vec(fb))
      }
    }
  }


  abstract class TwidBase(n: AInt, d: AInt, k: AInt)

  abstract class TwidHeader(n: AInt, d: AInt, k: AInt) extends TwidBase(n, d, k) with RepSelector2 {
    def getn() = repselect(n)

    def getd() = repselect(d)

    def getk() = repselect(k)

  }

  case class TwiddleScaling(n: AInt, d: AInt, k: AInt) extends TwidBase(n, d, k) {
    def getDynTwiddleScaling(): DynTwiddleScaling = new DynTwiddleScaling(n, d, k)

    def getStatTwiddleScaling(): StatTwiddleScaling = new StatTwiddleScaling(n, d, k)
  }

  class StatTwiddleScaling(val n: AInt, val d: AInt, val k: AInt) extends TwidHeader(n, d, k) with StatSelector2 {

    def genSig(): String = "n" + repselect(n).toSig() + "d" + repselect(d).toSig() + "k" + repselect(k).toSig()

    def freshExps(): Vector[Exp[_]] = n.ev.fresh()(n.evtyp) ++ d.ev.fresh()(d.evtyp) ++ k.ev.fresh()(k.evtyp)

    //base.ev.fresh()(base.evtyp) ++ s0.ev.fresh()(s0.evtyp) ++ s1.ev.fresh()(s1.evtyp)
    def vec2t(v: Vector[Exp[_]]): (DynTwiddleScaling, Vector[Exp[_]]) = {
      val (an, on) = n.ev.fetch[Int](v)
      val (ad, od) = d.ev.fetch[Int](an)
      val (ak, ok) = k.ev.fetch[Int](ad)
      val nx = OR2AInt(on)
      val dx = OR2AInt(od)
      val kx = OR2AInt(ok)
      (new DynTwiddleScaling(nx, dx, kx), ad)
    }
  }

  case class DynTwiddleScaling(n: AInt, d: AInt, k: AInt) extends TwidHeader(n, d, k) with DynSelector2 {
    def t2vec(in: DynTwiddleScaling): Vector[Exp[_]] = OO2Exp(repselect(n)) ++ OO2Exp(repselect(d)) ++ OO2Exp(repselect(k))
  }

  object TwiddleScaling {
    def apply(statx: Option[StatTwiddleScaling], dynx: Option[DynTwiddleScaling]): Option[TwiddleScaling] = {
      statx.fold[Option[TwiddleScaling]](None)(stat => dynx.fold[Option[TwiddleScaling]](None)(dyn => {
        val n: AInt = stat.getn().toOneEntry().getOrElse(dyn.getn().toOneEntry().get)
        val d = stat.getd().toOneEntry().getOrElse(dyn.getd().toOneEntry().get)
        val k = stat.getk().toOneEntry().getOrElse(dyn.getk().toOneEntry().get)
        Some(TwiddleScaling(n, d, k))
      }))
    }
  }

  case class iData(in: Data, out: Data, i: AInt, scalars: Boolean)

  def exposeiData(expdata: ExposeRep[Data]) = new ExposeRep[iData]() {
    val freshExps = (u: Unit) => Vector(Arg[Int]) ++ expdata.freshExps() ++ expdata.freshExps()
    val vec2t: Vector[Exp[_]] => iData = (in: Vector[Exp[_]]) => {
      val t = in(0).asInstanceOf[Rep[Int]]
      val input = expdata.vec2t(in.tail)
      val vecs = expdata.t2vec(input)
      val rest = in.tail.drop(vecs.size)
      val output = expdata.vec2t(rest)
      iData(input, output, R2AInt(t),false)
    }
    val t2vec: iData => Vector[Exp[_]] = (in: iData) => {
      val t = Vector(in.i.ev.toRep(in.i.a))
      val input: Vector[Exp[_]] = expdata.t2vec(in.in)
      val output: Vector[Exp[_]] = expdata.t2vec(in.out)
      t ++ input ++ output
    }
  }


}