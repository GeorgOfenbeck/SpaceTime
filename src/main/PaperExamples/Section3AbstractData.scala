package PaperExamples

/**
  * Created by rayda on 13-Nov-16.
  */
trait Section3AbstractData extends Skeleton {


  abstract class AC[R[_] : IRep] {
    val ev = implicitly[IRep[R]]

    import ev._

    val expose: ExposeRep[AC[R]]

    def apply(i: R[Int]): Rep[Double]

    def update(i: R[Int], y: Rep[Double])

    def length(): R[Int]

    def isscalarized(): Boolean

    def scalarize(size: Int): MetaArrayofScalars

    def fromScalars(x: Array[Rep[Double]]): AC[R]

    def t2vec(): Vector[Exp[_]]

    def vec2t(in: Vector[Exp[_]]): AC[R]

    def fresh(): Vector[Exp[_]]

    def splitAt(pos: R[Int]): (AC[R], AC[R])

    def concatx(rhs: AC[R]): AC[R]

    def concatx(lhs: StagedArray): StagedArray
    def concatx(lhs: MetaArrayofScalars): MetaArrayofScalars

  }

  class StagedArray(val data: Rep[Array[Double]]) extends
    AC[Rep] {

    val expose: ExposeRep[AC[Rep]] = exposeAC(this)

    def apply(i: Rep[Int]): Rep[Double] = data(i)

    def update(i: Rep[Int], y: Rep[Double]) = {
      data(i) = y
    }

    def length(): Rep[Int] = ???

    val isscalarized = false

    def scalarize(size: Int): MetaArrayofScalars = {
      val scalars = new MetaArrayofScalars(new Array[Rep[Double]](size))
      for (i <- 0 until size) scalars(i) = data(Const(i))
      scalars
    }

    def fresh(): Vector[Exp[_]] = Vector(Arg[Array[Double]])

    def t2vec(): Vector[Exp[_]] = Vector(data)

    def vec2t(in: Vector[Exp[_]]): StagedArray = new StagedArray(in.head.asInstanceOf[Rep[Array[Double]]])

    def splitAt(pos: Rep[Int]): (StagedArray, StagedArray) = {
      val l = take(data,pos)
      val r = drop(data,pos)
      (new StagedArray(l), new StagedArray(r))
    }

    def fromScalars(x: Array[Rep[Double]]): AC[Rep] =  new StagedArray( vector_obj_fromseq(x))

    def concatx(rhs: AC[Rep]): AC[Rep] = rhs.concatx(this)

    def concatx(lhs: StagedArray): StagedArray = {
      val con:Rep[Array[Double]] = concat(lhs.data, this.data)
      new StagedArray(con)
    }
    def concatx(lhs: MetaArrayofScalars): MetaArrayofScalars = ???
  }

  class MetaArrayofScalars(val data: Array[Rep[Double]])
    extends AC[NoRep] {

    val expose: ExposeRep[AC[NoRep]] = exposeAC(this)

    def apply(i: Int) = data(i)

    def update(i: Int, y: Rep[Double]) = {
      data(i) = y
    }

    def length(): NoRep[Int] = data.length

    val isscalarized = true

    def scalarize(size: Int) = this

    def fresh(): Vector[Exp[_]] = (0 until length()).foldLeft(Vector.empty[Exp[_]])((acc,ele) => acc :+ Arg[Double])

    def t2vec(): Vector[Exp[_]] = data.toVector

    def vec2t(in: Vector[Exp[_]]): MetaArrayofScalars = {
      val us = in.take(length())
      new MetaArrayofScalars(us.asInstanceOf[Array[Rep[Double]]])
    }

    def splitAt(pos: NoRep[Int]): (MetaArrayofScalars, MetaArrayofScalars) = {
      val (l,r) = data.splitAt(pos)
      (new MetaArrayofScalars(l), new MetaArrayofScalars(r))
    }

    def fromScalars(x: Array[Rep[Double]]): AC[NoRep] = new MetaArrayofScalars(x)

    def concatx(rhs: AC[NoRep]): AC[NoRep] = rhs.concatx(this)

    def concatx(lhs: StagedArray): StagedArray = ???
    def concatx(lhs: MetaArrayofScalars): MetaArrayofScalars = new MetaArrayofScalars(lhs.data ++ this.data)
  }


  implicit def exposeAC[R[_]: IRep](sample: AC[R]): ExposeRep[AC[R]] = new ExposeRep[AC[R]] {
    val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => sample.fresh()
    val t2vec: AC[R] => Vector[Exp[_]] = (in: AC[R]) => in.t2vec()
    val vec2t: Vector[Exp[_]] => AC[R] = (in: Vector[Exp[_]]) => sample.vec2t(in)
  }
}
