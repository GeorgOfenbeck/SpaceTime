package Filter2

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.scalalike._

trait Sort_DSL  extends BaseExp with FunctionsExp with BooleanOpsExpOpt with IfThenElsePureExp with PurePrimitiveOpsExp with OrderingOpsExp with RangeOpsExp with ImplicitOpsExp with ScalaCompile {

  case class Take(x: Rep[Array[Double]], pos: Exp[Int]) extends Def[Array[Double]]

  def take(x: Rep[Array[Double]], pos: Exp[Int]): Rep[Array[Double]] = Take(x, pos)

  case class Drop(x: Rep[Array[Double]], pos: Exp[Int]) extends Def[Array[Double]]

  def drop(x: Rep[Array[Double]], pos: Exp[Int]): Rep[Array[Double]] = Take(x, pos)

  case class Sin(x: Exp[Double]) extends Def[Double]

  def sinus(x: Exp[Double]): Exp[Double] = Sin(x)

  case class PixelGetBlue(r: Exp[Int]) extends Def[Int]

  def pixelgetBlue(r: Exp[Int]): Exp[Int] = PixelGetBlue(r)

  case class PixelGetGreen(r: Exp[Int]) extends Def[Int]

  def pixelgetGreen(r: Exp[Int]): Exp[Int] = PixelGetGreen(r)

  case class PixelGetRed(r: Exp[Int]) extends Def[Int]

  def pixelgetRed(r: Exp[Int]): Exp[Int] = PixelGetRed(r)

  case class PixelGetAlpha(r: Exp[Int]) extends Def[Int]

  def pixelgetAlpha(r: Exp[Int]): Exp[Int] = PixelGetAlpha(r)

  case class CombinePixel(a: Exp[Double], r: Exp[Double], g: Exp[Double], b: Exp[Double]) extends Def[Int]

  def combinePixel(a: Exp[Double], r: Exp[Double], g: Exp[Double], b: Exp[Double]): Exp[Int] = CombinePixel(a, r, g, b)

  case class ToInt[T: Numeric : TypeRep](lhs: Exp[T]) extends Def[Int]

  def toInt[T: Numeric : TypeRep](lhs: Exp[T]): Exp[Int] = ToInt(lhs)

  case class ToDouble[T: Numeric : TypeRep](lhs: Exp[T]) extends Def[Double]

  def toDouble[T: Numeric : TypeRep](lhs: Exp[T]): Exp[Double] = ToDouble(lhs)

  case class FromInt[T: Numeric : TypeRep](lhs: Exp[Int]) extends Def[T]

  def fromInt[T: Numeric : TypeRep](lhs: Exp[Int]): Exp[T] = FromInt[T](lhs)

  case class Plus[T: Numeric : TypeRep](lhs: Exp[T], rhs: Exp[T]) extends Def[T]

  def genplus[T: Numeric : TypeRep](lhs: Exp[T], rhs: Exp[T]): Exp[T] = {
    val nev = implicitly[Numeric[T]]
    val z = Const(nev.fromInt(0))
    (lhs, rhs) match {
      case (`z`, _) => rhs
      case (_, `z`) => lhs
      case _ => Plus(lhs, rhs)
    }
  }

  case class Times[T: Numeric : TypeRep](lhs: Exp[T], rhs: Exp[T]) extends Def[T]

  def gentimes[T: Numeric : TypeRep](lhs: Exp[T], rhs: Exp[T]): Exp[T] = {
    val nev = implicitly[Numeric[T]]
    val z = Const(nev.fromInt(0))
    val o = Const(nev.fromInt(1))
    (lhs, rhs) match {
      case (`o`, _) => rhs
      case (_, `o`) => lhs
      case (`z`, _) => Const(nev.fromInt(0))
      case (_, `z`) => Const(nev.fromInt(0))
      case _ => Times(lhs, rhs)
    }
  }


  case class SetImage[T: Numeric : TypeRep](img: Exp[ImageH], x: Exp[Int], y: Exp[Int], p: Exp[T]) extends Def[ImageH]

  def setImage[T: Numeric : TypeRep](img: Exp[ImageH], x: Exp[Int], y: Exp[Int], p: Exp[T]): Exp[ImageH] = SetImage(img, x, y, p)

  case class GetImage[T: Numeric : TypeRep](img: Exp[ImageH], x: Exp[Int], y: Exp[Int]) extends Def[T]

  def getImage[T: Numeric : TypeRep](img: Exp[ImageH], x: Exp[Int], y: Exp[Int]): Exp[T] = GetImage[T](img, x, y)

  case class Int_max(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]

  def int_max(lhs: Exp[Int], rhs: Exp[Int]): Exp[Int] = Int_max(lhs, rhs)

  case class MultiplyToLen(a: Exp[Array[Int]], alen: Exp[Int], b: Exp[Array[Int]], blen: Exp[Int]) extends Def[Array[Int]]

  def multiplyToLen(a: Exp[Array[Int]], alen: Exp[Int], b: Exp[Array[Int]], blen: Exp[Int]): Exp[Array[Int]] = MultiplyToLen(a, alen, b, blen)

  case class Int_Quick_Compare(a: Exp[Int], b: Exp[Int]) extends Def[Int]


  case class Double_Compare(a: Exp[Double], b: Exp[Double]) extends Def[Int]

  def int_quick_compare(a: Exp[Int], b: Exp[Int]): Exp[Int] = Int_Quick_Compare(a, b)

  def double_compare(a: Exp[Double], b: Exp[Double]): Exp[Int] = Double_Compare(a, b)

  case class Radix(n: Exp[Int]) extends Def[Int]

  def choose_radix(n: Exp[Int]): Exp[Int] = Radix(n)

  case class BaseCase(n: Exp[Int]) extends Def[Boolean]

  def isbasecase(n: Exp[Int]): Exp[Boolean] = BaseCase(n)

  case class IsPrime(n: Exp[Int]) extends Def[Boolean]

  def isPrime(n: Exp[Int]): Exp[Boolean] = IsPrime(n)

  case class Int_Eq(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Boolean]

  def int_eq(lhs: Exp[Int], rhs: Exp[Int]): Exp[Boolean] = Int_Eq(lhs, rhs)

  case class IVecUpRank(v: Exp[Vector[Int]]) extends Def[Vector[Int]]

  def ivecuprank(v: Exp[Vector[Int]]): Exp[Vector[Int]] = IVecUpRank(v)

  case class IVecCreate(s: Exp[Int]) extends Def[Vector[Int]]

  def iveccreate(i: Exp[Int]): Exp[Vector[Int]] = IVecCreate(i)

  case class IVecApply(vec: Exp[Vector[Int]], i: Exp[Int]) extends Def[Int]

  def ivecapply(vec: Exp[Vector[Int]], i: Exp[Int]): Exp[Int] = IVecApply(vec, i)


  case class IVecFirstorZero(vec: Exp[Vector[Int]]) extends Def[Int]

  def ivecfirstorzero(vec: Exp[Vector[Int]]): Exp[Int] = IVecFirstorZero(vec)


  case class IVecUpdate(vec: Exp[Vector[Int]], i: Exp[Int], y: Exp[Int]) extends Def[Vector[Int]]

  def ivecupdate(vec: Exp[Vector[Int]], i: Exp[Int], y: Exp[Int]): Exp[Vector[Int]] = IVecUpdate(vec, i, y)

  case class IVecAddStride(vec: Exp[Vector[Int]], y: Exp[Int], blub: Exp[Int]) extends Def[Vector[Int]]

  def ivecaddstride(vec: Exp[Vector[Int]], y: Exp[Int], blub: Exp[Int]): Exp[Vector[Int]] = IVecAddStride(vec, y, blub)

  case class IVecAppend(vec: Exp[Vector[Int]], y: Exp[Int]) extends Def[Vector[Int]]

  def ivecappend(vec: Exp[Vector[Int]], y: Exp[Int]): Exp[Vector[Int]] = IVecAppend(vec, y)

  case class IVecMult(base: Exp[Int], vec1: Exp[Vector[Int]], vec2: Exp[Vector[Int]]) extends Def[Int]

  def ivecmult(base: Exp[Int], vec1: Exp[Vector[Int]], vec2: Exp[Vector[Int]]): Exp[Int] = IVecMult(base, vec1, vec2)

  case class IVecZipMagic(vec1: Exp[Vector[Int]], vec2: Exp[Vector[Int]]) extends Def[Vector[Int]]

  def iveczipmagic(vec1: Exp[Vector[Int]], vec2: Exp[Vector[Int]]): Exp[Vector[Int]] = IVecZipMagic(vec1, vec2)

  //case class Concat[T:Manifest](lhs: Exp[Vector[T]], rhs: Exp[Vector[T]]) extends Def[Vector[T]]

  //def concat[T:Manifest](lhs: Exp[Vector[T]], rhs: Exp[Vector[T]]): Exp[Vector[T]] = Concat(lhs,rhs)

  case class Concat[T: Manifest](lhs: Exp[Array[T]], rhs: Exp[Array[T]]) extends Def[Array[T]]

  def concat[T: Manifest](lhs: Exp[Array[T]], rhs: Exp[Array[T]]): Exp[Array[T]] = Concat(lhs, rhs)

  //case class InserationCore[T: Manifest](v: Exp[Vector[T]], e: Exp[Int]) extends Def[Vector[T]]
  case class InserationCore[T: Manifest](v: Exp[Array[T]], e: Exp[Int]) extends Def[Array[T]]

  //def inserationcore[T: Manifest](v: Exp[Vector[T]], e: Exp[Int]): Exp[Vector[T]] = InserationCore(v,e)
  def inserationcore[T: Manifest](v: Exp[Array[T]], e: Exp[Int]): Exp[Array[T]] = InserationCore(v, e)

  case class InserationCoreI[T: Manifest](v: Exp[Array[T]], start: Exp[Int], end: Exp[Int]) extends Def[Array[T]]

  //def inserationcore[T: Manifest](v: Exp[Vector[T]], e: Exp[Int]): Exp[Vector[T]] = InserationCore(v,e)
  def inserationcore_imp[T: Manifest](v: Exp[Array[T]], start: Exp[Int], end: Exp[Int]): Exp[Array[T]] = InserationCoreI(v, start, end)


  case class QuickSortCore[T: Manifest](v: Exp[Array[T]], start: Exp[Int], end: Exp[Int], pivot: Exp[Int]) extends Def[Array[T]]

  def quicksortcore[T: Manifest](v: Exp[Array[T]], start: Exp[Int], end: Exp[Int], pivot: Exp[Int]): Exp[Array[T]] = QuickSortCore(v, start, end, pivot)

  case class QuickSorthack[T](x: Exp[Array[T]]) extends Def[Int]

  def quicksorthack[T](start: Exp[Array[T]]): Exp[Int] = QuickSorthack(start)


  //case class SumLoop[T: TypeRep](till: Exp[Int], body: Exp[ComplexVector]) extends Def[ComplexVector]

  //def sumLoop[T: TypeRep](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T])(implicit pos: SourceContext) = IfThenElse(cond, thenp, elsep)

  case class ChooseSort(size: Exp[Int]) extends Def[Int]

  def choose_sort(size: Exp[Int]): Exp[Int] = ChooseSort(size)

  case class ChooseInline(size: Exp[Int]) extends Def[Boolean]

  def choose_inlinex(size: Exp[Int]): Exp[Boolean] = ChooseInline(size)


  case class ChooseBase(size: Exp[Int]) extends Def[Int]

  def choose_base(size: Exp[Int]): Exp[Int] = ChooseBase(size)


  //case class Merge[T:Manifest](lhs: Exp[Vector[T]], rhs: Exp[Vector[T]]) extends Def[Vector[T]]
  //def merge[T:Manifest](lhs: Exp[Vector[T]], rhs: Exp[Vector[T]]): Exp[Vector[T]] = Merge(lhs,rhs)
  case class Merge[T: Manifest](lhs: Exp[Array[T]], rhs: Exp[Array[T]]) extends Def[Array[T]]

  def merge[T: Manifest](lhs: Exp[Array[T]], rhs: Exp[Array[T]]): Exp[Array[T]] = Merge(lhs, rhs)

  //case class Size[T](v: Rep[Vector[T]]) extends Def[Int]
  case class Size[T](v: Rep[Array[T]]) extends Def[Int]

  def size[T](v: Rep[Array[T]]): Rep[Int] = Size(v)

  //def size[T](v: Rep[Vector[T]]): Rep[Int] = Size(v)

  //case class Filter[T](v: Rep[Vector[T]], body: Exp[_ => _]) extends Def[Vector[T]]
  case class Filter[T](v: Rep[Array[T]], body: Exp[_ => _]) extends Def[Array[T]]

  def filter[T: Manifest](v: Rep[Array[T]], body: Rep[T] => Rep[Boolean])(implicit tupleexpose: ExposeRep[Rep[T]], singleexpose: ExposeRep[Rep[Boolean]]): Rep[Array[T]] = {
    //def filter[T:Manifest](v: Rep[Vector[T]],body: Rep[T] => Rep[Boolean])(implicit tupleexpose: ExposeRep[Rep[T]], singleexpose: ExposeRep[Rep[Boolean]]): Rep[Vector[T]] = {
    val lambda = doInternalLambda(body, false, None)(tupleexpose, singleexpose)
    val newsyms = singleexpose.freshExps()
    val looptuple = tupleexpose.freshExps()
    //val sumloopnode = SumFold(till, parallel, ini.y, loopvar, loopacc, lambda.exp)
    val sumloopnode = Filter[T](v, lambda.exp)
    val sumnodeexp = toAtom(sumloopnode)

    /*val returnNodes = if (newsyms.size > 1) {
      newsyms.zipWithIndex.map(fsym => {
        //had do to this ugly version since the compile time type is not know at this stage (of the return - Rep[_])
        val otp = exp2tp(fsym._1)
        val tag: TypeRep[Any] = otp.tag.asInstanceOf[TypeRep[Any]]
        val cc: Def[Any] = ReturnArg(sumnodeexp, fsym._1, fsym._2, true, newsyms.size == fsym._2 + 1)
        val newx = toAtom(cc)(tag, null)
        newx
      })
    } else {
      newsyms.zipWithIndex.map(fsym => {
        val tag: TypeRep[Any] = exp2tp(fsym._1).tag.asInstanceOf[TypeRep[Any]]
        val cc: Def[Any] = ReturnArg(sumnodeexp, fsym._1, fsym._2, false, true)
        val newx = toAtom(cc)(tag, null)
        newx
      })
    }
    val x1234 = singleexpose.vec2t(returnNodes)*/
    //Arg[Vector[T]]
    sumloopnode
  }

  case class FFor(from: Exp[Int], till: Exp[Int], loopvar: Exp[Int], body: Exp[_ => _]) extends Def[Any]

  def ffor[A](from: Rep[Int], till: Rep[Int], body: Rep[Int] => A)(implicit tupleexpose: ExposeRep[Rep[Int]], singleexpose: ExposeRep[A]): A = {
    val lambda = doInternalLambda(body, false, None)(tupleexpose, singleexpose)
    val newsyms = singleexpose.freshExps()
    val looptuple = tupleexpose.freshExps()
    val loopvar = looptuple.head.asInstanceOf[Exp[Int]]
    val sumloopnode = FFor(from, till, loopvar, lambda.exp)
    val sumnodeexp = toAtom(sumloopnode)

    val returnNodes = if (newsyms.size > 1) {
      newsyms.zipWithIndex.map(fsym => {
        //had do to this ugly version since the compile time type is not know at this stage (of the return - Rep[_])
        val otp = exp2tp(fsym._1)
        val tag: TypeRep[Any] = otp.tag.asInstanceOf[TypeRep[Any]]
        val cc: Def[Any] = ReturnArg(sumnodeexp, fsym._1, fsym._2, true, newsyms.size == fsym._2 + 1)
        val newx = toAtom(cc)(tag, null)
        newx
      })
    } else {
      newsyms.zipWithIndex.map(fsym => {
        val tag: TypeRep[Any] = exp2tp(fsym._1).tag.asInstanceOf[TypeRep[Any]]
        val cc: Def[Any] = ReturnArg(sumnodeexp, fsym._1, fsym._2, false, true)
        val newx = toAtom(cc)(tag, null)
        newx
      })
    }
    singleexpose.vec2t(returnNodes)
  }

}


trait ScalaGenSort_DSL extends ScalaCodegen with EmitHeadInternalFunctionAsClass {
  val IR: Sort_DSL

  import IR._

  var delay: Vector[(TP[_], Vector[String], (Block, Vector[String]) => Vector[String])] = Vector.empty
  var delaynow: Boolean = false
  val x = 10

  override def emitNode(tp: TP[_], acc: Vector[String],
                        block_callback: (Block, Vector[String]) => Vector[String]): Vector[String] = {
    val ma = tp.rhs match {
      case PixelGetAlpha(lhs) => Vector(emitValDef(tp, "((" + quote(lhs) + " >> 24) & 0xff)"))
      case PixelGetRed(lhs) => Vector(emitValDef(tp, "((" + quote(lhs) + " >> 16) & 0xff)"))
      case PixelGetGreen(lhs) => Vector(emitValDef(tp, "((" + quote(lhs) + " >> 8) & 0xff)"))
      case PixelGetBlue(lhs) => Vector(emitValDef(tp, "((" + quote(lhs) + " ) & 0xff)"))
      case CombinePixel(a, r, g, b) => {
        Vector(emitValDef(tp, "{ \n" +
          "val ia = 0xff\n" +
          "val ir = PixelUtils.clamp((" + quote(r) + "+0.5).toInt)\n" +
          "val ig = PixelUtils.clamp((" + quote(g) + "+0.5).toInt)\n" +
          "val ib = PixelUtils.clamp((" + quote(b) + "+0.5).toInt)\n" +
          "(ia << 24) | (ir << 16) | (ig << 8) | ib; \n } \n"

        )

        )
      }

      case FromInt(lhs) => Vector(emitValDef(tp, quote(lhs)))
      case ToDouble(lhs) => Vector(emitValDef(tp, quote(lhs) + ".toDouble"))
      case ToInt(lhs) => Vector(emitValDef(tp, quote(lhs) + ".toInt"))
      case Plus(lhs, rhs) => Vector(emitValDef(tp, quote(lhs) + " + " + quote(rhs)))
      case Times(lhs, rhs) => Vector(emitValDef(tp, quote(lhs) + " * " + quote(rhs)))
      case SetImage(img, x, y, p) => Vector(emitValDef(tp, quote(img) + ".set(" + quote(x) + "," + quote(y) + "," + quote(p) + ")"))
      case GetImage(img, x, y) => Vector(emitValDef(tp, quote(img) + ".get(" + quote(x) + "," + quote(y) + ")"))

      case Int_max(a, b) => Vector(emitValDef(tp, "Math.max(" + quote(a) + "," + quote(b) + ")"))
      case QuickSorthack(x) => Vector(emitValDef(tp, "Bla.uglyglobalj"))
      case Double_Compare(a, b) => Vector(emitValDef(tp, "if (" + quote(a) + " < " + quote(b) + ") -1 else if (" + quote(a) + " > " + quote(b) + ") 1 else 0"))
      case Int_Quick_Compare(a: Exp[Int], b: Exp[Int]) => Vector(emitValDef(tp, quote(b) + " - " + quote(a)))
      case QuickSortCore(v, s, e, p) => Vector(emitValDef(tp, "Bla.ref_quicksort(" + quote(v) + " , " + quote(s) + " , " + quote(e) + " , " + quote(p) + ")"))
      case InserationCoreI(v, s, e) => Vector(emitValDef(tp, "Bla.ref_insertioncore(" + quote(v) + " , " + quote(s) + " , " + quote(e) + ")"))
      case InserationCore(v, e) => Vector(emitValDef(tp, "Bla.insertioncore(" + quote(v) + " , " + quote(e) + ")"))
      case ChooseBase(size) => Vector(emitValDef(tp, " Bla.chooseBase(" + quote(size) + ")"))
      case ChooseSort(size) => Vector(emitValDef(tp, " Bla.chooseSort(" + quote(size) + ")"))
      case ChooseInline(size) => Vector(emitValDef(tp, quote(size) + "< 100"))
      case Merge(lhs, rhs) => Vector(emitValDef(tp, "Bla.merge(" + quote(lhs) + " , " + quote(rhs) + ")"))
      case Size(lhs) => Vector(emitValDef(tp, quote(lhs) + ".size"))
      case OrderingGTEQ(lhs, rhs) => Vector(emitValDef(tp, quote(lhs) + " >= " + quote(rhs)))
      case OrderingGT(lhs, rhs) => Vector(emitValDef(tp, quote(lhs) + " > " + quote(rhs)))
      case OrderingEquiv(lhs, rhs) => Vector(emitValDef(tp, quote(lhs) + " == " + quote(rhs)))
      case OrderingLT(lhs, rhs) => Vector(emitValDef(tp, quote(lhs) + " < " + quote(rhs)))

      case Concat(lhs, rhs) => Vector(emitValDef(tp, src"$lhs ++ $rhs"))
      case Until(start, end) => Vector(emitValDef(tp, src"$start until $end"))
      case BaseCase(n: Exp[Int]) => Vector(emitValDef(tp, quote(n) + " == 2 //check for base case"))
      case IsPrime(n: Exp[Int]) => Vector(emitValDef(tp, " false //put prime factor check here"))
      //case VecCreate(n: Exp[Int]) => Vector(emitValDef(tp, "new Array[Double](" + quote(n) + ") //buffer creation"))
      case IVecAddStride(v: Exp[Vector[Int]], y: Exp[Int], b: Exp[Int]) => Vector(emitValDef(tp, quote(v) + ".dropRight(1) :+ " + quote(v) + ".last / " + quote(y) + "/// ADD STride " + quote(b)))
      case IVecCreate(n: Exp[Int]) => Vector(emitValDef(tp, "Vector.empty[Int] //creating vector with " + quote(n)))
      case IVecAppend(v: Exp[Vector[Int]], y: Exp[Int]) => Vector(emitValDef(tp, quote(v) + " :+ " + quote(y)))
      case IVecApply(vec, i) => Vector(emitValDef(tp, quote(vec) + "(" + quote(i) + ")"))
      case IVecZipMagic(r, s) => Vector(emitValDef(tp, "Vector(" + quote(r) + ".headOption.getOrElse(0) * " + quote(s) + ".headOption.getOrElse(0)) ++ " + quote(r) + ".tail.zipAll(" + quote(s) + ".tail,0,0).map(p => p._1 + " + quote(r) + ".headOption.getOrElse(0) * p._2)"))
      case IVecMult(b, s, l) => Vector(emitValDef(tp, " VectorMult(" + quote(b) + "," + quote(s) + "," + quote(l) + ")"))
      case IVecFirstorZero(v) => Vector(emitValDef(tp, quote(v) + ".headOption.getOrElse(0)"))
      case IVecUpRank(v) => Vector(emitValDef(tp, "Vector(" + quote(v) + ".head, 0) ++ " + quote(v) + ".tail"))
      case Int_Eq(lhs, rhs) => Vector(emitValDef(tp, quote(lhs) + " == " + quote(rhs)))
      //case Divide(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " / " + quote(rhs)))
      case Radix(n: Exp[Int]) => Vector(emitValDef(tp, quote(n) + " / 2 //stupid radix choice placeholder"))


      case Filter(v, body) => {
        val bodylambda = exp2tp(body)
        val rets: Vector[String] = bodylambda.rhs match {
          case InternalLambda(tf, tx, ty, thot, targs, treturns) => Vector({
            //val l1 = "val " + quote(tp) + " = ("+ quote(loopvar) + " <- 0 until "+ quote(till) + ").foldLeft(Vector.empty) {\n "
            val helper = if (tx.size > 1) {
              tx.zipWithIndex.map(a => {
                val (tp, index) = a
                val typ = remap(tp.tag.mf)
                "val " + quote(tp) + " : " + remap(tp.tag) + " = helper" + tupleaccesshelper(index, "", index == tx.size - 1)
              }).mkString("\n")
            } else {
              //"val " + quote(x.head) + " : " + remap(x.head.tag.mf) + " = helper\n"
              "val " + quote(tx.head) + " : " + remap(tx.head.tag) + " = helper\n"
            }
            val argtuple = tupledeclarehelper(tx.map(a => remap(a.tag)), "")
            val l1 = "val " + quote(tp) + " = (" + quote(v) + ").filter( (helper) => {\n \n"
            val l10 = l1 + "\n" + helper + "\n"
            val l2 = block_callback(ty, Vector(l10))
            val trestuple: Vector[String] = ty.res.map(r => quote(r))
            val l3: String = l2.mkString("") + tupledeclarehelper(trestuple, "")
            val l4 = l3 + "\n})\n"
            l4
          })
          case _ => {
            assert(false, "got an SumLoop statment which does not contain a lambda")
            Vector.empty
          }
        }
        rets
      }

      case RangeFoldLeft(expose, r: Exp[Range], ini, loopvar: Exp[Int], loopacc, body) => {
        val bodylambda = exp2tp(body)
        val rets: Vector[String] = bodylambda.rhs match {
          case InternalLambda(tf, tx, ty, thot, targs, treturns) => Vector({
            //val l1 = "val " + quote(tp) + " = ("+ quote(loopvar) + " <- 0 until "+ quote(till) + ").foldLeft(Vector.empty) {\n "
            val helper = if (tx.size > 1) {
              tx.zipWithIndex.map(a => {
                val (tp, index) = a
                val typ = remap(tp.tag.mf)
                "val " + quote(tp) + " : " + remap(tp.tag) + " = helper" + tupleaccesshelper(index, "", index == tx.size - 1)
              }).mkString("\n")
            } else {
              //"val " + quote(x.head) + " : " + remap(x.head.tag.mf) + " = helper\n"
              "val " + quote(tx.head) + " : " + remap(tx.head.tag) + " = helper\n"
            }
            val argtuple = tupledeclarehelper(tx.map(a => remap(a.tag)), "")
            val iniexps = expose.t2vec(ini)
            val l1 = if (iniexps.size > 1) {
              if (iniexps.size > 2) ???
              "val " + quote(tp) + " = (" + quote(r) + ").foldLeft( (" + iniexps.map(e => quote(e)).mkString(",") + ") )(\n  (acc,ele) => {\n val helper = (acc._1,acc._2,ele)\n"

            } else "val " + quote(tp) + " = (" + quote(r) + ").foldLeft( (" + iniexps.map(e => quote(e)).mkString(",") + ") )(\n  (acc,ele) => {\n val helper = (acc,ele)\n"


            val l10 = l1 + "\n" + helper + "\n"
            val l2 = block_callback(ty, Vector(l10))
            val trestuple: Vector[String] = ty.res.map(r => quote(r))
            val l3: String = l2.mkString("") + tupledeclarehelper(trestuple, "")
            val l4 = l3 + "\n})\n"
            l4
          })
          case _ => {
            assert(false, "got an SumLoop statment which does not contain a lambda")
            Vector.empty
          }
        }
        rets
      }


      case _ => {
        println(tp)
        super.emitNode(tp, acc, block_callback)
      }
    }
    ma
  }
}



