
package SpiralSThesis


import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._

trait Spiral_DSL extends BaseExp with FunctionsExp with OrderingOpsExp with BooleanOpsExpOpt with IfThenElsePureExp with PurePrimitiveOpsExpOpt with ImplicitOpsExp with ScalaCompile {

  case class ListAdd(l: Exp[List[Int]], i: Exp[Int]) extends Def[List[Int]]

  def listadd(l: Exp[List[Int]], i: Exp[Int]): Exp[List[Int]] = ListAdd(l, i)

  case class ISingle(s: Single, i: Rep[Int])

  case class Single(y: Rep[ComplexVector])

  case class Lookupsize2id(n: Exp[Int]) extends Def[Int]

  def lookupsize2id(n: Exp[Int]): Exp[Int] = Lookupsize2id(n)

  case class Lookupid2lid(n: Exp[Int]) extends Def[Int]

  def lookupid2lid(n: Exp[Int]): Exp[Int] = Lookupid2lid(n)

  case class Lookupid2rid(n: Exp[Int]) extends Def[Int]

  def lookupid2rid(n: Exp[Int]): Exp[Int] = Lookupid2rid(n)


  case class Twid(n: Exp[List[Int]]) extends Def[Int]

  def choose_twid(n: Exp[List[Int]]): Exp[Int] = Twid(n)

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

  case class Twiddle_Apply(vec: Exp[ComplexVector], size: Exp[Int], n: Exp[Int], d: Exp[Int], k: Exp[Int]) extends Def[ComplexVector]

  def twiddle_apply(vec: Exp[ComplexVector], size: Exp[Int], n: Exp[Int], d: Exp[Int], k: Exp[Int]): Exp[ComplexVector] = Twiddle_Apply(vec, size, n, d, k)


  case class Twiddle_Apply_Index(n: Exp[Int], d: Exp[Int], k: Exp[Int], i: Exp[Int]) extends Def[Complex]

  def twiddle_apply_index(n: Exp[Int], d: Exp[Int], k: Exp[Int], i: Exp[Int]): Exp[Complex] = Twiddle_Apply_Index(n, d, k, i)

  case class Twiddle_Load(i: Exp[Int]) extends Def[Complex]

  def twiddle_load(i: Exp[Int]): Exp[Complex] = Twiddle_Load(i)

  case class Twiddle_Apply_Index_Store(n: Exp[Int], d: Exp[Int], k: Exp[Int], i: Exp[Int]) extends Def[Complex]

  def twiddle_apply_index_store(n: Exp[Int], d: Exp[Int], k: Exp[Int], i: Exp[Int]): Exp[Complex] = Twiddle_Apply_Index_Store(n, d, k, i)


  case class DTwiddle_Apply_Index(n: Exp[Int], d: Exp[Int], k: Exp[Int], i: Exp[Int], re: Boolean) extends Def[Double]

  def dtwiddle_apply_index(n: Exp[Int], d: Exp[Int], k: Exp[Int], i: Exp[Int], re: Boolean): Exp[Double] = DTwiddle_Apply_Index(n, d, k, i, re)

  case class DTwiddle_Load(i: Exp[Int]) extends Def[Double]

  def dtwiddle_load(i: Exp[Int]): Exp[Double] = DTwiddle_Load(i)

  case class DTwiddle_Apply_Index_Store(n: Exp[Int], d: Exp[Int], k: Exp[Int], i: Exp[Int], re: Boolean) extends Def[Double]

  def dtwiddle_apply_index_store(n: Exp[Int], d: Exp[Int], k: Exp[Int], i: Exp[Int], re: Boolean): Exp[Double] = DTwiddle_Apply_Index_Store(n, d, k, i, re)

  case class DTwiddle_Apply_Index_Load(n: Exp[Int], d: Exp[Int], k: Exp[Int], i: Exp[Int], re: Boolean) extends Def[Double]

  def dtwiddle_apply_index_load(n: Exp[Int], d: Exp[Int], k: Exp[Int], i: Exp[Int], re: Boolean): Exp[Double] = DTwiddle_Apply_Index_Load(n, d, k, i, re)


  case class VecCreate(s: Exp[Int]) extends Def[ComplexVector]

  def veccreate(i: Exp[Int]): Exp[ComplexVector] = VecCreate(i)

  case class DVecCreate(s: Exp[Int]) extends Def[Array[Double]]

  def dveccreate(i: Exp[Int]): Exp[Array[Double]] = DVecCreate(i)

  case class CompCreate(re: Exp[Double], im: Exp[Double]) extends Def[Complex]

  def compcreate(re: Exp[Double], im: Exp[Double]): Exp[Complex] = CompCreate(re, im)

  case class CompRe(x: Exp[Complex]) extends Def[Double]

  def compre(x: Exp[Complex]): Exp[Double] = CompRe(x)

  case class CompIm(x: Exp[Complex]) extends Def[Double]

  def compim(x: Exp[Complex]): Exp[Double] = CompIm(x)

  case class DVecApply(vec: Exp[Array[Double]], i: Exp[Int]) extends Def[Double]

  def dvecapply(vec: Exp[Array[Double]], i: Exp[Int]): Exp[Double] = DVecApply(vec, i)

  case class VecApply(vec: Exp[ComplexVector], i: Exp[Int]) extends Def[Complex]

  def vecapply(vec: Exp[ComplexVector], i: Exp[Int]): Exp[Complex] = VecApply(vec, i)

  case class VecUpdate(vec: Exp[ComplexVector], i: Exp[Int], y: Exp[Complex]) extends Def[ComplexVector]

  def vecupdate(vec: Exp[ComplexVector], i: Exp[Int], y: Exp[Complex]): Exp[ComplexVector] = VecUpdate(vec, i, y)

  case class DVecUpdate(vec: Exp[Array[Double]], i: Exp[Int], y: Exp[Double]) extends Def[Array[Double]]

  def dvecupdate(vec: Exp[Array[Double]], i: Exp[Int], y: Exp[Double]): Exp[Array[Double]] = DVecUpdate(vec, i, y)

  case class VecSame(x: Exp[ComplexVector], y: Exp[ComplexVector]) extends Def[ComplexVector]

  def vecsame(x: Exp[ComplexVector], y: Exp[ComplexVector]): Exp[ComplexVector] = VecSame(x, y)

  case class DVecSame(x: Exp[Array[Double]], y: Exp[Array[Double]]) extends Def[Array[Double]]

  def dvecsame(x: Exp[Array[Double]], y: Exp[Array[Double]]): Exp[Array[Double]] = DVecSame(x, y)

  case class Plus(lhs: Exp[Complex], rhs: Exp[Complex]) extends Def[Complex]

  def plus(lhs: Exp[Complex], rhs: Exp[Complex]): Exp[Complex] = Plus(lhs, rhs)

  case class Minus(lhs: Exp[Complex], rhs: Exp[Complex]) extends Def[Complex]

  def minus(lhs: Exp[Complex], rhs: Exp[Complex]): Exp[Complex] = Minus(lhs, rhs)

  case class Times(lhs: Exp[Complex], rhs: Exp[Complex]) extends Def[Complex]

  def times(lhs: Exp[Complex], rhs: Exp[Complex]): Exp[Complex] = Times(lhs, rhs)

  //case class SumLoop[T: TypeRep](till: Exp[Int], body: Exp[ComplexVector]) extends Def[ComplexVector]

  //def sumLoop[T: TypeRep](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T])(implicit pos: SourceContext) = IfThenElse(cond, thenp, elsep)

  case class SumFold(till: Exp[Int], parllel: Boolean, ini: Exp[ComplexVector], loopvar: Exp[Int], loopacc: Exp[ComplexVector], body: Exp[_ => _]) extends Def[ComplexVector]


  case class SigmaLoop[A, R](till: Exp[Int], parllel: Option[Int], ini: Exp[_], out: Exp[_], tupleexpose: ExposeRep[A], body: Exp[_ => _]) extends Def[Any]

  def sigmaLoop[A, R](till: Rep[Int], parallel: Option[Int], ini: Exp[_], out: Exp[_], body: A => R)(implicit tupleexpose: ExposeRep[A], singleexpose: ExposeRep[R]): R = {
    val lambda = doInternalLambda(body, false, None)(tupleexpose, singleexpose)
    val newsyms = singleexpose.freshExps()
    //val looptuple = tupleexpose.freshExps()
    //val loopvar = looptuple.head.asInstanceOf[Exp[Int]]
    //val loopacc = looptuple.tail.head.asInstanceOf[Exp[ComplexVector]]
    val sumloopnode = SigmaLoop(till, parallel, ini, out, tupleexpose, lambda.exp)
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


  case class SumFoldX[R](till: Exp[Int], parllel: Option[Int], ini: Exp[_], out: Exp[_], loopvar: Exp[Int], loopacc: Exp[ComplexVector], body: Exp[_ => _]) extends Def[ComplexVector]

  def sumFoldx[A, R](till: Rep[Int], parallel: Option[Int], ini: Exp[_], out: Exp[_], body: A => R)(implicit tupleexpose: ExposeRep[A], singleexpose: ExposeRep[R]): R = {
    val lambda = doInternalLambda(body, false, None)(tupleexpose, singleexpose)
    val newsyms = singleexpose.freshExps()
    val looptuple = tupleexpose.freshExps()
    val loopvar = looptuple.head.asInstanceOf[Exp[Int]]
    val loopacc = looptuple.tail.head.asInstanceOf[Exp[ComplexVector]]
    val sumloopnode = SumFoldX(till, parallel, ini, out, loopvar, loopacc, lambda.exp)
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


  def sumFold[A](till: Rep[Int], parallel: Boolean, ini: Single, body: ISingle => Single)(implicit tupleexpose: ExposeRep[ISingle], singleexpose: ExposeRep[Single]): Single = {
    val lambda = doInternalLambda(body, false, None)(tupleexpose, singleexpose)
    val newsyms = singleexpose.freshExps()
    val looptuple = tupleexpose.freshExps()
    val loopvar = looptuple.head.asInstanceOf[Exp[Int]]
    val loopacc = looptuple.tail.head.asInstanceOf[Exp[ComplexVector]]
    val sumloopnode = SumFold(till, parallel, ini.y, loopvar, loopacc, lambda.exp)
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

trait ScalaGenSpiral_DSL extends ScalaCodegen with TupleHelper /*with EmitHeadInternalFunctionAsClass  */ {
  val IR: Spiral_DSL

  import IR._

  var delay: Vector[(TP[_], Vector[String], (Block, Vector[String]) => Vector[String])] = Vector.empty
  var delaynow: Boolean = false


  override def emitNode(tp: TP[_], acc: Vector[String],
                        block_callback: (Block, Vector[String]) => Vector[String]): Vector[String] = {
    val ma = tp.rhs match {
      case CompRe(x: Exp[Complex]) => Vector(emitValDef(tp, s"${quote(x)}.re"))
      case CompIm(x: Exp[Complex]) => Vector(emitValDef(tp, s"${quote(x)}.im"))
      case CompCreate(re: Exp[Double], im: Exp[Double]) => Vector(emitValDef(tp, "Complex(" + quote(re) + "," + quote(im) + ")"))
      case BaseCase(n: Exp[Int]) => Vector(emitValDef(tp, quote(n) + " == 2 //check for base case"))
      case IsPrime(n: Exp[Int]) => Vector(emitValDef(tp, " false //put prime factor check here"))
      //case VecCreate(n: Exp[Int]) => Vector(emitValDef(tp, "new Array[Double](" + quote(n) + ") //buffer creation"))
      case VecCreate(n: Exp[Int]) => Vector(emitValDef(tp, "new ComplexVector(new Array[Complex](" + quote(n) + ")) //buffer creation"))
      case VecApply(vec: Exp[ComplexVector], i: Exp[Int]) => Vector(emitValDef(tp, "" + quote(vec) + "(" + quote(i) + ")"))
      case VecSame(x: Exp[ComplexVector], y: Exp[ComplexVector]) => Vector(emitValDef(tp, "" + quote(x)))
      case VecUpdate(vec: Exp[ComplexVector], i: Exp[Int], y: Exp[Complex]) => Vector(emitValDef(tp, "" + quote(vec) + ".update(" + quote(i) + "," + quote(y) + ")"))

      case DVecCreate(n: Exp[Int]) => Vector(emitValDef(tp, "new Array[Double](2*" + quote(n) + ") //buffer creation"))
      case DVecApply(vec: Exp[Array[Double]], i: Exp[Int]) => Vector(emitValDef(tp, "" + quote(vec) + "(" + quote(i) + ")"))
      case DVecSame(x: Exp[Array[Double]], y: Exp[Array[Double]]) => Vector(emitValDef(tp, "" + quote(x)))
      case DVecUpdate(vec: Exp[Array[Double]], i: Exp[Int], y: Exp[Double]) => Vector(emitValDef(tp, "{" + quote(vec) + ".update(" + quote(i) + "," + quote(y) + "); " + quote(vec) + "}"))
      case IVecAddStride(v: Exp[Vector[Int]], y: Exp[Int], b: Exp[Int]) => Vector(emitValDef(tp, quote(v) + ".dropRight(1) :+ " + quote(v) + ".last / " + quote(y) + "/// ADD STride " + quote(b)))
      case IVecCreate(n: Exp[Int]) => Vector(emitValDef(tp, "Vector.empty[Int] //creating vector with " + quote(n)))
      case IVecAppend(v: Exp[Vector[Int]], y: Exp[Int]) => Vector(emitValDef(tp, quote(v) + " :+ " + quote(y)))
      case IVecApply(vec, i) => Vector(emitValDef(tp, quote(vec) + "(" + quote(i) + ")"))
      case IVecZipMagic(r, s) => Vector(emitValDef(tp, "Vector(" + quote(r) + ".headOption.getOrElse(0) * " + quote(s) + ".headOption.getOrElse(0)) ++ " + quote(r) + ".tail.zipAll(" + quote(s) + ".tail,0,0).map(p => p._1 + " + quote(r) + ".headOption.getOrElse(0) * p._2)"))
      case IVecMult(b, s, l) => Vector(emitValDef(tp, " VectorMult(" + quote(b) + "," + quote(s) + "," + quote(l) + ")"))
      case IVecFirstorZero(v) => Vector(emitValDef(tp, quote(v) + ".headOption.getOrElse(0)"))
      case IVecUpRank(v) => Vector(emitValDef(tp, "Vector(" + quote(v) + ".head, 0) ++ " + quote(v) + ".tail"))
      case Int_Eq(lhs, rhs) => Vector(emitValDef(tp, quote(lhs) + " == " + quote(rhs)))
      case Plus(lhs, rhs) => Vector(emitValDef(tp, quote(lhs) + " + " + quote(rhs)))
      case Minus(lhs, rhs) => Vector(emitValDef(tp, quote(lhs) + " - " + quote(rhs)))
      case Times(lhs, rhs) => Vector(emitValDef(tp, quote(lhs) + " * " + quote(rhs)))
      //case Divide(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " / " + quote(rhs)))
      case Twiddle_Apply(vec: Exp[ComplexVector], size: Exp[Int], n: Exp[Int], d: Exp[Int], k: Exp[Int]) => Vector(emitValDef(tp, " Twiddle(" + quote(vec) + "," + quote(n) + "," + quote(d) + "," + quote(k) + ")"))
      case Twiddle_Apply_Index(n: Exp[Int], d: Exp[Int], k: Exp[Int], i: Exp[Int]) => Vector(emitValDef(tp, " Twiddle(" + quote(n) + "," + quote(d) + "," + quote(k) + "," + quote(i) + ")"))
      case Twiddle_Apply_Index_Store(n: Exp[Int], d: Exp[Int], k: Exp[Int], i: Exp[Int]) => Vector(emitValDef(tp, " Twiddle.store(" + quote(n) + "," + quote(d) + "," + quote(k) + "," + quote(i) + ")"))
      case Twiddle_Load(i: Exp[Int]) => Vector(emitValDef(tp, " Twiddle.load()"))

      case DTwiddle_Apply_Index(n: Exp[Int], d: Exp[Int], k: Exp[Int], i: Exp[Int], re: Boolean) => {
        val t = if (re) {
          ".re"
        } else {
          ".im"
        }
        Vector(emitValDef(tp, " Twiddle(" + quote(n) + "," + quote(d) + "," + quote(k) + "," + quote(i) + ")" + t))
      }
      case DTwiddle_Apply_Index_Store(n: Exp[Int], d: Exp[Int], k: Exp[Int], i: Exp[Int], re: Boolean) => {
        Vector(emitValDef(tp, " Twiddle.dstore(" + quote(n) + "," + quote(d) + "," + quote(k) + "," + quote(i) + "," + re + ")"))
      }

      case DTwiddle_Apply_Index_Load(n: Exp[Int], d: Exp[Int], k: Exp[Int], i: Exp[Int], re: Boolean) => {
        Vector(emitValDef(tp, " Twiddle.dload(" + quote(n) + "," + quote(d) + "," + quote(k) + "," + quote(i) + "," + (if(re) "0" else "1") + ")"))
      }

      case DTwiddle_Load(i: Exp[Int]) => Vector(emitValDef(tp, " Twiddle.dload()"))


      //case Radix(n: Exp[Int]) => Vector(emitValDef(tp, quote(n) + " / 2 //stupid radix choice placeholder"))
      case Lookupid2lid(n: Exp[Int]) => Vector(emitValDef(tp, s"Settings.id2ids.getOrElse(${quote(n)},(-99,-99))._1"))
      case Lookupid2rid(n: Exp[Int]) => Vector(emitValDef(tp, s"Settings.id2ids.getOrElse(${quote(n)},(-99,-99))._2"))
      case Lookupsize2id(n: Exp[Int]) => Vector(emitValDef(tp, s"Settings.size2id.getOrElse(${quote(n)},-99)"))
      case Radix(l: Exp[Int]) => Vector(emitValDef(tp, "Settings.size2radix.getOrElse(" + quote(l) + ",2)"))

      case Twid(l: Exp[List[Int]]) => Vector(emitValDef(tp, "{val t = Settings.decompchoice.getOrElse(" + quote(l) + ",{ val t: (Int,Boolean,Boolean) = ???; \nt})._3\nif(t) 1 else 0}\n"))
      case ListAdd(l: Exp[List[Int]], i: Exp[Int]) => Vector(emitValDef(tp, quote(l) + ":+ " + quote(i)))

      case SumFold(till: Exp[Int], parllel: Boolean, ini: Exp[ComplexVector], loopvar: Exp[Int], acc: Exp[ComplexVector], body) => {
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
            val l1 = if (parllel)
              "val " + quote(tp) + " = (0 until " + quote(till) + ").par.foldLeft( " + quote(ini) + " )(\n  (acc,ele) => {\n val helper = (acc,ele)\n"
            else
              "val " + quote(tp) + " = (0 until " + quote(till) + ").foldLeft( " + quote(ini) + " )(\n  (acc,ele) => {\n val helper = (acc,ele)\n"
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

      case SumFoldX(till: Exp[Int], parllel: Option[Int], ini: Exp[_], out: Exp[_], loopvar: Exp[Int], acc: Exp[ComplexVector], body) => {
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
            val l1 = parllel.fold[String]("val " + quote(tp) + " = (0 until " + quote(till) + ").foldLeft( " + quote(ini) + " )(\n  (acc,ele) => {\n val helper = (acc,ele)\n")(nrthreads => {
              //"val " + quote(tp) + " = (0 until " + quote(till) + ").par.foldLeft( " + quote(ini) + " )(\n  (acc,ele) => {\n val helper = (acc,ele)\n"
              "val " + quote(tp) + " = Twiddle.parloop(" + quote(till) + "," + nrthreads + "," + quote(ini) + "," + quote(out) + ",(x: (SpiralSThesis.ComplexVector,Int)) => {\n        val helper = x\n "
            })


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


      case SigmaLoop(till: Exp[Int], parallel: Option[Int], in: Exp[_], out: Exp[_], exparg: ExposeRep[_], body) => {
        val bodylambda = exp2tp(body)


        val rets: Vector[String] = bodylambda.rhs match {
          case InternalLambda(tf, tx, ty, thot, targs, treturns) => Vector({
            //val l1 = "val " + quote(tp) + " = ("+ quote(loopvar) + " <- 0 until "+ quote(till) + ").foldLeft(Vector.empty) {\n "


            val helper_old = if (tx.size > 1) {
              tx.zipWithIndex.map(a => {
                val (tp, index) = a
                val typ = remap(tp.tag.mf)
                "val " + quote(tp) + " : " + remap(tp.tag) + " = helper" + tupleaccesshelper(index, "", index == tx.size - 1)
              }).mkString("\n")
            } else {
              //"val " + quote(x.head) + " : " + remap(x.head.tag.mf) + " = helper\n"
              "val " + quote(tx.head) + " : " + remap(tx.head.tag) + " = helper\n"
            }
            val helper = if (tx.size > 1) {
              tx.zipWithIndex.map(a => {
                val (tp, index) = a
                val typ = remap(tp.tag.mf)
                "val " + quote(tp) + " : " + remap(tp.tag) + s" = helper$index"
              }).mkString("\n")
            } else {
              //"val " + quote(x.head) + " : " + remap(x.head.tag.mf) + " = helper\n"
              "val " + quote(tx.head) + " : " + remap(tx.head.tag) + " = helper\n"
            }
            val argtuple = tupledeclarehelper(tx.map(a => remap(a.tag)), "")

            parallel.fold[String]({
              val l1 =
                s"""val ${quote(tp)} = {var lc = 0
                   |while(lc < ${quote(till)}){
                   |val helper0 = lc
                   | val helper1 = ${quote(in)}
                   | val helper2 = ${quote(out)}""".stripMargin
              val l10 = l1 + "\n" + helper + "\n"
              val l2 = block_callback(ty, Vector(l10))
              val trestuple: Vector[String] = ty.res.map(r => quote(r))
              val l3: String = l2.mkString("") //+ tupledeclarehelper(trestuple, "")
              val l4 = l3 + s"\n lc = lc + 1\n};\n${quote(out)} }\n"
              l4
            })(nrthreads => {
              val l1 = s"val ${quote(tp)} = Twiddle.parloop(${quote(till)},$nrthreads,${quote(in)},${quote(out)},(lc: Int) => {\n val helper0 = lc\n val helper1 =${quote(in)}\n val helper2 = ${quote(out)}\n "
              val l10 = l1 + "\n" + helper + "\n"
              val l2 = block_callback(ty, Vector(l10))
              val trestuple: Vector[String] = ty.res.map(r => quote(r))
              val l3: String = l2.mkString("") + tupledeclarehelper(trestuple, "")
              val l4 = l3 + s"\n}) \n"
              l4
            })



            })
            case _ => {
              assert(false, "got an SumLoop statment which does not contain a lambda")
              Vector.empty
            }
          }
            rets
        }


        case _ => {

          super.emitNode(tp, acc, block_callback)
        }
      }
        ma
    }
  }


