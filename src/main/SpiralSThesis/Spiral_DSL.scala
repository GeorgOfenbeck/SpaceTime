package SpiralSThesis

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.scalalike._

trait Spiral_DSL extends BaseExp with FunctionsExp with OrderingOpsExp with BooleanOpsExpOpt with IfThenElsePureExp with PurePrimitiveOpsExpOpt with ImplicitOpsExp with ScalaCompile {

  case class ISingle(s: Single, i: Rep[Int])

  case class Single(y: Rep[ComplexVector])

  case class Radix(n: Exp[Int]) extends Def[Int]

  def choose_radix(n: Exp[Int]): Exp[Int] = Radix(n)

  case class DTwiddle_Apply_Index(n: Exp[Int], d: Exp[Int], k: Exp[Int], i: Exp[Int], re: Boolean) extends Def[Double]

  def dtwiddle_apply_index(n: Exp[Int], d: Exp[Int], k: Exp[Int], i: Exp[Int], re: Boolean): Exp[Double] = DTwiddle_Apply_Index(n, d, k, i, re)

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

  case class Plus(lhs: Exp[Complex], rhs: Exp[Complex]) extends Def[Complex]

  def plus(lhs: Exp[Complex], rhs: Exp[Complex]): Exp[Complex] = Plus(lhs, rhs)

  case class Minus(lhs: Exp[Complex], rhs: Exp[Complex]) extends Def[Complex]

  def minus(lhs: Exp[Complex], rhs: Exp[Complex]): Exp[Complex] = Minus(lhs, rhs)

  case class Times(lhs: Exp[Complex], rhs: Exp[Complex]) extends Def[Complex]

  def times(lhs: Exp[Complex], rhs: Exp[Complex]): Exp[Complex] = Times(lhs, rhs)

  case class SumFold(till: Exp[Int], parllel: Boolean, ini: Exp[ComplexVector], loopvar: Exp[Int], loopacc: Exp[ComplexVector], body: Exp[_ => _]) extends Def[ComplexVector]

  case class SigmaLoop[A, R](till: Exp[Int], parllel: Option[Int], ini: Exp[_], out: Exp[_], tupleexpose: ExposeRep[A], body: Exp[_ => _]) extends Def[Any]

  def sigmaLoop[A, R](till: Rep[Int], parallel: Option[Int], ini: Exp[_], out: Exp[_], body: A => R)(implicit tupleexpose: ExposeRep[A], singleexpose: ExposeRep[R]): R = {
    val lambda = doInternalLambda(body, false, None)(tupleexpose, singleexpose)
    val newsyms = singleexpose.freshExps()
    val sumloopnode = SigmaLoop(till, parallel, ini, out, tupleexpose, lambda.exp)
    val sumnodeexp = toAtom(sumloopnode)
    val returnNodes = if (newsyms.size > 1) {
      newsyms.zipWithIndex.map(fsym => { //had do to this ugly version since the compile time type is not know at this stage (of the return - Rep[_])
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
  override def emitNode(tp: TP[_], acc: Vector[String],
                        block_callback: (Block, Vector[String]) => Vector[String]): Vector[String] = {
    val ma = tp.rhs match {
      case CompRe(x: Exp[Complex]) => Vector(emitValDef(tp, s"${quote(x)}.re"))
      case CompIm(x: Exp[Complex]) => Vector(emitValDef(tp, s"${quote(x)}.im"))
      case CompCreate(re: Exp[Double], im: Exp[Double]) => Vector(emitValDef(tp, "Complex(" + quote(re) + "," + quote(im) + ")"))
      case VecCreate(n: Exp[Int]) => Vector(emitValDef(tp, "new ComplexVector(new Array[Complex](" + quote(n) + ")) //buffer creation"))
      case VecApply(vec: Exp[ComplexVector], i: Exp[Int]) => Vector(emitValDef(tp, "" + quote(vec) + "(" + quote(i) + ")"))
      case VecUpdate(vec: Exp[ComplexVector], i: Exp[Int], y: Exp[Complex]) => Vector(emitValDef(tp, "" + quote(vec) + ".update(" + quote(i) + "," + quote(y) + ")"))
      case DVecCreate(n: Exp[Int]) => Vector(emitValDef(tp, "new Array[Double](2*" + quote(n) + ") //buffer creation"))
      case DVecApply(vec: Exp[Array[Double]], i: Exp[Int]) => Vector(emitValDef(tp, "" + quote(vec) + "(" + quote(i) + ")"))
      case DVecUpdate(vec: Exp[Array[Double]], i: Exp[Int], y: Exp[Double]) => Vector(emitValDef(tp, "{" + quote(vec) + ".update(" + quote(i) + "," + quote(y) + "); " + quote(vec) + "}"))
      case Plus(lhs, rhs) => Vector(emitValDef(tp, quote(lhs) + " + " + quote(rhs)))
      case Minus(lhs, rhs) => Vector(emitValDef(tp, quote(lhs) + " - " + quote(rhs)))
      case Times(lhs, rhs) => Vector(emitValDef(tp, quote(lhs) + " * " + quote(rhs)))
      case DTwiddle_Apply_Index(n: Exp[Int], d: Exp[Int], k: Exp[Int], i: Exp[Int], re: Boolean) =>
        Vector(emitValDef(tp, " Twiddle(" + quote(n) + "," + quote(d) + "," + quote(k) + "," + quote(i) + ")" + {
          if (re) ".re" else ".im"
        }))
      case DTwiddle_Apply_Index_Load(n: Exp[Int], d: Exp[Int], k: Exp[Int], i: Exp[Int], re: Boolean) =>
        Vector(emitValDef(tp, " Twiddle.dload(" + quote(n) + "," + quote(d) + "," + quote(k) + "," + quote(i) + "," + (if (re) "0" else "1") + ")"))
      case Radix(l: Exp[Int]) => Vector(emitValDef(tp, "Settings.size2radix.getOrElse(" + quote(l) + ",2)"))
      case SumFold(till: Exp[Int], parllel: Boolean, ini: Exp[ComplexVector], loopvar: Exp[Int], acc: Exp[ComplexVector], body) => {
        val bodylambda = exp2tp(body)
        val rets: Vector[String] = bodylambda.rhs match {
          case InternalLambda(tf, tx, ty, thot, targs, treturns) => Vector({
            val helper = if (tx.size > 1) {
              tx.zipWithIndex.map(a => {
                val (tp, index) = a
                val typ = remap(tp.tag.mf)
                "val " + quote(tp) + " : " + remap(tp.tag) + " = helper" + tupleaccesshelper(index, "", index == tx.size - 1)
              }).mkString("\n")
            } else {
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
      case SigmaLoop(till: Exp[Int], parallel: Option[Int], in: Exp[_], out: Exp[_], exparg: ExposeRep[_], body) => {
        val bodylambda = exp2tp(body)
        val rets: Vector[String] = bodylambda.rhs match {
          case InternalLambda(tf, tx, ty, thot, targs, treturns) => Vector({
            val helper_old = if (tx.size > 1) {
              tx.zipWithIndex.map(a => {
                val (tp, index) = a
                val typ = remap(tp.tag.mf)
                "val " + quote(tp) + " : " + remap(tp.tag) + " = helper" + tupleaccesshelper(index, "", index == tx.size - 1)
              }).mkString("\n")
            } else {
              "val " + quote(tx.head) + " : " + remap(tx.head.tag) + " = helper\n"
            }
            val helper = if (tx.size > 1) {
              tx.zipWithIndex.map(a => {
                val (tp, index) = a
                val typ = remap(tp.tag.mf)
                "val " + quote(tp) + " : " + remap(tp.tag) + s" = helper$index"
              }).mkString("\n")
            } else {
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
      case _ => super.emitNode(tp, acc, block_callback)
    }
    ma
  }
}


