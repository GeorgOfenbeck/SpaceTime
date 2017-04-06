package scala.lms
package targets
package graphviz

import scala.lms.internal._

import scala.lms.internal.FunctionsExp
import scala.lms.ops._
import scala.lms.targets.scalalike._

trait MyRange extends PurePrimitiveOpsExp with FunctionsExp with ImplicitOpsExp {

  case class RangeMap[T: Manifest](start: Exp[Int], end: Exp[Int], body: Exp[_ => _]) extends Def[IndexedSeq[T]]

  def range_map[T](s: Exp[Int], end: Exp[Int], block: Exp[Int] => Exp[T])(implicit tr: TypeRep[T], mf: Manifest[T]): Exp[IndexedSeq[T]] = {
    val lambda = doInternalLambda(block,true, None)(exposeRepFromRep[Int], exposeRepFromRep[T])
    val cc = RangeMap[T](s, end, lambda.exp)
    toAtom(cc)
  }

  case class ExpensiveF(x: Exp[Int], y: Exp[Int]) extends Def[Int]

  def expensive_pure_f(x: Exp[Int], y: Exp[Int]): Exp[Int] = ExpensiveF(x, y)

}

trait ScalaGenMyRange extends ScalaCodegen with EmitHeadInternalFunctionAsClass {
  val IR: MyRange

  import IR._

  override def emitNode(tp: TP[_], acc: Vector[String],
                        block_callback: (Block, Vector[String]) => Vector[String]): Vector[String] = {
    tp.rhs match {
      case ExpensiveF(lhs, rhs) => Vector(emitValDef(tp, src"Math.max($lhs,$rhs)"))
      case RangeMap(start, end, body) =>  {
        val thenlambda = exp2tp(body)
        val rets: Vector[String] = (thenlambda.rhs) match {
          case (InternalLambda(tf, tx, ty, thot, targs, treturns)) =>  Vector ({
            val l1 = "val " + quote(tp) + " = for (" + quote(tx.head) + " <- " + quote(start) + " until " + quote(end) + " ) yield {\n" //TODO: fix me!
            val l2 = block_callback(ty, Vector(l1))
            val trestuple: Vector[String] = ty.res.map(r => quote(r))
            val l3: String = l2 + tupledeclarehelper(trestuple, "")
            val l4 = l3 + "\n}"
            l4 + "\n"
          })
          case _ => {
            assert(false, "got an if statment which does not contain lambdas for its branches")
            Vector.empty
          }
        }
        rets
      }
      case _ => super.emitNode(tp, acc, block_callback)
    }
  }
}

trait InstGraphVizExport {
  self =>
  val IR: BaseExp with FunctionsExp with MyRange with IfThenElsePureExp with OrderingOpsExp

  type specCM = CodeMotion {
    val reifiedIR: ReificationPure {
      val IR: self.IR.type
    }}

  def emitDepGraph(file: String, landscape: Boolean = false): String = ???

  def quote(x: Any) = "\"" + x + "\""

  def emitDepGraphf[A, R](f: Function1[A, R], mode: Int)(implicit args: IR.ExposeRep[A], returns: IR.ExposeRep[R]): (String, specCM) = {
    val reify = new ReifyPure {
      override val IR: self.IR.type = self.IR
    }
    val reification = reify.reifyProgram(f)(args, returns)
    val cm: specCM = CodeMotion(reification)
    (emitDepGraph(cm,mode), cm)
  }


  def emitDepGraph(cm: specCM, mode: Int): String = {
    def emitNodeString(tp: cm.reifiedIR.IR.TP[_]): String = {
      val nodestring = tp.rhs match {
        case IR.ConstDef(x) =>  x
        case IR.ExternalLambda(f, x, y, hot, args, returns,global,name) =>  "EntryF"
        case IR.InternalLambda(f, x, y, hot, a, r) => "InternalF"
        case IR.RangeMap(s, e, b) => "Range"
        case IR.myIfThenElse(c, t, e, b) => "IF"
        case IR.ExpensiveF(x, y) => "ExpensiveF"
        case IR.IntPlus(x, y) => "+"
        case IR.IntDivide(x, y) => " / "
        case IR.OrderingLTEQ(x,y) => "<="
        case IR.ArgDef(id) => "Function Argument"
        case IR.IntMod(x,y) => "%"
        case IR.ReturnArg(x,y,pos,tuple,last) => "ReturnTuple"
        case _ => tp.sym.id + " \\n " + tp.rhs + "\\n" + tp.tag.mf.toString()
      }
      tp.sym.id + " [label=" + quote(nodestring) + "\n,shape=box]"

    }

    def emitNode(node: cm.EnrichedGraphNode): String = {
      val tp = cm.reifiedIR.id2tp(node.irdef)

      val nodestring = emitNodeString(tp)


      val sucessorstring = node.successors.foldLeft("") {
        (acc, ele) => acc + "\n\"" + node.irdef + "\" -> \"" + ele + "\"" + " [label=\"s\"] "
      }
      val predecessorstring = node.predecessors.foldLeft("") {
        (acc, ele) => acc + "\n\"" + node.irdef + "\" -> \"" + ele + "\"" + " [label=\"p\"] "
      }

      val blockresids: Set[Int] = node.blocks.flatMap(v => v.res.map(res => res.id))
      val blockdepstring = blockresids.foldLeft("") {
        (acc, ele) => acc + "\n\"" + node.irdef + "\" -> \"" + ele + "\"" + " [label=\"b\"] "
      }

      val blockCMcontained: Set[Vector[Int]] = node.blocks.map(b => IR.block2tps(b).map(tp => tp.sym.id))

      val blockCMdepstring: String = blockCMcontained.map(b => b.foldLeft("") {
        (acc, ele) => acc + "\n\"" + node.irdef + "\" -> \"" + ele + "\"" + " [label=\"cm\"] "
      }).mkString("\n")




      nodestring + sucessorstring + predecessorstring + blockdepstring //+ blockCMdepstring

    }

    def emitPlainNode(tp: cm.reifiedIR.IR.TP[_]): String = {
      val (sym, rhs) = (tp.sym, tp.rhs)
      //val str = tp.sym.id + " [label=" + quote(sym + " \\n " + rhs) + "shape=box]"
      val str = emitNodeString(tp)
      val deps = cm.reifiedIR.IR.syms(rhs)
      val depsstr = for (dep <- deps) yield {
        ("\"" + dep.id + "\" -> \"" + sym.id + "\"" )
      }
      val blocks = cm.reifiedIR.IR.blocks(rhs)
      val blockstr = for (b <- blocks) yield {
        b.res.map( res => "\"" + sym.id + "\" -> \"" + res.id + "\"[style=dotted]").mkString("\n")
      }
      val args: String = rhs match{
        case IR.ExternalLambda(f, x, y, hot, args, returns,global,name) =>  x.map(ele => "\"" + sym.id + "\" -> \"" + ele.sym.id + "\"[style=dotted]").mkString("\n")
        case IR.InternalLambda(f, x, y, hot, a, r) => x.map(ele => "\"" + sym.id + "\" -> \"" + ele.sym.id + "\"[style=dotted]").mkString("\n")
        case _ => ""
      }

      str + depsstr.mkString("\n") + blockstr.mkString("\n") + args
    }

    mode match {
      case 0 => {
        //we emit the head node and all blocks
        //this is assuming that head node is always a function and everything else is contained within
        val lamdbaid: Int = cm.block_cache3.root.sym.id
        val check = cm.enriched_graph
        val head = emitNode(check(lamdbaid))
        val graphstring = cm.block_cache3.blockinfo.foldLeft(Vector.empty[String])((acc, ele) => {
          val (block, blockinfo) = ele
          val blockres = blockinfo.children.foldLeft(Vector.empty[String]) {
            (iacc, iele) => {
              val (id, node) = iele
              iacc :+ emitNode(node)
            }
          }
          acc ++ blockres
        })
        "digraph G {\n" + head + "\n" + graphstring.mkString("\n") + "\n}"
      }
      case _ => {
        val nodes = cm.reifiedIR.id2tp.map(ele => emitPlainNode(ele._2))
        "digraph G {\n" + nodes.mkString("\n") + "\n}"
      }
    }
  }
}
