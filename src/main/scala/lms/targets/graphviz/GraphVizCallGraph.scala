package scala.lms
package targets
package graphviz

import scala.lms.internal._


/**
  * Created by rayda on 02-Feb-17.
  */
trait GraphVizCallGraph {
  self =>
  val IR: BaseExp with FunctionsExp

  type specCM = CodeMotion {
    val reifiedIR: Reification {
      val IR: self.IR.type
    }}


  def emitDepGraph(file: String, landscape: Boolean = false): String = ???

  def quote(x: Any) = "\"" + x + "\""

  def emitDepGraphf[A, R](f: Function1[A, R])(implicit args: IR.ExposeRep[A], returns: IR.ExposeRep[R]): (String, specCM) = {
    val reify = new Reify {
      override val IR: self.IR.type = self.IR
    }
    val reification = reify.reifyProgram(f)(args, returns)
    val cm: specCM = CodeMotion(reification)
    (emitDepGraph(cm), cm)
  }

  var current: Int = 0

  def emitDepGraph(cm: specCM): String = {

    def emitbody(y : IR.Block): Vector[String] = {
      val blockinfo = cm.block_cache3.blockinfo(y)
      val block = y
      //val (block, blockinfo) =
      //val effs = block.effects.map( e => (e.id,e))
      val blockres = blockinfo.children.foldLeft(Vector.empty[String]) {
        (iacc, iele) => {
          val (id, node) = iele
          val tp = cm.reifiedIR.id2tp(id)
          println(tp.rhs)
          val ns = emitNode(node)
          val nss: Vector[String] = if (ns == "") Vector.empty else Vector(ns)
          iacc ++ nss
        }
      }
      blockres
    }


    def emitNode(node: cm.EnrichedGraphNode): String = {
      val tp = cm.reifiedIR.id2tp(node.irdef)
      val nodestring = tp.rhs match {
       case IR.InternalLambda(f,x,y,hot,args,returns) => emitbody(y).mkString("")
        //case IR.ExternalLambda(f, x, y, hot, args, returns, global, name) => tp.sym.id + " [label=" + quote(tp.sym.id + " \\n " + "ExternalLambda - Args: " + x + "Ret:" + y.res + "\\n" + tp.tag.mf.toString()) + "\n,shape=box]"
        //case _ => tp.sym.id + " [label=" + quote(tp.sym.id + " \\n " + tp.rhs + "\\n" + tp.tag.mf.toString()) + "\n,shape=box]"
        case IR.InternalApply(f, arg, name) => {
          //tp.sym.id + " [label=" + quote(tp.sym.id + " \\n " + tp.rhs + "\\n" + tp.tag.mf.toString()) + "\n,shape=box]"
          //s"${tp.sym.id} -> ${current}"
          s"${current} -> ${f.id}\n"
        }
        case _ => ""
      }

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

      val blockeffresids: Set[Int] = node.blocks.flatMap(v => v.effects.map(res => res.id))
      val blockeffdepstring = blockeffresids.foldLeft("") {
        (acc, ele) => acc + "\n\"" + node.irdef + "\" -> \"" + ele + "\"" + " [label=\"eff\"] "
      }


      val blockCMcontained: Set[Vector[Int]] = node.blocks.map(b => IR.block2tps(b).map(tp => tp.sym.id))

      val blockCMdepstring: String = blockCMcontained.map(b => b.foldLeft("") {
        (acc, ele) => acc + "\n\"" + node.irdef + "\" -> \"" + ele + "\"" + " [label=\"cm\"] "
      }).mkString("\n")


      nodestring //+ sucessorstring + predecessorstring + blockdepstring + /*blockCMdepstring +*/ blockeffdepstring

    }

    //we emit the head node and all blocks
    //this is assuming that head node is always a function and everything else is contained within
    val lamdbaid: Int = cm.block_cache3.root.sym.id


    //emitNode(check(lamdbaid))



    val all = cm.block_cache3.globals :+ cm.reifiedIR.id2tp(lamdbaid)
    val graphstring =  all.foldLeft(Vector.empty[String])((acc, ele) => {
      val res = ele.rhs match {
        case IR.ExternalLambda(f, x, y, hot, args, returns, global, name) => {
          current = ele.sym.id
          val blockinfo = cm.block_cache3.blockinfo(y)
          val block = y
          //val (block, blockinfo) =
          //val effs = block.effects.map( e => (e.id,e))
          val blockres = blockinfo.children.foldLeft(Vector.empty[String]) {
            (iacc, iele) => {
              val (id, node) = iele
              val tp = cm.reifiedIR.id2tp(id)
              println(tp.rhs)
              val ns = emitNode(node)
              val nss: Vector[String] = if (ns == "") Vector.empty else Vector(ns)
              iacc ++ nss
            }

          }
          val tp = ele
          /*val s: String = if (tp.sym.id == lamdbaid)
            tp.sym.id + " [label=" + quote(tp.sym.id + "\\n" +"Top Level" ) + "\n,shape=polygon]"
          else
            tp.sym.id + " [label=<" + quote(tp.sym.id + "\\n" +name.get) + ">\n,shape=box]"*/
          val s: String = if (tp.sym.id == lamdbaid)
            tp.sym.id + s" [id = ${quote(tp.sym.id)}label=<" + quote("Top Level" ) + ">\n,shape=polygon]"
          else
            tp.sym.id + s" [id = ${quote(tp.sym.id)} " + "label=<" + name.get + ">\n,shape=box]"
          Vector(s) ++ blockres
        }
        case _ => Vector.empty
      }


      acc ++ res
    })


    "digraph G {\n" + "1 [id = \"1\"label=<Input>,shape=circle]\n 1 -> 0\n" + graphstring.mkString("\n") + "\n}"
  }


}
