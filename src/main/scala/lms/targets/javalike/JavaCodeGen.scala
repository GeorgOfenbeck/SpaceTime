package scala.lms
package targets
package javalike

import java.io.PrintWriter

import scala.lms.internal._


trait JavaCodegen extends GenericCodegen with Config {
  self =>
  val IR: BaseExp with FunctionsExp
  import IR._

  var className: String = ""





  def emitSource[A,R](
                      f: Function1[A,R],
                      className: String,
                      out: PrintWriter)(implicit args: IR.ExposeRep[A], returns: IR.ExposeRep[R]) = {
    self.className = className //RF!
    val esc = self.emit(out,Vector.empty,f)(args, returns)
    out.flush()
    esc
  }


  def emitValDef(tp: TP[_], rhs: String): String = {
    val extra = if ((sourceinfo < 2) || tp.sym.pos.isEmpty) "" else {
      val context = tp.sym.pos(0)
      "      // " + relativePath(context.fileName) + ":" + context.line
    }
    val typ = remap(tp.tag.mf)



    s"${typ} ${quote(tp)} = $rhs$extra;\n "
  }

  def relativePath(fileName: String): String = {
    val i = fileName.lastIndexOf('/')
    fileName.substring(i + 1)
  }
}



trait TupleHelper extends JavaCodegen{

  val tuplesize = 21
  def tupleaccesshelper(pos: Int, acc: String, lastele: Boolean): String = {
    if (pos < tuplesize)
      if (pos == 0 && lastele)
        acc //its not a tuple
      else
        acc + "._" + (pos + 1).toInt
    else{
      tupleaccesshelper(pos - tuplesize,acc + "._" + (tuplesize+1), lastele)
    }
  }
  def tupledeclarehelper(rest: Vector[String], acc: String): String = {
    if (rest.size <= tuplesize)
      acc + rest.mkString(",")
    else
    {
      val start = acc   + rest.take(tuplesize).mkString(",") + ","
      tupledeclarehelper(rest.drop(tuplesize),start)
    }
  }

  override def remap[A](m: Manifest[A]): String = {
    m.toString() match{
      case "Array[Double]" => "double []"
      case "Int" => "int"
      case "Double" => "double"
      case _ => super.remap(m)
    }
  }

  def remap(tag: IR.TypeRep[_]): String = {
    tag match {
      case IR.TypeExp(mf,dyntags) => {
        dyntags match {
          case Some(f) => {
            val (args,returns) = f()
            val rargs = args.map(a => remap(a))
            val rrets = returns.map(r => remap(r))
            val a = tupledeclarehelper(rargs, "")
            val r = tupledeclarehelper(rrets, "")
            "scala.Function1[" + a + "," + r + "]"
          }
          case None => {
            if (mf.toString().contains("Function"))
            {
              println("bla")
            }
            remap(mf)
          }
        }
      }
      case _ => {
        assert(false, "this should just never happen")
        ""
      }
    }
  }

}

trait EmitHeadNoTuples extends JavaCodegen with TupleHelper{
  self =>

  var head: IR.TP[_] = null //RF - get rid of the state!
  val staticData = Vector.empty[(IR.TP[_],Any)]
  var className: String



  override def emitSource[A,R](
                                f: Function1[A,R],
                                className: String,
                                out: PrintWriter)(implicit args: IR.ExposeRep[A], returns: IR.ExposeRep[R]) = {

    val res = super.emitSource(f,className,out)(args,returns)
    head = null
    res
  }

  override def emitNode(tp: self.IR.TP[_], acc: Vector[String],
                        block_callback: (self.IR.Block,Vector[String]) => Vector[String]): Vector[String] = tp.rhs match {
    case IR.ExternalLambda(f,x,y,hot,args,returns,global,name) => {
      val returntuple = tupledeclarehelper(y.res.map(a => remap(IR.exp2tp(a).tag) ),"")
      val restuple: Vector[String] = y.res.map(r => quote(r))

      val paras = x.map(tp => {
        val typ = remap(tp.tag.mf)
        s"${remap(tp.tag)} ${quote(tp)}"
      }).mkString(", ")




      if (head == null || head == tp) {
        head = tp
        /*if (y.res.size > 1)
          assert(false, "still need to implement multiy result unparsing")*/



        val stringheader =
          "/*****************************************\n"+
            "  Emitting Generated Code                  \n"+
            "*******************************************/\n" +
            "class "+className+
             "{" +
            //"\ndef apply("+x.map(a => quote(a) + ":" + remap(a.tag.mf)).mkString(", ")+"): ("+returntuple+") = {\n"
            s"\nstatic $returntuple apply( $paras ) {\n" + "\n"

        val t1 = block_callback(y,Vector(stringheader))
        val res =  t1 :+
          "\n return "+ tupledeclarehelper(restuple,"") +  ";\n" +
            "}" +
            "" +
            "\n/*****************************************\n"+
            "  End Main                  \n"+
            "*******************************************/\n"
        res
      }
      else {
        val t1 = "static " +returntuple + name.map(_ + "_").getOrElse("") + quote(tp) + "" +
          "( " + paras + ") {\n" + "\n"
          //"("+ argtuple +") => (" + returntuple + ") = \n" +
          //"(helper: ("+ argtuple+")) =>{\n" + helper + "\n"
        val t2: Vector[String] = block_callback(y,Vector(t1))
        val t3 =   "\n return "+ tupledeclarehelper(restuple,"") +  ";\n" + "}\n"
        //Vector(t1) ++ t2 :+ t3
        Vector(t3) //t1 and t2 streamed out through the block callback
        //emitValDef(tp,string)
        //assert(false, "you are emitting code that has Internal Lambdas in the body - not handling this yet")
      }
    }
    case IR.InternalApply(f,arg,name) => Vector( {
      //"val " + res.res.map(r => quote(r)).mkString(", ") + " = " + quote(f) + "(" + arg.map(r => quote(r)).mkString(", ") + ")\n"
      emitValDef(tp, " " + name.map(_ + "_").getOrElse("") + quote(f) + "(" + arg.map(r => quote(r)).mkString(", ") + ");\n")
    } )
    case IR.ReturnArg(f,sym,posx,tuple,last) => Vector({
      /*tp.tag match {
        case x@IR.TypeExp(mf,dyntags) => {
          println("..........")
          println(dyntags)
        }
        case _ => {
          assert(false, "should never happen")
        }
      }*/
      if (tuple) {
        ???
        //emitValDef(tp, quote(f) + "._" + (pos + 1).toInt)
        val start = "val " + quote(tp) + " = " + quote(f)
        tupleaccesshelper(posx,start,last) + "//returnarg  " + last + "\n"//RF
      }
      else
        emitValDef(tp,quote(f))
    })
    case IR.ArgDef(id) => Vector.empty //args are handled in the according lambda
    case IR.ConstDef(x) => Vector.empty //are handeled through remaps
    case IR.InternalLambda(f,x,y,hot,a,r) => Vector.empty //are inlined by the symbol containing them
    case IR.Reflect(a,b,c) => {
      tp match {
        case tpm@IR.TP(sym,IR.Reflect(x,summary,deps),tag) => {
          val t = tpm.copy(rhs = x)
          emitNode(t,acc,block_callback)
        }
        case _ => {
          assert(false, "this should be unreachable")
          Vector.empty
        }
      }
    }

    case _ => super.emitNode(tp,acc,block_callback)
  }

}