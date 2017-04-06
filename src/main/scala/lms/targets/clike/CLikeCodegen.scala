package scala.lms
package targets
package clike

import java.io.PrintWriter

import scala.lms.internal._

trait CLikeCodegen extends GenericCodegen with Config {
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
    val tpe = tp.tag.mf.toString()
    if (!isVoidType(tpe))
      remapWithRef(tpe) + quote(tp) + " = " + rhs + ";"
    else ""
  }

  def remapWithRef[A](m: Manifest[A]): String = remap(m) + addRef(m)
  def remapWithRef(tpe: String): String = tpe + addRef(tpe)
  def isPrimitiveType(tpe: String) : Boolean = {
    tpe match {
      case "bool" | "int8_t" | "uint16_t" | "int16_t" | "int32_t" | "int64_t" | "float" | "double" => true
      case _ => false
    }
  }
  def isVoidType(tpe: String) : Boolean = {
    if(tpe == "void") true
    else false
  }

  def addRef(): String = if (cppMemMgr=="refcnt") " " else " *"
  def addRef[A](m: Manifest[A]): String = addRef(remap(m))
  def addRef(tpe: String): String = {
    if (!isPrimitiveType(tpe) && !isVoidType(tpe)) addRef()
    else " "
  }


  def relativePath(fileName: String): String = {
    val i = fileName.lastIndexOf('/')
    fileName.substring(i + 1)
  }
}


trait EmitHeadInternalFunctionAsCMain extends CLikeCodegen {
  self =>

  var head: IR.TP[_] = null //RF - get rid of the state!
  val staticData = Vector.empty[(IR.TP[_],Any)]
  var className: String

  val tuplesize = 21

  override def emitSource[A,R](
                                f: Function1[A,R],
                                className: String,
                                out: PrintWriter)(implicit args: IR.ExposeRep[A], returns: IR.ExposeRep[R]) = {

    val res = super.emitSource(f,className,out)(args,returns)
    head = null
    res
  }

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
      acc + "(" + rest.mkString(",") + ")"
    else
    {
      val start = acc + "(" + rest.take(tuplesize).mkString(",") + ","
      tupledeclarehelper(rest.drop(tuplesize),start) + ")"
    }
  }
  /*


    def myhelp(tp: IR.TP[_]): String = {
      val rm:String = tp.rhs match {


        case IR.InternalLambda(f,x,y,args,returns) => {
          val av: Vector[String] = x.map(ele => remap(ele.tag.mf))
          val rv: Vector[String] = y.res.map(ele => myhelp(IR.exp2tp(ele)))
          val a = tupledeclarehelper(av, "")
          val r = tupledeclarehelper(rv, "")
          println("................")
          println(a + r)
          println("................")
          "scala.Function1[" + a + "," + r + "]"
        }
        case IR.ReturnArg(f,newsym,pos,tuple,last) => {
          if (tp.tag.toString.contains("Function"))
            println(".....")
          val newsymtp = IR.exp2tp(newsym)
          val applynodetp = IR.exp2tp(f)
          myhelp(newsymtp)
        }
        case IR.ArgDef(id) => {
          if (tp.tag.toString.contains("Function")){
            val funexp = tp.sym
            val aro = IR.funexp2StagedFunction.get(funexp)
            aro match{
              case Some(sf) =>{
                val args = sf.args.freshExps()
                val returns = sf.returns.freshExps()
                val av: Vector[String] = args.map(ele => myhelp(IR.exp2tp(ele)))
                val rv: Vector[String] = returns.map(ele => myhelp(IR.exp2tp(ele)))
                val a = tupledeclarehelper(av, "")
                val r = tupledeclarehelper(rv, "")
                "scala.Function1[" + a + "," + r + "]"
              }
              case None => {
                //val otp = IR.id2tp(id)
                println("Function - but not found in map")
                val r = myhelp(IR.id2tp(id))
                println(r)
                r
                //remap(tp.tag.mf)
              }

            }
          }
          else
            remap(tp.tag.mf)

        }



        case _ => {
          remap(tp.tag.mf)
        }
      }
      rm
    }
  */

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




  override def emitNode(tp: self.IR.TP[_], acc: Vector[String],
                        block_callback: (self.IR.Block,Vector[String]) => Vector[String]): Vector[String] = tp.rhs match {
    case IR.ExternalLambda(f,x,y,hot,args,returns,global,name) => {
      val returntuple = tupledeclarehelper(y.res.map(a => remap(IR.exp2tp(a).tag) ),"")
      val restuple: Vector[String] = y.res.map(r => quote(r))
      val helper = if (x.size > 1) {
        x.zipWithIndex.map(a => {
          val (tp,index) = a
          val typ = remap(tp.tag.mf)
          "val " + quote(tp) + " : " + remap(tp.tag) + " = helper" + tupleaccesshelper(index,"",index == x.size-1)
        }).mkString("\n")
      } else {
        //"val " + quote(x.head) + " : " + remap(x.head.tag.mf) + " = helper\n"
        "val " + quote(x.head) + " : " + remap(x.head.tag) + " = helper\n"
      }
      val argtuple = tupledeclarehelper(x.map(a => remap(a.tag)),"")

      if (head == null || head == tp) {
        head = tp
        /*if (y.res.size > 1)
          assert(false, "still need to implement multiy result unparsing")*/



        val stringheader =
        "/*****************************************\n" +
                     "  Emitting C Generated Code                  \n"+
                     "*******************************************/\n" +
          "#include <stdio.h>\n" +
          "#include <stdlib.h>\n" +
          "#include <string.h>\n" +
          "#include <stdbool.h>\n" +
            "class "+className+(if (staticData.isEmpty) "" else "("+staticData.map(p=>"p"+quote(p._1)+":"+p._1.tag).mkString(",")+")")+
            " extends (("+ argtuple +")=> (" + returntuple + ")) {" +
            //"\ndef apply("+x.map(a => quote(a) + ":" + remap(a.tag.mf)).mkString(", ")+"): ("+returntuple+") = {\n"
            "\ndef apply( helper: ("+ argtuple +")): ("+returntuple+") = {\n" + helper + "\n"

        val t1 = block_callback(y,Vector(stringheader))
        val res =  t1 :+
          "\n "+ tupledeclarehelper(restuple,"") +  "\n" +
            "}" +
            "}" +
          "/*****************************************\n"+
          "  End of C Generated Code                  \n"+
          "*******************************************/\n"
        res
      }
      else {
        val t1 = "def " + quote(tp) + ": " +
          "("+ argtuple +") => (" + returntuple + ") = " +
          "(helper: ("+ argtuple+")) =>{\n" + helper + "\n"
        val t2: Vector[String] = block_callback(y,Vector(t1))
        val t3 =   "\n "+ tupledeclarehelper(restuple,"") +  "\n" + "}\n"
        //Vector(t1) ++ t2 :+ t3
        Vector(t3) //t1 and t2 streamed out through the block callback
        //emitValDef(tp,string)
        //assert(false, "you are emitting code that has Internal Lambdas in the body - not handling this yet")
      }
    }
    case IR.InternalApply(f,arg, name) => Vector( {
      //"val " + res.res.map(r => quote(r)).mkString(", ") + " = " + quote(f) + "(" + arg.map(r => quote(r)).mkString(", ") + ")\n"
      emitValDef(tp, " " + quote(f) + "(" + arg.map(r => quote(r)).mkString(", ") + ")\n")
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