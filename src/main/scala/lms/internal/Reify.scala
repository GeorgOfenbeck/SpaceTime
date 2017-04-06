package scala.lms
package internal

import scala.lms.BaseExp



trait ReificationPure{
 val IR: BaseExp with FunctionsExp
 val sym2tp: Map[IR.Exp[_], IR.TP[_]]
 val def2tp: Map[IR.Def[_], IR.TP[_]]
 val id2tp: Map[Int, IR.TP[_]]
 val rootlambda: IR.AbstractLambda[_,_]
 val block2tps: Map[IR.Block, Vector[IR.TP[_]]]
 //val funexp2AR: Map[IR.Exp[_], (Vector[IR.Exp[_]],Vector[IR.Exp[_]])]
 //val funexp2StagedFunction: Map[IR.Exp[_], IR.StagedFunction[_,_]]
// val fun2tp: Map[IR.StagedFunction[_,_], IR.TP[_=>_]]
 //val tp2fun: Map[IR.TP[_=>_],IR.StagedFunction[_,_]]

 object Const {
  def unapply[T](e: IR.Exp[T]): Option[IR.ConstDef[T]] = {
   val tp = sym2tp(e)
   tp.rhs match {
    case d@IR.ConstDef(x) => Some(d.asInstanceOf[IR.ConstDef[T]]) //TODO - get rid of type cast
    case _ => None
   }
  }
 }

 object Arg {
  def unapply[T](e: IR.Exp[T]): Option[IR.ArgDef[T]] = {
   val tp = sym2tp(e)
   tp.rhs match {
    case d@IR.ArgDef(id) => Some(d.asInstanceOf[IR.ArgDef[T]]) //TODO - get rid of type cast
    case _ => None
   }
  }
 }
}


trait ReifyPure{
 self =>
 val IR: BaseExp with FunctionsExp
 import IR._

 def reifyProgram[A,R](f: Function1[A,R])(implicit args: ExposeRep[A], returns: ExposeRep[R]): ReificationPure{ val IR: self.IR.type} = {
  IR.reset()
  val lambda = IR.doLambda(f, false, None)
  val lambdatp: IR.TP[_] = IR.exp2tp(lambda.exp)
  reifyProgramfromLambda(lambdatp)
 }

 def reifyProgramfromLambda(lambdatp: TP[_]): ReificationPure{ val IR: self.IR.type} = {
  //val tp = exp2tp(lambda)
  val lam: AbstractLambda[_,_] = lambdatp.rhs match{
   case x@InternalLambda(_,_,block,_,_,_) => x
   case x@ExternalLambda(_,_,block,_,_,_,_,name) => x
   case _ => {
    assert(false, "This should not be possible")
    ???
   }
  }

  val immutable_out = new ReificationPure {
   val IR: self.IR.type = self.IR
   val sym2tp: Map[IR.Exp[_], IR.TP[_]] = self.IR.exp2tp
   val def2tp: Map[IR.Def[_], IR.TP[_]] = self.IR.def2tp
   val id2tp: Map[Int,IR.TP[_]] = self.IR.id2tp
   val block2tps: Map[Block,Vector[IR.TP[_]]] = self.IR.block2tps
   val funTable: Map[Any, StagedFunction[_,_]] = self.IR.funTable


   //val funexp2AR: Map[Exp[_], (Vector[Exp[_]],Vector[Exp[_]])] = self.IR.funexp2AR
   //val funexp2StagedFunction: Map[Exp[_], StagedFunction[_,_]] = self.IR.funexp2StagedFunction
   //val fun2tp: Map[StagedFunction[_,_], IR.TP[_=>_]] = self.IR.fun2tp
   //val tp2fun: Map[IR.TP[_=>_],StagedFunction[_,_]] = self.IR.tp2fun

   //val rootblock = lam.y
   val rootlambda = lam
  }
  immutable_out
 }
}



trait Reification extends ReificationPure{
 val IR: BaseExp with FunctionsExp
 val sym2tp: Map[IR.Exp[_], IR.TP[_]]
 val def2tp: Map[IR.Def[_], IR.TP[_]]
 val id2tp: Map[Int, IR.TP[_]]
 val rootlambda: IR.AbstractLambda[_,_]
 val block2tps: Map[IR.Block, Vector[IR.TP[_]]]

}


trait Reify extends ReifyPure{
 self =>
 val IR: BaseExp with FunctionsExp
 import IR._

 override def reifyProgram[A,R](f: Function1[A,R])(implicit args: ExposeRep[A], returns: ExposeRep[R]): Reification{ val IR: self.IR.type} = {
  IR.reset()
  val lambda = IR.doLambda(f, false, None)
  val lambdatp: IR.TP[_] = IR.exp2tp(lambda.exp)
  reifyProgramfromLambda(lambdatp)
 }

 override def reifyProgramfromLambda(lambdatp: TP[_]): Reification{ val IR: self.IR.type} = {
  //val tp = exp2tp(lambda)
  val lam: AbstractLambda[_,_] = lambdatp.rhs match{
   case x@InternalLambda(_,_,block,_,_,_) => x
   case x@ExternalLambda(_,_,block,_,_,_,_,name) => x
   case _ => {
    assert(false, "This should not be possible")
    ???
   }
  }

  val immutable_out = new Reification {
   val IR: self.IR.type = self.IR
   val sym2tp: Map[IR.Exp[_], IR.TP[_]] = self.IR.exp2tp
   val def2tp: Map[IR.Def[_], IR.TP[_]] = self.IR.def2tp
   val id2tp: Map[Int,IR.TP[_]] = self.IR.id2tp
   val block2tps: Map[Block,Vector[IR.TP[_]]] = self.IR.block2tps
   val rootlambda = lam
  }
  immutable_out
 }
}