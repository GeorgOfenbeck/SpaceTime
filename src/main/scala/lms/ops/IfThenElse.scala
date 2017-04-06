package scala.lms
package ops


import java.io.PrintWriter
import org.scala_lang.virtualized.SourceContext

import scala.lms.internal.{FunctionsExp, ExposeRepBase, Effects}



trait IfThenElse extends Base with ExposeRepBase  {
  def __ifThenElse[T:TypeRep](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T])(implicit pos: SourceContext): Rep[T]
  def myifThenElse[A](cond: Rep[Boolean], thenp: => A, elsep: => A)(implicit pos: SourceContext, branch: ExposeRep[A]): A
}
// TODO: it would be nice if IfThenElseExp would extend IfThenElsePureExp
// but then we would need to use different names.

trait IfThenElsePureExp extends IfThenElse with BaseExp with FunctionsExp{
  case class IfThenElse[T:TypeRep](cond: Exp[Boolean], thenp: Exp[T], elsep: Exp[T]) extends Def[T]
  def __ifThenElse[T:TypeRep](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T])(implicit pos: SourceContext) = IfThenElse(cond, thenp, elsep)

  case class myIfThenElse[A](cond: Exp[Boolean], thenp: Exp[_ => _],
                             elsep: Exp[_ => _], branch: ExposeRep[A]) extends Def[Any]


  def myifThenElse[A](cond: Rep[Boolean], thenp: => A, elsep: => A)(implicit pos: SourceContext, branch: ExposeRep[A]): A =
  {
    val thenf: Rep[Unit] => A = (u: Rep[Unit]) => thenp
    val elsef: Rep[Unit] => A = (u: Rep[Unit]) => elsep
    val thenlambda = doInternalLambda(thenf, false, None)
    val elselambda = doInternalLambda(elsef, false, None)
    val newsyms = branch.freshExps()
    val ifthenelsenode = myIfThenElse(cond, thenlambda.exp, elselambda.exp,branch)
    val ifthenelseexp = toAtom(ifthenelsenode)


    val returnNodes = if (newsyms.size > 1) {
      newsyms.zipWithIndex.map(fsym => {
        //had do to this ugly version since the compile time type is not know at this stage (of the return - Rep[_])
        val otp = exp2tp(fsym._1)
        val tag: TypeRep[Any] = otp.tag.asInstanceOf[TypeRep[Any]]
        val cc: Def[Any] = ReturnArg(ifthenelseexp, fsym._1, fsym._2, true, newsyms.size == fsym._2 + 1)
        val newx = toAtom(cc)(tag, null)
        /*if (tag.mf.toString().contains("Function")) {
          val newtp = exp2tp(newx)
          println(newtp)
        }*/
        newx
      })
    } else {
      newsyms.zipWithIndex.map(fsym => {
        val tag: TypeRep[Any] = exp2tp(fsym._1).tag.asInstanceOf[TypeRep[Any]]
        val cc: Def[Any] = ReturnArg(ifthenelseexp, fsym._1, fsym._2, false, true)
        val newx = toAtom(cc)(tag, null)
        newx
      })
    }
    branch.vec2t(returnNodes)
  }
}


trait IfThenElseExp extends IfThenElse with BaseExp with Effects {

  abstract class AbstractIfThenElse[T] extends Def[T] {
    val cond: Exp[Boolean]
    val thenp: Block
    val elsep: Block
  }
  
  case class IfThenElse[T:TypeRep](cond: Exp[Boolean], thenp: Block, elsep: Block) extends AbstractIfThenElse[T]
  case class IfThenElseLamda[R](cond: Exp[Boolean], thenp: R, elsep: R) extends Def[_ => _]
  def ifThenElseLambda[R](cond: Rep[Boolean], thenpf: Function1[Rep[Unit],R], elsepf: Function1[Rep[Unit],R])
                         (implicit pos: SourceContext, returns: ExposeRep[R]): R = {
    /*val f1 = doLambda(thenpf)
    val f2 = doLambda(elsepf)
    val applythen: R = doApply(f1,unitToRepUnit())
    val applyelse: R = doApply(f2,unitToRepUnit())
    val t: Def[_] = IfThenElseLamda(cond,applythen,applyelse)
    val sf = toAtom(t) //create the function
    sf*/
    ???
  }





  override def __ifThenElse[T:TypeRep](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T])
                                      (implicit pos: SourceContext) = {
    /*val a = reifyEffectsHere(thenp)
    val b = reifyEffectsHere(elsep)
    ifThenElse(cond,a,b)*/
    ???
  }

  def ifThenElse[T:TypeRep](cond: Rep[Boolean], thenp: Block, elsep: Block)(implicit pos: SourceContext) = {
    /*val ae = summarizeEffects(thenp)
    val be = summarizeEffects(elsep)*/
    
    // TODO: make a decision whether we should call reflect or reflectInternal.
    // the former will look for any read mutable effects in addition to the passed
    // summary whereas reflectInternal will take ae orElse be literally.
    // the case where this comes up is if (c) a else b, with a or b mutable.
    // (see TestMutation, for now sticking to old behavior)
    
    ////reflectEffect(IfThenElse(cond,thenp,elsep), ae orElse be)
    //reflectEffectInternal(IfThenElse(cond,thenp,elsep), ae orElse be)
    ???
  }
  
  /*override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = e match {
    case IfThenElse(c,a,b) => IfThenElse(f(c),f(a),f(b))
    case _ => super.mirrorDef(e,f)
  }
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case Reflect(IfThenElse(c,a,b), u, es) => 
      if (f.hasContext)
        __ifThenElse(f(c),f.reflectBlock(a),f.reflectBlock(b))
      else
        reflectMirrored(Reflect(IfThenElse(f(c),f(a),f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case IfThenElse(c,a,b) => 
      if (f.hasContext)
        __ifThenElse(f(c),f.reflectBlock(a),f.reflectBlock(b))
      else
        IfThenElse(f(c),f(a),f(b)) // FIXME: should apply pattern rewrites (ie call smart constructor)
    case _ => super.mirror(e,f)
  }
*/
/*
  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = e match {
    case Reflect(IfThenElse(c,a,b), u, es) => mirror(IfThenElse(c,a,b)) // discard reflect
    case IfThenElse(c,a,b) => ifThenElse(f(c),f(a),f(b)) // f.apply[A](a: Block[A]): Exp[A] mirrors the block into the current context
    case _ => super.mirror(e,f)
  }  
*/



  override def aliasSyms(e: Any): Vector[Exp[_]] = e match {
    case IfThenElse(c,a,b) => syms(a) ++ syms(b)
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): Vector[Exp[_]] = e match {
    case IfThenElse(c,a,b) => Vector.empty
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): Vector[Exp[_]] = e match {
    case IfThenElse(c,a,b) => Vector.empty
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): Vector[Exp[_]] = e match {
    case IfThenElse(c,a,b) => Vector.empty // could return a,b but implied by aliasSyms
    case _ => super.copySyms(e)
  }


  override def symsFreq(e: Any): Vector[(Exp[_], Double)] = e match {
    case IfThenElse(c, t, e) => {
      freqNormal(c) ++ freqCold(t) ++ freqCold(e)
    }
    case _ => super.symsFreq(e)
  }

/*
  override def coldSyms(e: Any): List[Sym[Any]] = e match {
    case IfThenElse(c, t, e) => syms(t) ++ syms(e)
    case _ => super.coldSyms(e)
  }
*/

  override def boundSyms(e: Any): Vector[Exp[_]] = e match {
    case IfThenElse(c, t, e) => effectSyms(t) ++ effectSyms(e)
    case _ => super.boundSyms(e)
  }

}
/*

trait IfThenElseFatExp extends IfThenElseExp with BaseFatExp {

  abstract class AbstractFatIfThenElse extends FatDef {
    val cond: Exp[Boolean]
    val thenp: List[Block[Any]]
    val elsep: List[Block[Any]]
    
    var extradeps: List[Exp[Any]] = Nil //HACK
  }

  case class SimpleFatIfThenElse(cond: Exp[Boolean], thenp: List[Block[Any]], elsep: List[Block[Any]]) extends AbstractFatIfThenElse

/* HACK */

  override def syms(e: Any): List[Sym[Any]] = e match {
    case x@SimpleFatIfThenElse(c, t, e) => super.syms(x) ++ syms(x.extradeps)
    case _ => super.syms(e)
  }

/* END HACK */


  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case SimpleFatIfThenElse(c, t, e) => effectSyms(t):::effectSyms(e)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case x@SimpleFatIfThenElse(c, t, e) => freqNormal(c) ++ freqCold(t) ++ freqCold(e)    ++ freqNormal(x.extradeps)
    case _ => super.symsFreq(e)
  }

  // aliasing / sharing
  
  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case SimpleFatIfThenElse(c,a,b) => syms(a):::syms(b)
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case SimpleFatIfThenElse(c,a,b) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case SimpleFatIfThenElse(c,a,b) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case SimpleFatIfThenElse(c,a,b) => Nil // could return a,b but implied by aliasSyms
    case _ => super.copySyms(e)
  }
}

*/
/*

trait IfThenElseExpOpt extends IfThenElseExp { this: BooleanOpsExp with EqualExpBridge =>
  
  //TODO: eliminate conditional if both branches return same value!

  // it would be nice to handle rewrites in method ifThenElse but we'll need to
  // 'de-reify' blocks in case we rewrite if(true) to thenp. 
  // TODO: make reflect(Reify(..)) do the right thing
  
  override def __ifThenElse[T:Manifest](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T])(implicit pos: SourceContext) = cond match {
    case Const(true) => thenp
    case Const(false) => elsep
    case Def(BooleanNegate(a)) => __ifThenElse(a, elsep, thenp)
    case Def(NotEqual(a,b)) => __ifThenElse(equals(a,b), elsep, thenp)
    case _ =>
      super.__ifThenElse(cond, thenp, elsep)
  }
}


*/
