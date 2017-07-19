package scala.lms
package internal

import scala.virtualization.lms.util.ClosureCompare


trait ExposeRepBase extends Expressions {

  trait ExposeRep[T] {
    def freshExps(): Vector[Exp[_]]
    def vec2t(v: Vector[Exp[_]]): T
    def t2vec(t: T): Vector[Exp[_]]
  }

}

trait Functions extends Base with ExposeRepBase {

  var recurse_toggle: Boolean = true
  
  var funTable: Map[Any, StagedFunction[_, _]] = Map.empty

  //def doLambda[A, R](f: Function1[A, R], hot: Boolean, recuse: Boolean)(implicit args: ExposeRep[A], returns: ExposeRep[R]): StagedFunction[A, R]

  //def doInternalLambda[A, R](f: Function1[A, R], hot: Boolean, recuse: Boolean)(implicit args: ExposeRep[A], returns: ExposeRep[R]): StagedFunction[A, R]

  //def doGlobalLambda[A, R](f: Function1[A, R], recuse: Boolean, name: Option[String])(implicit args: ExposeRep[A], returns: ExposeRep[R]): StagedFunction[A, R]

  def doLambda[A, R](f: Function1[A, R], hot: Boolean, recuse: Option[String])(implicit args: ExposeRep[A], returns: ExposeRep[R]): StagedFunction[A, R]

  def doInternalLambda[A, R](f: Function1[A, R], hot: Boolean, recuse: Option[String])(implicit args: ExposeRep[A], returns: ExposeRep[R]): StagedFunction[A, R]

  def doGlobalLambda[A, R](f: Function1[A, R], recuse: Option[String], name: Option[String])(implicit args: ExposeRep[A], returns: ExposeRep[R]): StagedFunction[A, R]


  //implicit def fun[A, R](f: Function1[A, R], hot: Boolean)(implicit args: ExposeRep[A], returns: ExposeRep[R]): StagedFunction[A, R] = doLambda(f, hot)

  //implicit def internalfun[A, R](f: Function1[A, R], hot: Boolean)(implicit args: ExposeRep[A], returns: ExposeRep[R]): StagedFunction[A, R] = doInternalLambda(f, hot)

  //def doApply[A,R](fun: Rep[_ => _], arg: A)(implicit args: ExposeRep[A], returns: ExposeRep[R]): R

  case class StagedFunction[A, R](f: A => R, exp: Rep[_ => _], args: ExposeRep[A], returns: ExposeRep[R], name: Option[String] = None)

  implicit def toLambdaOps[A, R](fun: StagedFunction[A, R]): LambdaOps[A, R] = new LambdaOps(fun)

  class LambdaOps[A, R](f: StagedFunction[A, R]) {
    def apply(x: A): R = doApplySF(f, x, f.args, f.returns)
  }

  def doApplySF[A, R](fun: StagedFunction[A, R], arg: A, args: ExposeRep[A], returns: ExposeRep[R]): R
}


trait FunctionsExp extends Functions with BaseExp with ClosureCompare with EffectExp {
  implicit def liftFunction2[T, R](implicit t: TypeRep[T], r: TypeRep[R]): TypeRep[T => R] = typeRep[T => R]

  implicit def exposeFunction[A, R](implicit args: ExposeRep[A], returns: ExposeRep[R]): ExposeRep[StagedFunction[A, R]] =
    new ExposeRep[StagedFunction[A, R]]() {
      def freshExps(): Vector[Exp[_]] = {
        def helper[T]()(implicit tag: TypeRep[T]): TypeRep[T] = {
          tag match {
            case x@TypeExp(mf, dynTags) => {
              val f: Unit => (Vector[TypeRep[_]], Vector[TypeRep[_]]) = (u: Unit) => {
                val t = args.freshExps()
                val a = t.map(ele => exp2tp(ele).tag)
                val r = returns.freshExps().map(ele => exp2tp(ele).tag)
                (a, r)
              }
              x.copy(dynTags = Some(f))
            }
            case _ => {
              assert(false, "this should never match")
              tag
            }
          }
        }
        val tagnew = helper[Any => Any]()
        val lambda: Exp[Function[Any, Any]] = Arg[Any => Any](tagnew)
        Vector(lambda)
      }
      def vec2t(in: Vector[Exp[_]]): StagedFunction[A, R] = {
        val f: (A => R) = (ina: A) => ???
        StagedFunction(f, in.head.asInstanceOf[Rep[_ => _]], args, returns)
      }
      def t2vec(in: StagedFunction[A, R]): Vector[Exp[_]] = {
        Vector(in.exp)
      }
    }

  case class ReturnArg(f: Exp[_], newsym: Exp[_], pos: Int, tuple: Boolean, last: Boolean) extends Def[Any]

  abstract class AbstractLambda[CA, CR](val f: Function1[Vector[Exp[_]], Vector[Exp[_]]], val x: Vector[TP[_]], val y: Block, val hot: Boolean,
                                        val args: ExposeRep[CA], val returns: ExposeRep[CR]) extends Def[_ => _]

  //seems we can only put any here since we cannot know at compile time
  case class InternalLambda[CA, CR](override val f: Function1[Vector[Exp[_]], Vector[Exp[_]]], override val x: Vector[TP[_]],
                                    override val y: Block,
                                    override val hot: Boolean,
                                    override val args: ExposeRep[CA],
                                    override val returns: ExposeRep[CR]) extends AbstractLambda(f, x, y, hot, args, returns)

  //seems we can only put any here since we cannot know at compile time
  case class ExternalLambda[CA, CR](override val f: Function1[Vector[Exp[_]], Vector[Exp[_]]], override val x: Vector[TP[_]],
                                    override val y: Block,
                                    override val hot: Boolean,
                                    override val args: ExposeRep[CA],
                                    override val returns: ExposeRep[CR],
                                    global: Boolean, name: Option[String] = None) extends AbstractLambda(f, x, y, hot, args, returns)





  case class InternalApply[CA, CR](f: Exp[_ => _], arg: Vector[Exp[_]], name: Option[String] = None) extends Def[Any]

  //RF (any?)





  def doLambdaDef[A, R](f: Function1[A, R], internal: Boolean, hot: Boolean, global: Boolean, recuse: Option[String], name: Option[String] = None)
                       (implicit args: ExposeRep[A], returns: ExposeRep[R]): AbstractLambda[A, R] = {

      addBlockTPBuffer()
      val freshexps = args.freshExps()
      val tps = freshexps map (exp => id2tp(exp.id))
      val vecf = (in: Vector[Exp[_]]) => {
        val container = args.vec2t(in)
        val tres = f(container)
        val hres = returns.t2vec(tres)
        hres
      }

      val explist = vecf(freshexps)
      val summary = summarizeContext()
      val t = context
      val block = if (context.isEmpty && mustPure(summary))
        Block(explist)
      else {
        //RF! pruneContext ? (check what this is supposed to do)
        val deps = context.map(e => e._1)
        Block(explist, summary, pruneContext(deps)) // calls toAtom...
      }

      block2tps = block2tps + (block -> getBlockTPBuffer())
      if (internal)
        InternalLambda(vecf, tps, block, hot, args, returns)
      else
        ExternalLambda(vecf, tps, block, hot, args, returns, global, name)
  }


  //this is called by the apply of StagedFunction
  def doApplySF[A, R](fun: StagedFunction[A, R], arg: A, args: ExposeRep[A], returns: ExposeRep[R]): R = {
    val newsyms = returns.freshExps()
    val funexp = fun.exp
    val applynode = InternalApply(funexp, args.t2vec(arg),fun.name) //, Block(newsyms))
    val applynodeexp = toAtom(applynode)
    val returnNodes = if (newsyms.size > 1) {
      newsyms.zipWithIndex.map(fsym => {
        //had do to this ugly version since the compile time type is not know at this stage (of the return - Rep[_])
        val otp = exp2tp(fsym._1)
        val tag: TypeRep[Any] = otp.tag.asInstanceOf[TypeRep[Any]]
        val cc: Def[Any] = ReturnArg(applynodeexp, fsym._1, fsym._2, true, newsyms.size == fsym._2 + 1)
        //val newx = toAtom(cc)(tag, null)
        val newx = toAtom(cc)(null,tag)
        newx
      })
    } else {
      newsyms.zipWithIndex.map(fsym => {
        val tag: TypeRep[Any] = exp2tp(fsym._1).tag.asInstanceOf[TypeRep[Any]]
        val cc: Def[Any] = ReturnArg(applynodeexp, fsym._1, fsym._2, false, true)
        val newx = toAtom(cc)(null,tag)
        //val newx = toAtom(cc)(tag, null)
        newx
      })
    }
    returns.vec2t(returnNodes)
  }


  def doAbstractLambda[A, R](f: Function1[A, R], internal: Boolean, hot: Boolean, global: Boolean, recurse: Option[String], name: Option[String] = None)(implicit args: ExposeRep[A], returns: ExposeRep[R]): StagedFunction[A, R] = {
    def helper[T](d: Def[T])(implicit tag: TypeRep[T]): TypeRep[T] = {
      tag match {
        case x@TypeExp(mf, dynTags) => {
          val f: Unit => (Vector[TypeRep[_]], Vector[TypeRep[_]]) = (u: Unit) => {
            val a = args.freshExps().map(ele => exp2tp(ele).tag)
            val r = returns.freshExps().map(ele => exp2tp(ele).tag)
            (a, r)
          }
          x.copy(dynTags = Some(f))
        }
        case _ => {
          assert(false, "this should never match")
          tag
        }
      }
    }
    if (recurse.isDefined) {
      val can = recurse.get
      /*recurse_toggle = false

      {
        val expx: Rep[_ => _] = fresh[_ => _]
        val sf = StagedFunction(f, expx, args, returns, name)
        val y: AbstractLambda[A, R] = doLambdaDef(f, internal, hot, global, recurse, name)(args, returns)
        val tag = helper(y)
        val tp = createDefinition(expx, y)(tag) //creates tp and stores it
        recurse_toggle = true
        sf
      }*/


      if (funTable.contains( can )) {
        val t: StagedFunction[_, _] = funTable(can)
        t.asInstanceOf[StagedFunction[A, R]]
      }
      else {
        {
          val expx: Rep[_ => _] = fresh[_ => _]
          val sf = StagedFunction(f, expx, args, returns, name)
          funTable = funTable + (can -> sf)

          val y: AbstractLambda[A, R] = doLambdaDef(f, internal, hot, global, recurse, name)(args, returns)
          val tag = helper(y)
          val tp = createDefinition(expx, y)(tag) //creates tp and stores it
          sf
        }
      }
    }
    else {
      val expx: Rep[_ => _] = fresh[_ => _]
      //case class StagedFunction[A, R](f: A => R, exp: Rep[_ => _], args: ExposeRep[A], returns: ExposeRep[R], name: Option[String] = None)
      val sf = StagedFunction(f, expx, args, returns, name)
      val y: AbstractLambda[A, R] = doLambdaDef(f, internal, hot, global, recurse, name)(args, returns)
      val tag = helper(y)
      val tp = createDefinition(expx, y)(tag)
      sf
    }
  }

  override def doLambda[A, R](f: Function1[A, R], hot: Boolean, recuse: Option[String])(implicit args: ExposeRep[A], returns: ExposeRep[R]): StagedFunction[A, R] = {
    doAbstractLambda(f, false, hot, false, recuse)
  }

  override def doInternalLambda[A, R](f: Function1[A, R], hot: Boolean, recuse: Option[String])(implicit args: ExposeRep[A], returns: ExposeRep[R]): StagedFunction[A, R] = {
    doAbstractLambda(f, true, hot, false, recuse)
  }

  override def doGlobalLambda[A, R](f: Function1[A, R], recuse: Option[String], name: Option[String] = None)(implicit args: ExposeRep[A], returns: ExposeRep[R]): StagedFunction[A, R] = {
    doAbstractLambda(f, false, true, true, recuse, name)
  }




  override def syms(e: Any): Vector[Exp[_]] = e match {
    case InternalLambda(f, x, y, hot, args, returns) => {
      Vector.empty
    }
    case ExternalLambda(f, x, y, hot, args, returns, global, name) => {
      Vector.empty
    }
    case _ => {
      super.syms(e)
    }
  }

  override def boundSyms(e: Any): Vector[Exp[_]] = e match {
    case a@InternalApply(f, arg, name) => {
      Vector.empty
    }
    case l@InternalLambda(f, x, y, hot, _, _) => {
      //The parameters of the function need to be bound
      val exps = l.x map (tp => tp.sym)
      val t = syms(exps)
      t
    }
    case l@ExternalLambda(f, x, y, hot, _, _, _, name) => {
      val exps = l.x map (tp => tp.sym)
      val t = syms(exps)
      t
    }
    case _ => {
      super.boundSyms(e)
    }
  }


  override def symsFreq(e: Any): Vector[(Exp[_], Double)] = e match {
    case TP(sym, rhs, tag) => rhs match {
      //case InternalLambda(f, x, y, hot, args, returns) => if (hot) freqHot(sym) else freqCold(sym)
      //case ExternalLambda(f, x, y, hot, args, returns, global, name) => if (hot) freqHot(sym) else freqCold(sym)
      case _ => super.symsFreq(e)
    }
    case _ => super.symsFreq(e)
  }

  override def reset() = {
    funTable = Map.empty
    super.reset()
  }


}

