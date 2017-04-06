package org.scala_lang.virtualized

import scala.reflect.macros.blackbox.Context
import language.experimental.macros
import scala.collection.mutable

/**
 * Converts Scala features that can not be overridden to method calls that can be given
 * arbitrary semantics.
 *
 * ==Features covered are==
 * {{{
 *   var x = e              =>       var x = __newVar(e)
 *   if(c) t else e         =>       __ifThenElse(c, t, e)
 *   return t               =>       __return(t)
 *   x = t                  =>       __assign(x, t)
 *   while(c) b             =>       __whileDo(c, b)
 *   do b while c           =>       __doWhile(c, b)
 * }}}
 *
 * ===Poor man's infix methods for `Any` methods===
 * {{{
 *   t == t1                =>       infix_==(t, t1)
 *   t != t1                =>       infix_!=(t, t1)
 *   t.##                   =>       infix_##(t, t1)
 *   t.equals t1            =>       infix_equals(t, t1)
 *   t.hashCode             =>       infix_hashCode(t)
 *   t.asInstanceOf[T]      =>       infix_asInstanceOf[T](t)
 *   t.isInstanceOf[T]      =>       infix_isInstanceOf[T](t)
 *   t.toString             =>       infix_toString(t)
 * }}}
 *
 * ===Poor man's infix methods for `AnyRef` methods===
 * {{{
 *   t eq t1                =>       infix_eq(t, t1)
 *   t ne t1                =>       infix_ne(t, t1)
 *   t.notify               =>       infix_notify(t)
 *   t.notifyAll            =>       infix_notifyAll(t)
 *   t.synchronized[T](t1)  =>       infix_synchronized(t, t1)
 *   t.wait                 =>       infix_wait(t)
 *   t.wait(l)              =>       infix_wait(t, l)
 *   t.wait(t1, l)          =>       infix_wait(t, t1, l)
 * }}}
 *
 * @todo
 * {{{
 *   try b catch c          =>       __tryCatch(b, c, f)
 *   throw e                =>       __throw(e)
 *   case class C { ... }   =>       ???
 *   Nothing                =>       ???
 *   Null                   =>       ???
 * }}}
 */
trait LanguageVirtualization extends MacroModule with TransformationUtils with DataDefs {
  import c.universe._

  def virtualize(t: Tree): (Tree, Seq[DSLFeature]) = VirtualizationTransformer(t)

  object VirtualizationTransformer {
    def apply(tree: Tree) = {
      val t = new VirtualizationTransformer().apply(tree)
      log("(virtualized, Seq[Features]): " + t, 2)
      t
    }
  }

  private class VirtualizationTransformer extends Transformer {
    val lifted = mutable.ArrayBuffer[DSLFeature]()

    def liftFeature(receiver: Option[Tree], nme: String, args: List[Tree], targs: List[Tree] = Nil): Tree = {
      lifted += DSLFeature(receiver.map(_.tpe), nme, targs, List(args.map(_.tpe)))
      log(show(method(receiver.map(transform), nme, List(args.map(transform)), targs)), 3)
      method(receiver.map(transform), nme, List(args.map(transform)), targs)
    }

    override def transform(tree: Tree): Tree = atPos(tree.pos) {
      tree match {
        // sstucki: It seems necessary to keep the MUTABLE flag in the
        // new ValDef set, otherwise it becomes tricky to
        // "un-virtualize" a variable definition, if necessary
        // (e.g. if the DSL does not handle variable definitions in a
        // special way).
        case ValDef(mods, sym, tpt, rhs) if mods.hasFlag(Flag.MUTABLE) =>
          ValDef(mods, sym, tpt, liftFeature(None, "__newVar", List(rhs)))

        case t @ If(cond, thenBr, elseBr) =>
          liftFeature(None, "__ifThenElse", List(cond, thenBr, elseBr))

        case Return(e) =>
          liftFeature(None, "__return", List(e))

        case Assign(lhs, rhs) =>
          liftFeature(None, "__assign", List(lhs, rhs))

        case LabelDef(sym, List(), If(cond, Block(body :: Nil, Apply(Ident(label),
          List())), Literal(Constant(())))) if label == sym => // while(){}
          liftFeature(None, "__whileDo", List(cond, body))

        case LabelDef(sym, List(), Block(body :: Nil, If(cond, Apply(Ident(label),
          List()), Literal(Constant(()))))) if label == sym => // do while(){}
          liftFeature(None, "__doWhile", List(cond, body))

//        case Apply(Select(qualifier, TermName("$plus")), List(arg)) =>
//          liftFeature(None, "infix_$plus", List(qualifier, arg))

        case Apply(Select(qualifier, TermName("$eq$eq")), List(arg)) =>
          liftFeature(None, "infix_$eq$eq", List(qualifier, arg))

        case Apply(Select(qualifier, TermName("$bang$eq")), List(arg)) =>
          liftFeature(None, "infix_$bang$eq", List(qualifier, arg))

        case Apply(lhs @ Select(qualifier, TermName("$hash$hash")), List()) =>
          liftFeature(None, "infix_$hash$hash", List(qualifier))

        case Apply(lhs @ Select(qualifier, TermName("equals")), List(arg)) =>
          liftFeature(None, "infix_equals", List(qualifier, arg))

        case Apply(lhs @ Select(qualifier, TermName("hashCode")), List()) =>
          liftFeature(None, "infix_hashCode", List(qualifier))

        case TypeApply(Select(qualifier, TermName("asInstanceOf")), targs) =>
          liftFeature(None, "infix_asInstanceOf", List(qualifier), targs)

        case TypeApply(Select(qualifier, TermName("isInstanceOf")), targs) =>
          liftFeature(None, "infix_isInstanceOf", List(qualifier), targs)

        case Apply(lhs @ Select(qualifier, TermName("toString")), List()) =>
          liftFeature(None, "infix_toString", List(qualifier))

        case Apply(lhs @ Select(qualifier, TermName("eq")), List(arg)) =>
          liftFeature(None, "infix_eq", List(qualifier, arg))

        case Apply(lhs @ Select(qualifier, TermName("ne")), List(arg)) =>
          liftFeature(None, "infix_ne", List(qualifier, arg))

        case Apply(Select(qualifier, TermName("notify")), List()) =>
          liftFeature(None, "infix_notify", List(qualifier))

        case Apply(Select(qualifier, TermName("notifyAll")), List()) =>
          liftFeature(None, "infix_notifyAll", List(qualifier))

        case Apply(Select(qualifier, TermName("synchronized")), List(arg)) =>
          liftFeature(None, "infix_synchronized", List(qualifier, arg))

        case Apply(TypeApply(Select(qualifier, TermName("synchronized")), targs), List(arg)) =>
          liftFeature(None, "infix_synchronized", List(qualifier, arg), targs)

        case Apply(Select(qualifier, TermName("wait")), List()) =>
          liftFeature(None, "infix_wait", List(qualifier))

        case Apply(Select(qualifier, TermName("wait")), List(arg)
          ) if arg.tpe =:= typeOf[Long] =>
          liftFeature(None, "infix_wait", List(qualifier, arg))

        case Apply(Select(qualifier, TermName("wait")), List(arg0, arg1)
          ) if arg0.tpe =:= typeOf[Long] && arg1.tpe =:= typeOf[Int] =>
          liftFeature(None, "infix_wait", List(qualifier, arg0, arg1))

        case Try(block, catches, finalizer) => {
          c.warning(tree.pos, "virtualization of try/catch expressions is not supported.")
          super.transform(tree)
        }

        case Throw(expr) => {
          c.warning(tree.pos, "virtualization of throw expressions is not supported.")
          super.transform(tree)
        }

        case ClassDef(mods, n, _, _) if mods.hasFlag(Flag.CASE) =>
          // sstucki: there are issues with the ordering of
          // virtualization and expansion of case classes (i.e. some
          // of the expanded code might be virtualized even though it
          // should not be and vice-versa).  So until we have decided
          // how proper virtualization of case classes should be done,
          // any attempt to do so should fail.
          // TR: not 100% sure what the issue is (although i vaguely
          // remember that we had issues in Scala-Virtualized with 
          // auto-generated case class equality methods using virtualized
          // equality where it shouldn't). For the moment it seems like 
          // just treating case classes as regular classes works fine.
          //println(tree)
          c.warning(tree.pos, "virtualization of case classes is not fully supported.")
          super.transform(tree) 
        case _ =>
          super.transform(tree)
      }
    }
    def apply(tree: c.universe.Tree): (Tree, Seq[DSLFeature]) =
      (transform(tree), lifted.toSeq)
  }

}
