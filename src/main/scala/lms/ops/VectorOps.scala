package scala.lms
package ops

import java.io.PrintWriter
import org.scala_lang.virtualized.SourceContext
import scala.lms.util._

trait VectorOps extends Base with OverloadHack {

  // multiple definitions needed because implicits won't chain
  // not using infix here because apply doesn't work with infix methods
  //implicit def varToArrayOps[T:Manifest](x: Var[Array[T]]) = new ArrayOpsCls(readVar(x))
  implicit def repVectorToVectorOps[T:Manifest](a: Rep[Vector[T]]) = new VectorOpsCls(a)
  implicit def vectorToVectorOps[T:Manifest](a: Vector[T]) = new VectorOpsCls(unit(a))

  // substitution for "new Array[T](...)"



 /* object Vector {
    def apply[T:Manifest](xs: Rep[T]*) = vector_obj_fromseq(xs)
  }*/

  class VectorOpsCls[T:Manifest](a: Rep[Vector[T]]){
    def apply(n: Rep[Int])(implicit pos: SourceContext) = vector_apply(a, n)
    def update(n: Rep[Int], y: Rep[T])(implicit pos: SourceContext) = vector_update(a,n,y)
    def length(implicit pos: SourceContext) = vector_length(a)
    //def foreach(block: Rep[T] => Rep[Unit])(implicit pos: SourceContext) = vector_foreach(a, block)
    def sort(implicit pos: SourceContext) = vector_sort(a)
    def map[B:Manifest](f: Rep[T] => Rep[B]) = vector_map(a,f)
    def toSeq = vector_toseq(a)
    def slice(start:Rep[Int], end:Rep[Int]) = vector_slice(a,start,end)
  }


  def vector_obj_fromseq[T:Manifest](xs: Seq[Rep[T]]): Rep[Vector[T]]
  def vector_apply[T:Manifest](x: Rep[Vector[T]], n: Rep[Int])(implicit pos: SourceContext): Rep[T]
  def vector_update[T:Manifest](x: Rep[Vector[T]], n: Rep[Int], y: Rep[T])(implicit pos: SourceContext): Rep[Vector[T]]

  def vector_length[T:Manifest](x: Rep[Vector[T]])(implicit pos: SourceContext) : Rep[Int]
  //def vector_foreach[T:Manifest](x: Rep[Vector[T]], block: Rep[T] => Rep[Unit])(implicit pos: SourceContext): Rep[Unit]


  def vector_sort[T:Manifest](x: Rep[Vector[T]])(implicit pos: SourceContext): Rep[Vector[T]]
  def vector_map[A:Manifest,B:Manifest](a: Rep[Vector[A]], f: Rep[A] => Rep[B]): Rep[Vector[B]]
  def vector_toseq[A:Manifest](a: Rep[Vector[A]]): Rep[Seq[A]]
  def vector_slice[A:Manifest](a: Rep[Vector[A]], start:Rep[Int], end:Rep[Int]): Rep[Vector[A]]
}

trait VectorOpsExp extends VectorOps with EffectExp {
  case class VectorFromSeq[T:Manifest](xs: Seq[Exp[T]]) extends Def[Vector[T]] {
    val m = manifest[T]
  }
  case class VectorApply[T:Manifest](a: Exp[Vector[T]], n: Exp[Int]) extends Def[T]
  case class VectorUpdate[T:Manifest](a: Exp[Vector[T]], n: Exp[Int], y: Exp[T]) extends Def[Vector[T]]
  case class VectorLength[T:Manifest](a: Exp[Vector[T]]) extends Def[Int] {
    val m = manifest[T]
  }

  case class VectorCopy[T:Manifest](src: Exp[Vector[T]], srcPos: Exp[Int], dest: Exp[Vector[T]], destPos: Exp[Int], len: Exp[Int]) extends Def[Unit] {
    val m = manifest[T]
  }
  case class VectorSort[T:Manifest](x: Exp[Vector[T]]) extends Def[Vector[T]] {
    val m = manifest[T]
  }
  case class VectorToSeq[A:Manifest](x: Exp[Vector[A]]) extends Def[Seq[A]]
  case class VectorSlice[A:Manifest](a: Exp[Vector[A]], s:Exp[Int], e:Exp[Int]) extends Def[Vector[A]]


  def vector_obj_fromseq[T:Manifest](xs: Seq[Exp[T]]) = /*reflectMutable(*/ VectorFromSeq(xs) /*)*/
  def vector_apply[T:Manifest](x: Exp[Vector[T]], n: Exp[Int])(implicit pos: SourceContext): Exp[T] = VectorApply(x, n)
  def vector_update[T:Manifest](x: Exp[Vector[T]], n: Exp[Int], y: Exp[T])(implicit pos: SourceContext) =  VectorUpdate(x,n,y)
  def vector_unsafe_update[T:Manifest](x: Rep[Vector[T]], n: Rep[Int], y: Rep[T])(implicit pos: SourceContext) = VectorUpdate(x,n,y)
  def vector_length[T:Manifest](a: Exp[Vector[T]])(implicit pos: SourceContext) : Rep[Int] = VectorLength(a)

  def vector_copy[T:Manifest](src: Exp[Vector[T]], srcPos: Exp[Int], dest: Exp[Vector[T]], destPos: Exp[Int], len: Exp[Int])(implicit pos: SourceContext) = ??? //reflectWrite(dest)(VectorCopy(src,srcPos,dest,destPos,len))
  def vector_unsafe_copy[T:Manifest](src: Exp[Vector[T]], srcPos: Exp[Int], dest: Exp[Vector[T]], destPos: Exp[Int], len: Exp[Int])(implicit pos: SourceContext) = VectorCopy(src,srcPos,dest,destPos,len)
  def vector_sort[T:Manifest](x: Exp[Vector[T]])(implicit pos: SourceContext) = VectorSort(x)
  def vector_map[A:Manifest,B:Manifest](a: Exp[Vector[A]], f: Exp[A] => Exp[B]) = {
    ???
    /*    val x = fresh[A]
        val b = reifyEffects(f(x))
        reflectEffect(VectorMap(a, x, b), summarizeEffects(b))*/
  }
  def vector_toseq[A:Manifest](a: Exp[Vector[A]]) = VectorToSeq(a)
  def vector_slice[A:Manifest](a: Rep[Vector[A]], start:Rep[Int], end:Rep[Int]) = VectorSlice(a,start,end)

  //////////////
  // mirroring

  /*override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case VectorApply(a,x) => vector_apply(f(a),f(x))(mtype(manifest[A]),pos)
    case VectorLength(x) => vector_length(f(x))
    case e@VectorSort(x) => vector_sort(f(x))(e.m,pos)
    case e@VectorCopy(a,ap,d,dp,l) => toAtom(VectorCopy(f(a),f(ap),f(d),f(dp),f(l))(e.m))(mtype(manifest[A]),pos)
    case Reflect(e@VectorNew(n), u, es) => reflectMirrored(Reflect(VectorNew(f(n))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@VectorLength(x), u, es) => reflectMirrored(Reflect(VectorLength(f(x))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(VectorApply(l,r), u, es) => reflectMirrored(Reflect(VectorApply(f(l),f(r))(mtype(manifest[A])), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@VectorSort(x), u, es) => reflectMirrored(Reflect(VectorSort(f(x))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(VectorUpdate(l,i,r), u, es) => reflectMirrored(Reflect(VectorUpdate(f(l),f(i),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@VectorCopy(a,ap,d,dp,l), u, es) => reflectMirrored(Reflect(VectorCopy(f(a),f(ap),f(d),f(dp),f(l))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??*/


}