package Filter

/**
  * Created by rayda on 01-Nov-16.
  */
object DesignQuestion extends App{

  trait NumSelector{
    val rrep: Boolean

    def repselect[T](a: T, ev: Numeric[T]): Option[T] =
      if (rrep) {
        if (ev.zero.isInstanceOf[Int]) Some(a) else None
      } else if (ev.zero.isInstanceOf[Int]) None else Some(a)
  }

  trait IntSelector extends NumSelector{
    val rrep: Boolean = true
  }

  trait DoubleSelector extends NumSelector{
    val rrep: Boolean = false
  }

  abstract class Base[A: Numeric, B: Numeric](a: A, b: B)

  abstract class NumericHeader[A: Numeric, B: Numeric](a: A, b: B) extends Base(a,b) with NumSelector{
    def a(): Option[A] =  repselect(a,implicitly[Numeric[A]])
    def b(): Option[B] =  repselect(b,implicitly[Numeric[B]])
  }

  class IntOnly[A: Numeric, B: Numeric](a: A, b: B, i: Int) extends NumericHeader(a,b) with IntSelector

  class DoubleOnly[A: Numeric, B: Numeric](d: Double, a: A, b: B) extends NumericHeader(a,b) with DoubleSelector

  case class Mix[A: Numeric, B: Numeric](d: Double, a: A, b: B, i: Int) extends Base(a,b)

  object Mix{
    private def choose[T](a: Option[T], b: Option[T], ev: Numeric[T]): T = if (ev.zero.isInstanceOf[Int]) b.get else a.get

    def apply[A: Numeric, B: Numeric](hi: IntOnly[A,B], hd: DoubleOnly[A,B]) = {
      val a: A = choose(hi.a(),hd.a(),implicitly[Numeric[A]])
      val b: B = choose(hi.b(),hd.b(),implicitly[Numeric[B]])
    }
  }


}
