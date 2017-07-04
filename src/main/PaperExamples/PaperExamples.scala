package PaperExamples

/**
  * Created by rayda on 10-Nov-16.
  */
class PaperExamples {

  class GenericClass[T](val constructor_parm: T) {
    val member: T = constructor_parm

    def method[X](param: X): X = param
  }

  abstract class GenericClass2 {
    type T
    val member: T
  }


  def foo() = {

    val inst = new GenericClass(3) //implicit [Int]
    val inst_explicit = new GenericClass[Long](3)

    val refine_inst = new GenericClass2 {
      override type T = Int
      override val member: T = 3
    }
  }


  class A {
    def plus(l: A, r: A): A = ???
  }

  class B {
    def plus(l: B, r: B): B = ???
  }

  //def generic_method[T](p: T) = p.plus(p,p)

  case class Kelvin(k: Double)
  case class Celcius(c: Double){
    def add(other: Celcius) = Celcius(c + other.c)
  }  
  implicit def toCelcius(k: Kelvin) = Celcius(k.k + 273.15)
  def usage(k: Kelvin, c: Celcius): Celcius = c.add(k)

  def foo(s: String)(implicit celcius: Celcius): String = ???
  



  implicit def toOps[T](x: T)(implicit num: PlusTypeClass[T]): PlusTypeClass[T]#Ops = new num.Ops(x)
  trait PlusTypeClass[T] {
    def interface_plus(l: T, r: T): T
    class Ops(lhs: T) {
      def +(rhs: T) = interface_plus(lhs, rhs)
    }
  }

    object A_Evidence extends PlusTypeClass[A] {
      def interface_plus(l: A, r: A): A = l.plus(l, r)
    }

    object B_Evidence extends PlusTypeClass[B] {
      def interface_plus(l: B, r: B): B = l.plus(l, r)
    }

    def generic_method[T: PlusTypeClass](p: T) = {
      p + p
    }
  def generic_method_desugar[T]
  (p: T)(implicit ev: PlusTypeClass[T]) = {
    //ev = A_Evidence or B_Evidence at runtime
    val op: PlusTypeClass[T]#Ops = toOps(p)(ev)
    op.+(p)
  }


}
