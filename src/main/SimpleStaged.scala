import scala.lms.BaseExp
import scala.lms.internal.FunctionsExp
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.graphviz.{GraphVizCallGraph, GraphVizExport}
import scala.lms.targets.scalalike._

/**
  * Created by rayda on 06-Apr-17.
  */
object SimpleStaged extends App {

  class DSL extends Header {
    self =>
    val emitGraph = new GraphVizExport {
      override val IR: self.type = self
    }
    override val codegen = new ScalaCodegen with EmitHeadNoTuples with ScalaGenPrimitivOps with ScalaGenSpiral_DSL with ScalaGenBooleanOps with ScalaGenIfThenElse with ScalaGenOrderingOps {
      val IR: self.type = self
    }
    val Primes_low = Set(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53,
      59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139,
      149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239,
      241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349,
      353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457,
      461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577,
      587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683,
      691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821,
      823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941,
      947, 953, 967, 971, 977, 983, 991, 997)


    def exposeComplexVector(size: Int): ExposeRep[Vector[Complex]] = {
      new ExposeRep[Vector[Complex]]() {
        val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => {
          val x = (0 until size).foldLeft(Vector.empty[Exp[_]])((acc, ele) => {
            acc ++ Vector(Arg[Double], Arg[Double])
          })
          x
        }
        val vec2t: Vector[Exp[_]] => Vector[Complex] = (in: Vector[Exp[_]]) => {
          val t = in.grouped(2).map(pair => Complex(pair(0).asInstanceOf[Rep[Double]], pair(1).asInstanceOf[Rep[Double]])).toVector
          assert(in.size == t.size * 2)
          t
        }
        val t2vec: Vector[Complex] => Vector[Exp[_]] = (in: Vector[Complex]) => {
          val t = in.map(v => Vector(v.re, v.im)).flatten.toVector
          t
        }
      }
    }


    def myprog(in: Vector[Complex]): Vector[Complex] = {
      val t = in
      comp(in, in.size, None)
    }

    def graphexport(path: String = "F:\\Phd\\git\\code\\SpaceTime\\", name: String = "Graph.dot") = {
      val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream(path + name))
      //dumpCode (stream2)
      val (str, esc) = emitGraph.emitDepGraphf(myprog)(exposeComplexVector(16), exposeComplexVector(16))
      stream2.println(str)
      stream2.flush()
      stream2.close()
      this.graphname = false
    }


    case class Complex(val re: Rep[Double], val im: Rep[Double]) {
      def +(rhs: Complex): Complex = {

        Complex(re + rhs.re, im + rhs.im)
      }

      def -(rhs: Complex): Complex = {

        Complex(re - rhs.re, im - rhs.im)
      }

      def *(rhs: Complex): Complex = Complex(re * rhs.re - im * rhs.im, re * rhs.im + im * rhs.re)

    }


    def DivisorPairs(n: Int): List[(Int, Int)] = {
      (2 to Math.sqrt(n).toInt).filter(n % _ == 0).flatMap(x => (if (n / x == x) List(x) else List(n / x, x))).toList.sortWith(_ > _).map(x => (n / x, x))
    }

    //returns a factor 2 <= x <= n/2 where n%x == 0
    def divisior(n: Int): (Int, Int) = {
      val t = DivisorPairs(n)
      println(DivisorPairs(64))
      t(0)
    }

    def isPrime(n: Int): Boolean = Primes_low.contains(n)

    def comp(x: Vector[Complex], size: Int, scaling: Option[Vector[Double]]): Vector[Complex] = {
      if (size == 2) basecase(x, size, scaling)
      else if (isPrime(size)) primecase(x, size, scaling)
      else divcase(x, size, scaling)
    }

    def basecase(x: Vector[Complex], size: Int, scaling: Option[Vector[Double]]): Vector[Complex] = {
      Vector(x(0) + x(1), x(0) - x(1))
    }

    def primecase(x: Vector[Complex], size: Int, scaling: Option[Vector[Double]]): Vector[Complex] = {
      val ti = (size / 2) * 2 //highest twopower
      comp(x.take(ti), ti, scaling) ++ x.drop(ti)
    }

    def gf[T](x: Vector[T], n: Int, i: Int): Vector[T] = x.grouped(n).toVector(i)

    def ogf[T](x: Option[Vector[T]], n: Int, i: Int): Option[Vector[T]] = x.map(s => s.grouped(n).toVector(i))

    def loop(lb: Int, f: Int => Vector[Complex]): Vector[Complex] = {
      (for (i <- 0 until lb) yield f(i)).flatten.toVector
    }

    def divcase(x: Vector[Complex], size: Int, scaling: Option[Vector[Double]]): Vector[Complex] = {
      val (n, k) = divisior(size) //returns factors n and k where 2 <= n <= size/2 and n*k == size

      val part1 = loop(k, i => comp(gf(x, n, i), n, ogf(scaling, n, i)))

      val ns: Vector[Double] = (for (i <- 0 until size) yield Math.sin(2 * Math.PI / (i + 1))).toVector

      loop(n, i => comp(gf(part1, k, i), k, Some(gf(ns, k, i))))
    }
  }

  val dsl = new DSL

  dsl.graphexport()


}
