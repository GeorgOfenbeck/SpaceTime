package SpiralSThesisGui

import scala.util.Random

/**
  * Created by rayda on 06-Mar-17.
  */
object NewTwiddles extends org.scalacheck.Properties("Twiddle Testing") {

  import org.scalacheck.{Gen, Prop, Arbitrary}


  def twopower(): Gen[Int] = {
    val rand = new Random()
    val n = rand.nextInt(14)
    Math.pow(2, n).toInt
  }

  def yieldk(n: Int) = {
    //TODO - find short form for return value
    def tmp() = {
      for (k <- 0 until n
           // this if checks if x^t becomes 1 before n==t, this is e.g. the
           // case for 2nd root of unity of 4 where it becomes 1 at x^2
           if (for (t <- 2 until n - 1
                    if (Math.cos(2 * math.Pi * k * t / n) == 1)
           ) yield 1).isEmpty
      )
        yield k
    }

    tmp.last
  }

  def yieldnew(n: Int):Int = {
    var k = n
    var nonzero = true
    while (k > 0 && nonzero) {
      k = k - 1
      var t = 2
      nonzero = false
      while (t < n - 1 && !nonzero) {
        val one = (Math.cos(2 * math.Pi * k * t / n) == 1)
        nonzero = nonzero || one
        t = t + 1
      }
    }
    k
  }


  property("root of unity") = {
    Prop.forAll(twopower()) {
      twop => {
        yieldk(twop) == yieldnew(twop)
      }
    }
  }
}
