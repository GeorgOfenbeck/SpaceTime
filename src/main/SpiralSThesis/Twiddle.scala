package SpiralSThesis

/**
  * Created by rayda on 29-Dec-16.
  */
object Twiddle {

  var TMap = Map.empty[(Int, Int, Int), ComplexVector]

  object MathUtilities {

    def dLin(N: Int, a: Double, b: Double): Array[Double] = {
      val t_array = new Array[Double](N)
      for (i <- 0 until N)
        t_array(i) = a * i + b
      t_array
    }

    def diagTensor(a: Array[Double], b: Array[Double]): Array[Double] = {
      val t_array = new Array[Double](a.size * b.size)
      for (i <- 0 until a.size)
        for (j <- 0 until b.size)
          t_array(i * b.size + j) = a(i) * b(j)
      t_array
    }
  }

  def apply(x: ComplexVector, n: Int, d: Int, k: Int): ComplexVector = {
    val diag = MathUtilities.diagTensor(MathUtilities.dLin(n / d, 1, 0), MathUtilities.dLin(d, 1, 0))
    val t = E(n)
    val root_list_re = diag map (ele => t.re(ele.toInt * k))
    val root_list_im = diag map (ele => t.im(ele.toInt * k))

    for (i <- 0 until root_list_re.size) {
      val u = Complex(root_list_re(i), root_list_im(i))
      //val idx = vrep(yi)f
      val tx = x.apply(i)
      x.update(i, tx * u)
    }
    x
  }

  def apply(n: Int, d: Int, k: Int, i: Int): Complex = {

    if (!TMap.contains((n, d, k))) {
      val diag = MathUtilities.diagTensor(MathUtilities.dLin(n / d, 1, 0), MathUtilities.dLin(d, 1, 0))
      val t = E(n)
      val root_list_re = diag map (ele => t.re(ele.toInt * k))
      val root_list_im = diag map (ele => t.im(ele.toInt * k))

      val cv = new ComplexVector(new Array[Complex](root_list_re.size))
      for (i <- 0 until root_list_re.size) {
        val u = Complex(root_list_re(i), root_list_im(i))
        cv.update(i, u)
      }
      TMap = TMap + ((n, d, k) -> cv)
    }
    val cv = TMap.get((n, d, k))
    cv.get(i)
  }


  def DFT(n: Int): Vector[ComplexVector] = {
    val m = new Array[ComplexVector](n)
    val k = 1
    val t_e = E(n)
    for (x <- 0 until n)
      m(x) = new ComplexVector(new Array[Complex](n))
    for (x <- 0 until n)
      for (y <- 0 until n) {

        m(x).update(y, new Complex(t_e.re(x * y * k), t_e.im(x * y * k)))
      }
    m.toVector
  }


  def WHT(n: Int, x: Int, y: Int): Complex = {
    //1* 1*
    //1* -1*


    val t = if (n == 1) new Complex(1, 0) /*{
      if (rx == 1 && ry == 1) new Complex(-1, 0) else
    }*/
    else {
      val nx = x % (n / 2)
      val ny = y % (n / 2)
      if (x >= n / 2 && y >= n / 2)
        Complex(-1, 0) * WHT(n / 2, nx, ny)
      else
        WHT(n / 2, nx, ny)
    }
    t

  }

  //this is the version that returns a single complex
  def DFT(n: Int, x: Int, y: Int): Complex = {
    val k = 1
    val t_e = E(n)
    new Complex(t_e.re(x * y * k), t_e.im(x * y * k))
  }


}


object E {
  var EMap = Map.empty[Int, E]

  def apply(n: Int): E = {
    val eo = EMap.get(n)
    eo.getOrElse({
      val ne = new E(n)
      EMap = EMap + (n -> ne)
      ne
    })
  }
}

class E(val n: Int) {
  def Gcd[A](x: A, y: A)(implicit integral: Integral[A]): A = {
    val t = scala.math.BigInt(integral.toLong(x))
    val res = t.gcd(scala.math.BigInt(integral.toLong(y)))
    x match {
      case _: Int => res.toInt.asInstanceOf[A]
      case _: Long => res.toLong.asInstanceOf[A]
      case _: Short => res.toShort.asInstanceOf[A]
    }
  }

  def NormalizeRational[A](x: A, y: A)(implicit integral: Integral[A]): (A, A) = {
    val gcd = Gcd(x, y)
    (integral.quot(x, gcd), integral.quot(y, gcd))
  }

  def normalize_2pi_shift(xin: Double, yin: Double): (Double, Double) = {
    var (x, y) = NormalizeRational(Math.round(xin), Math.round(yin))
    if ((x / y) < 0) {
      val t: Long = Math.ceil(x.toDouble / y.toDouble / (-2.0)).toLong
      x = x + 2 * t * y
    } else {
      val t = (Math.floor((x.toDouble - 2 * y.toDouble) / y.toDouble / 2.0) + 1).toLong;
      x = x - 2 * y * t;
    }
    val (xp, yp) = NormalizeRational(x, y)
    (xp.toDouble, yp.toDouble)
  }

  def normalize_pi_over2_shift(xin: Double, yin: Double): (Double, Double) = {
    val (x, y) = (Math.round(xin), Math.round(yin))
    val (xp, yp) = NormalizeRational(2 * x - y, 2 * y)
    (xp.toDouble, yp.toDouble)
  }

  def normalize_pi_over2_reflection(xin: Double, yin: Double): (Double, Double) = {
    val (x, y) = (Math.round(xin), Math.round(yin))
    val (xp, yp) = NormalizeRational(y - 2 * x, 2 * y)
    (xp.toDouble, yp.toDouble)
  }

  def normalize_trig(sign: Int, trig: Boolean, x: Double, y: Double): (Int, Boolean, Double, Double, Double) = {
    // normalization in 2Pi, achieving: 0 <= xn / yn <= 2
    val (xn, yn) = normalize_2pi_shift(x, y)
    if (xn > yn) {
      if (trig) normalize_trig(sign * (-1), trig, xn - yn, yn) else normalize_trig(sign * (-1), false, xn - yn, yn)
    } else if (xn == yn) {
      if (trig) (sign, true, xn, yn, sign * (+0.0)) else (sign, false, xn, yn, sign * (-1.0))
    } else {
      if (xn > yn / 2) {
        // normalization in Pi, achieving 0 <= xn / yn <= 1/2
        val (xp, yp) = normalize_pi_over2_shift(xn, yn)
        if (trig) normalize_trig(sign * (+1), false, xp, yp)else normalize_trig(sign * (-1), true, xp, yp)

      } else if (xn == yn / 2) {
        if (trig) (sign, true, xn, yn, sign * (+1.0)) else
        (sign, false, xn, yn, sign * (+0.0))

      } else {
        // now reflect in Pi / 2, and make sure that 0 <= xn / yn <= 1/4
        if (xn > yn / 4) {
          val (xp, yp) = normalize_pi_over2_reflection(xn, yn)
          if (trig) (sign, false, xp, yp, Double.MaxValue) else (sign, true, xp, yp, Double.MaxValue)

        } else if (xn == yn / 4) {
          (sign, false, 1.0, 4.0, Double.MaxValue)
        } else {
          if (xn == 0.0) {
            if (trig) (sign, true, xn, yn, sign * (+0.0)) else (sign, false, xn, yn, sign * (+1.0))

          } else {
            if (trig) (sign, true, xn, yn, Double.MaxValue) else (sign, false, xn, yn, Double.MaxValue)

          }
        }
      }
    }
  }

  private def valueSinOrCos(f: Boolean, x: Double, y: Double): Double = {
    val (sign, trig, xn, yn, value) = normalize_trig(1, f, x, y)
    if (!value.equals(scala.Double.MaxValue))  value  else {
      if (trig)
        (xn, yn) match {
          case (1.0, 6.0) => sign * 0.5
          case _ => sign * Math.sin(xn * Math.PI / yn)
        }
        else sign * Math.cos(xn * Math.PI / yn)

    }
  }

  def SinPi(x: Double, y: Double): Double = valueSinOrCos(true, x, y)

  def CosPi(x: Double, y: Double): Double = valueSinOrCos(false, x, y)

  private def yieldk(n: Int) :Int = {
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

  lazy val store = yieldk(n)

  def re(p: Int): Double = {
    val x = CosPi(2.0 * p * store, n)
    x
  }

  def im(p: Int): Double = SinPi(2.0 * p * store, n) * -1.0
}


case class Complex(val re: Double, val im: Double) {
  def +(rhs: Complex): Complex = {

    Complex(re + rhs.re, im + rhs.im)
  }

  def -(rhs: Complex): Complex = {

    Complex(re - rhs.re, im - rhs.im)
  }

  def *(rhs: Complex): Complex = Complex(re * rhs.re - im * rhs.im, re * rhs.im + im * rhs.re)

}

class ComplexVector(val save: Array[Complex]) extends AnyVal {
  //class ComplexVector(n: Int) extends AnyVal{
  //val save = new Array[Complex](n)

  def apply(i: Int): Complex = save(i)

  def update(i: Int, y: Complex): ComplexVector = {
    save(i) = y
    this
  }

  def print() = {
    save.map(p => println(p))
  }

}