/**
  * Created by rayda on 16-Feb-17.
  */
object RunExampleTest extends App{

  def recurse(a: Array[Double], s: Double): Array[Double] =
    if (a.length < 2) a else {
      val scaled = scale(a,s)
      val sumx    = sum(scaled)
      val (l, r) = split(sumx)
      recurse(l, s) ++ recurse(r, s)
    }
  def split(a: Array[Double]) = a.splitAt(a.length / 2)

  def scale(a: Array[Double], s: Double) = a.zipWithIndex.map(
    p =>  {
      val (ele, idx) = p
      ele * Math.sin((idx + a.length) % 10) * s })

  def sum(a: Array[Double]) =
    (0 until a.length - 1).foldLeft(Array.empty[Double]) {
      (acc, i) => acc :+ (a(i) + a(i + 1))
    }

  (0 until 10).foldLeft(Array.empty[Double])
    {
      (acc,ele) => {
        val n: Array[Double] = acc :+ ele.toDouble
        val res: Array[Double] = recurse(n,2.0)
        println(runtime.ScalaRunTime.stringOf(res))
        n
      }
    }


}
