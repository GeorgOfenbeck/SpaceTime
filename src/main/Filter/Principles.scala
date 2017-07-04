package Filter

import scala.collection.mutable

/**
  * Created by rayda on 01-Nov-16.
  */
object Principles extends App{

  val t1 = Vector.empty[Int]
  val t2 = Vector(1)
  val t3 = Vector(1,2)

  val r1 = if(t1.isEmpty) None else Some(t1.reduceLeft(_ + _))
  t2.reduceLeft(_ + _)
  t3.reduceLeft(_ + _)
  val dres = symbench[Int,Long,Float,Float,Int,Int,Double,Double,Double](1,4,3.1f,3,1,3,3.16,2.2,2.2,Map.empty)
  println(dres)
  blocking(128,128,8,8)

  def blocking(x: Int, y: Int, blockingx: Int, blockingy: Int) = {

    def init (nRows: Int, nCols: Int) = Array.tabulate(nRows,nCols)( (x,y) => false )

    var a: Array[Array[Boolean]] = init(x,y)

    val unrollx = 2
    val unrolly = 2

    val iterations_x = x / blockingx //dyn
    val iterations_y = y / blockingy //dyn

    val rest_x = x % blockingx
    val rest_y = y % blockingy

    val itblockedx = blockingx / unrollx
    val itblockedy = blockingy / unrolly

    //main block


    for (i <- 0 until iterations_x)
      for (j <- 0 until iterations_y)
          for (ii <- 0 until itblockedx)
            for (jj <- 0 until itblockedy) {
              val f = (i * blockingx + ii * unrollx)
              //val t = (i * blockingx) + blockingx + (ii * unrollx) + unrollx
              val t = (i * blockingx) + (ii * unrollx) + unrollx
              for (iii <- f until t) {
                val f1 = (j * blockingy + jj * unrolly)
                //val t1 = (j * blockingy) + blockingy + (jj * unrolly) + unrolly
                val t1 = (j * blockingy)  + (jj * unrolly) + unrolly
                for (jjj <- f1 until t1)
                  a(iii)(jjj) = true
              }
            }
          /*for (ii <- (i*blockingx) until (i*blockingx)+blockingx)
            for (jj <- (j*blockingy) until (j*blockingy)+blockingy)
              {


                a(ii)(jj) = true
              }*/

    //bottom border
    for (r <- iterations_x*blockingx until x)
      for (c <- 0 until iterations_y)
        for (cb <- (c * blockingy) until (c* blockingy)+blockingy)
          {
            a(r)(cb) = true
          }


    //right border
    for (r <- 0 until iterations_x)
      for (rb <- (r*blockingx) until (r*blockingx)+blockingx)
        for (c <- (iterations_y*blockingy) until y)
          a(rb)(c) = true



       //bottom right border

       for (i <- (iterations_x * blockingx) until x)
         for (j <- (iterations_y * blockingy) until y)
           a(i)(j) = true



    val all = a.foldLeft(true){
      (acc,ele) => ele.foldLeft(acc){
        (acc2,ele2) => acc2 && ele2
      }
    }
    println(all)
  }


  case class Bla(x: Int)


  def prep(a: Int, b: Int, c: Int, d: Int) = {

    var map = scala.collection.immutable.HashMap.empty[Int, Bla]

    map = map + (0 -> Bla(a))
    map = map + (1 -> Bla(b))
    map = map + (2 -> Bla(c))
    map = map + (3 -> Bla(d))


    val reverse = map.foldLeft(scala.collection.immutable.HashMap.empty[Bla, Vector[Int]]){
      (acc,ele) => {
        if (acc.contains(ele._2)) {
          acc + (ele._2 -> (acc(ele._2) ++ Vector(ele._1)))
        } else
          acc + (ele._2 -> Vector(ele._1))
      }
    }

  }


  def symetry(a: Int, b: Int, c: Int, d: Int, pv: Map[Int,Int], vp: Map[Int,Vector[Int]]) = {

    val input = Vector(1,2,3,4)

    def take(i: Int): Int = i match{
      case 0 => a
      case 1 => b
      case 2 => c
      case 3 => d
      case _ => ???
    }
    vp.foldLeft(0){
      (acc,ele) => acc + take(ele._1) * (ele._2.foldLeft(0){
        (acc2,ele2) => acc2 + input(ele2)
      })
    }
  }


  def symbench[A, B, C, D, E, F, G, H, I](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, dup: Map[Int, Vector[Int]])(implicit eva: Numeric[A], evb: Numeric[B], evc: Numeric[C], evd: Numeric[D], eve: Numeric[E], evf: Numeric[F], evg: Numeric[G], evh: Numeric[H], evi: Numeric[I]): Double = {

    def get(idx: Int): Any = {
      idx match {
        case 1 => a
        case 2 => b
        case 3 => c
        case 4 => d
        case 5 => e
        case 6 => f
        case 7 => g
        case 8 => h
        case 9 => i
        case _ => ???
      }
    }

    val tmap: Map[Int,Numeric[_]] = Map(1 -> eva, 2 -> evb, 3 -> evc, 4 -> evd, 5 -> eve, 6 -> evf, 7 -> evg, 8 -> evh, 9 -> evi)

    val tintmap: Vector[Int] = tmap.foldLeft(Vector.empty[Int]) {
      (acc, ele) => if (ele._2 == implicitly[Numeric[Int]]) acc :+ ele._1 else acc
    }
    val tintvec = tintmap.map(p => get(p).asInstanceOf[Int])

    val longmap: Vector[Int] = tmap.foldLeft(Vector.empty[Int]) {
      (acc, ele) => if (ele._2 == implicitly[Numeric[Long]]) acc :+ ele._1 else acc
    }
    val tlongvec = longmap.map(p => get(p).asInstanceOf[Long])
    val floatmap: Vector[Int] = tmap.foldLeft(Vector.empty[Int]) {
      (acc, ele) => if (ele._2 == implicitly[Numeric[Float]]) acc :+ ele._1 else acc
    }
    val tfloatvec = floatmap.map(p => get(p).asInstanceOf[Float])
    val doublemap: Vector[Int] = tmap.foldLeft(Vector.empty[Int]) {
      (acc, ele) => if (ele._2 == implicitly[Numeric[Double]]) acc :+ ele._1 else acc
    }
    val tdoublevec = doublemap.map(p => {
      val ug: Any = get(p)
      ug.asInstanceOf[Double]
    })





    val rtmap = tmap.foldLeft(Map.empty[Numeric[_],Vector[Int]]){
      (acc,ele) => if (acc.contains(ele._2)){
        val app = acc(ele._2) :+ ele._1
        acc + (ele._2 -> app)
      }
        else
        acc + (ele._2 -> Vector(ele._1))
    }

    val rint: Option[Int] = if (tintmap.isEmpty) None else Some(tintmap.reduceLeft(_ + _))
    val rlong: Option[Long] = if (tlongvec.isEmpty) None else Some(tlongvec.reduceLeft(_ + _))
    val rfloat = if(tfloatvec.isEmpty) None else Some(tfloatvec.reduceLeft(_ + _))
    val rdouble = if(tdoublevec.isEmpty) None else Some(tdoublevec.reduceLeft(_ + _))


    rint
    
    val dres:Double = (rint,rlong,rfloat,rdouble) match {
      case (Some(xi), Some(xl), Some(xf), Some(xd)) => xi * xl * xf * xd
      case (None, Some(xl), Some(xf), Some(xd)) =>  xl * xf * xd
      case (Some(xi), None, Some(xf), Some(xd)) => xi * xf * xd
      case (None, None, Some(xf), Some(xd)) => xf * xd
      case (Some(xi), Some(xl), None, Some(xd)) => xi * xl  * xd
      case (None, Some(xl), None, Some(xd)) =>  xl * xd
      case (Some(xi), None, None, Some(xd)) => xi * xd
      case (None, None, None, Some(xd)) => xd
      case (Some(xi), Some(xl), Some(xf), None) => xi * xl * xf 
      case (None, Some(xl), Some(xf), None) => xl * xf
      case (Some(xi), None, Some(xf), None) => xi * xf
      case (None, None, Some(xf), None) => xf
      case (Some(xi), Some(xl), None, None) => xi * xl 
      case (None, Some(xl), None, None) => xl
      case (Some(xi), None, None, None) => xi
      case (None, None, None, None) => ???
    }

    dres

  }




}
