package SpiralSThesisGui
import SpiralSThesis._
/**
  * Created by rayda on 03-Feb-17.
  */
object DFT_dyn_hand extends App{

  def resolveTwid(fbn: Int, fbd: Int, fbk: Int, i: Int): Complex = ???
  def chooseRadix(n: Int): Int = 2
  def fuseIM(r: IMH, s: IMH, lv: Int): IMH = IMH((r.base + (r.s0 * s.base)) + r.s1 * lv, r.s0 * s.s0, (0 + (r.s0 * s.s1)))

  case class IMH(base: Int, s0: Int, s1: Int)

  def DFT(x: Array[Complex], y: Array[Complex], n: Int, lb: Int, g: IMH, s: IMH, tw: IMH, twiddle: Option[(Int,Int,Int)]): Array[Complex] = {
    if (n == 2) F2(x,y,lb, g, s, tw, twiddle)
    else DFT_CT(x,y,n,lb, g, s, tw, twiddle)
  }

  def DFT_CT(x: Array[Complex], y: Array[Complex], n: Int, lb: Int, g: IMH, s: IMH, tw: IMH,  twiddle: Option[(Int,Int,Int)]): Array[Complex] = {
    val k = chooseRadix(n)
    val m = n/k
    for (j <- 0 until lb){
      val (s0,s1) = (k,1)
      val inner = IMH(0,s0,s1)
      val s1_gather: IMH = fuseIM(g,inner,j)
      val s1_scatter: IMH = IMH(0,1,m)
      val s1_tw: IMH = fuseIM(tw, inner, j)

      val buffer = DFT(x,new Array[Complex](n),m,k,s1_gather,s1_scatter,tw,None)

      val twiddles = (n,m,1)
      val s2_gather: IMH = IMH(0,m,1)
      val s2_scatter: IMH = fuseIM(s,s2_gather,j)
      DFT(buffer,y,k,m,s2_gather,s2_scatter,s2_gather,Some(twiddles))
    }
    y
  }
  def F2(x: Array[Complex], y: Array[Complex], lb: Int, g: IMH, s: IMH, tw: IMH,
         twiddle: Option[(Int,Int,Int)]): Array[Complex] = {
    for (j <- 0 until lb){
      val t00 = x(g.base + (g.s0 * 0) + (g.s1 * j))
      val t01 = x(g.base + (g.s0 * 1) + (g.s1 * j))
      val (t0,t1) = twiddle.fold((t00,t01))( twt => {
        val (fbn,fbd,fbk) = twt
        ((resolveTwid(fbn, fbd, fbk, (tw.base + (tw.s0 * 0) + (tw.s1 * j))) * t00),
        (resolveTwid(fbn, fbd, fbk, (tw.base + (tw.s0 * 0) + (tw.s1 * j))) * t01))})
      y(s.base + (s.s0 * 0) + (s.s1 * j)) = (t0 + t1)
      y(s.base + (s.s0 * 1) + (s.s1 * j)) = (t0 - t1)
    }
    y
  }

}
