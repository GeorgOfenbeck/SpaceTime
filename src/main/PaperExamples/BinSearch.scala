package PaperExamples

/**
  * Created by rayda on 15-Nov-16.
  */
object BinSearch extends App{

  for (i <- 0 until 64)
    println(binsearch(i,0,66) == i)


  def binsearch(check: Int, low: Int, high: Int): Int = {
    val mid = low + (high - low)/2
    if( (high - low) <= 1 )

      if (check == low) {
        //println(" in low "+low + " " + high)
        low
      } else {
        //println(" in high "+high + " " + high)
        high
      }
    else {
      if(check <= mid)
        binsearch(check, low, mid) else

        binsearch(check, mid, high)

    }
  }

}
