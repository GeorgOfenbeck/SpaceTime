package scala.lms.util






object TimeLog {


 
 case class Timing(id: Int, description: String){
  var timers = Map.empty[String,Vector[(Long,Long)]]
 }
 private var timingnr = 0
 private var cur_timer: Timing = null


 def setupTimer(description: String) = {
  timingnr = timingnr + 1
  cur_timer = Timing(timingnr: Int, description: String)
 }

 def saveTiming(path: String) = {
  assert(cur_timer != null, "tried save timing without prior setup")
  val file = new java.io.FileOutputStream(path + "timing" + timingnr)
  val stream = new java.io.PrintWriter(file)
  stream.println(cur_timer.description)
  cur_timer.timers.toVector.sortBy(ele => ele._1).foreach({
   ele => {
    stream.print(ele._1)
    ele._2.foreach({
     ele2 => {
      val diff = ele2._2 - ele2._1
      stream.print(" , " + diff )
     }
    })
   }
  })
  stream.println("")
  stream.flush()
  stream.close()
  file.flush()
  file.close()
 }

 //
 // Usage: call once to start the timer, and once to stop it, using the same timer name parameter
 //
 def timer(timerName: String, start: Boolean) = {
  //assert(cur_timer != null, "tried to do timing without prior setup")
  if (cur_timer != null){
  if (cur_timer.timers contains timerName) {
   val ele = cur_timer.timers(timerName)
   if (start){
    assert((0).toLong != ele.last._2, "Seems the last timing was never stopped")
    val append = ele :+ (System.nanoTime(),(0).toLong)
    cur_timer.timers = cur_timer.timers + (timerName -> append)
   } else {
    assert((0).toLong == ele.last._2, "Seems the last timing was never started")
    val withoutlast = ele.dropRight(1)
    val last = ele.last
    val append = withoutlast :+ (last._1,System.nanoTime())
    cur_timer.timers = cur_timer.timers + (timerName -> append)
   }
  }
  else {
   assert(start,s"Seems the timer $timerName was stopped without being started")
   val time = System.nanoTime()
   cur_timer.timers = cur_timer.timers + (timerName -> Vector((System.nanoTime(),(0).toLong)))
  }
 }
 }
}