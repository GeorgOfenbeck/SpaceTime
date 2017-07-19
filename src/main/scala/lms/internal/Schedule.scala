package scala.lms
package internal

trait Schedule {
 self =>
 //lazy val col = new ScheduleIterable
 val IR: BaseExp
 def choose(tps: Vector[IR.TP[_]]): IR.TP[_] = {
  if (tps.isEmpty){
   assert(false, "this should just not happen")
  }
  val h = tps.dropWhile(p => { //RF! This is a nasty workaround to make sure block dependencies are done before unparsing
    !IR.blocks(p).isEmpty
  })
  if( h.isEmpty)
   tps.head
  else h.head
 }

 class ScheduleIterable(val esc : ExposeScheduleChoice{
  val cminfo : CodeMotion {
   val reifiedIR: ReificationPure {
    val IR: self.IR.type }}}) extends Iterable[self.IR.TP[_]] {

  class ScheduleIterator(val state: esc.MyScheduleChoice) extends Iterator[self.IR.TP[_]] {
   override def hasNext(): Boolean = {
    val t = getTrav()
    !t.scheduleoptions.isEmpty
   }

   override def next(): self.IR.TP[_] = {
    val t = getTrav()    
    val tps = t.scheduleoptions.map(p => t.cminfo.reifiedIR.id2tp(p._1))  
    val choice = choose(tps)
    val choice_index = tps.indexOf(choice)
    t.scheduleoptions(choice_index)
    val xx = t.scheduleoptions(choice_index)
    val yy = xx._2
    val u: Unit = null
    val zz = yy.apply(u)
    setTrav(zz)
    choice
   }

   private var typemadness: esc.MyScheduleChoice = state
   //had issues with doing this without a var / therefore the strange name
   private def getTrav(): esc.MyScheduleChoice = typemadness
   private def setTrav(t: esc.MyScheduleChoice): Unit = {
    typemadness = t
   }
  }

  def iterator(): Iterator[self.IR.TP[_]] = new ScheduleIterator(esc.getForwardIterator())
  def iterator(block: self.IR.Block): Iterator[self.IR.TP[_]] =
   //for dotty - no idea why it doesnt like it
   new ScheduleIterator(esc.getForwardIterator(block.asInstanceOf[esc.cminfo.reifiedIR.IR.Block]))
 }



 def getSchedulewithIterator(esc : ExposeScheduleChoice{
  val cminfo : CodeMotion {
   val reifiedIR: ReificationPure {
    val IR: self.IR.type }}}): ScheduleIterable = new ScheduleIterable(esc)
}


object DefaultSchedule{
 def apply(pIR: BaseExp) = {
  new Schedule {
   override val IR: pIR.type = pIR
  }
 }
}











