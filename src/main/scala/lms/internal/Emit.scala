package scala.lms
package internal

import java.io.PrintWriter


trait Emit[C] extends Logging{
  self =>
  val IR: BaseExp with FunctionsExp


  type specCM = CodeMotion {
    val reifiedIR: ReificationPure {
      val IR: self.IR.type
    }}

  type specEsc = ExposeScheduleChoice {val cminfo: specCM}





  def emitNode(tp: self.IR.TP[_], acc: C,
              block_callback: (self.IR.Block,C) => C): C = {
  val ret: C = tp match{
   case _ => {
    assert(false, "no translation for " + tp)
    ???
   }
  }
  ret
 }


  def emitc(start: C, it: Iterator[self.IR.TP[_]], block_callback: (self.IR.Block,C) => C ): C = {
    it.foldLeft(start){
      (acc,ele) => {
        val t: C = emitNode(ele,acc,block_callback)
        t
      }
    }
  }




  def emit[A,R]( start: C, f: Function1[A,R])(implicit args: IR.ExposeRep[A], returns: IR.ExposeRep[R]):
  (C, specEsc)
     = {
    val schedule = new Schedule {
      override val IR: self.IR.type = self.IR
    }
    emit(schedule,start,f)(args,returns)
  }

  def emit[A,R]( schedule: Schedule{ val IR: self.IR.type},
                 start: C,
                 f: Function1[A,R])
               (implicit args: IR.ExposeRep[A], returns: IR.ExposeRep[R]): (C,specEsc)  = {
    val reify = new ReifyPure {
      override val IR: self.IR.type = self.IR
    }
    printlog("starting reify")
    val reification = reify.reifyProgram(f)(args, returns)
    printlog("starting codemotion")
    val cm: specCM = CodeMotion(reification)
    printlog("starting schedule")
    val exposedScheduleChoice: specEsc = ExposeScheduleChoice(cm)
    printlog("starting getting Iterator")
    val iteratable = schedule.getSchedulewithIterator(exposedScheduleChoice)
    def blockcallback (block: self.IR.Block, bstart: C): C = {
      val bit = iteratable.iterator(block)
      emitc(bstart,bit,blockcallback)
    }
    printlog("starting iterating")
    val acc = emitc(start,iteratable.iterator,blockcallback)
    printlog("finished iterating")
    (acc,exposedScheduleChoice)
  }





}


