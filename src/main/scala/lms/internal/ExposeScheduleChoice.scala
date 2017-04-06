package scala.lms
package internal

trait ScheduleChoice {
 self =>
 val cminfo: CodeMotion
 val explored: Vector[Int]

 type MyScheduleChoice2 = ScheduleChoice{
  val cminfo :self.cminfo.type
 }
 val scheduleoptions: Vector[(Int , Unit => MyScheduleChoice2)]

 protected def getNewFront(block: cminfo.BlockInfo3, current_roots: Set[Int], root: Int, done : Set[Int]): Set[Int] = {
  //val onblocks =
  val ir = cminfo.reifiedIR
  //val tp = ir.id2tp(root)
  //val onblocks = ir.IR.blocks(tp)
  if (!block.children.contains(root))
   assert(false, "bla")
  val nexts = block.children(root).successors //get all successors
  val onlyfromblock = nexts.filter(n => block.children.contains(n)) //don't consider successors that are not part of the block
  val onlynew = onlyfromblock.filter(n => !done.contains(n))

  //at this part we check if its predecessors are already done
  //preddone will contain all those sucessors whos predecessors are done/ outside the block or are the current root
  val preddone = onlynew.flatMap(n => {
    val allprev = block.children(n).predecessors //get its predecessors
    val withoutroots = allprev - root -- done
    val onlyfromblock = withoutroots.filter(n => block.children.contains(n))
    if (onlyfromblock.isEmpty) Some(n) else None
  })
  val newroots = current_roots ++ preddone - root

  /*val withoutprev = nexts flatMap (
     next => { //for each successors
      if(!block.children.contains(next))
       assert(false,"bla")
     val allprev = block.children(next).predecessors //get its predecessors
     val withoutroot = allprev - root -- done
      val onlyfromblock = withoutroot.filter(x => block.children.contains(x))
      if (onlyfromblock.isEmpty) Some(next) else None
     }
     )
*/
  //val newroots = (current_roots - root ++ withoutprev).filter(p => block.children.contains(p)) //the filter to make sure we dont schedule the calling block function
  newroots
 }

 def getNewGlobalScheduleChoice(done: Set[Int]): MyScheduleChoice2 = {
  val globals = cminfo.block_cache3.globals
  val globalids: Set[Int] = globals.map( p => p.sym.id).toSet
  val newfront = globalids -- done
  val newfs: Vector[(Int , Unit => MyScheduleChoice2)] = newfront.toVector map (
    node => {
     val f: (Unit => MyScheduleChoice2) = (u: Unit) => {
      val t: MyScheduleChoice2 = getNewGlobalScheduleChoice(done + node)
      t
     }
     (node,f)
    })

  val newtrav: MyScheduleChoice2 = new ScheduleChoice {
   val cminfo: self.cminfo.type = self.cminfo
   val scheduleoptions: Vector[(Int , Unit => MyScheduleChoice2)] = newfs
   val explored: Vector[Int] = Vector()
  }
  newtrav

 }

 def getNewScheduleChoice(block: cminfo.BlockInfo3, current_roots: Set[Int], root: Int, done: Set[Int]): MyScheduleChoice2 = {
  val newfront = getNewFront(block,current_roots,root, done).toVector
  val newfs: Vector[(Int , Unit => MyScheduleChoice2)] = newfront map (
    node => {
     val f: (Unit => MyScheduleChoice2) = (u: Unit) => getNewScheduleChoice(block,newfront.toSet,node, done + root)
     (node,f)
    }
    )

  val newtrav: MyScheduleChoice2 = new ScheduleChoice {
   val cminfo: self.cminfo.type = self.cminfo
   val scheduleoptions: Vector[(Int , Unit => MyScheduleChoice2)] = newfs
   val explored: Vector[Int] = Vector()
  }
  newtrav

 }
}




trait ExposeScheduleChoice {
 self =>
 val cminfo: CodeMotion
 type MyScheduleChoice = ScheduleChoice{
  val cminfo :self.cminfo.type
 }

 //this returns an iterator to traverse all free symbols (e.g. global variables)
 //symbols that are not bound on the arguments of the Block
 def getFreeSymsIterator() =  ???

 //returns a traversal iterator which traverses the DAG in Arguments -> Result direction
 def getForwardIterator(): MyScheduleChoice = {
  val lam = cminfo.reifiedIR.rootlambda


  val t: MyScheduleChoice = new ScheduleChoice {
   val cminfo: self.cminfo.type = self.cminfo
   val scheduleoptions = Vector()
   val explored: Vector[Int] = Vector()
  }
  val newfs: Vector[(Int , Unit => MyScheduleChoice)] = {
   val id = cminfo.reifiedIR.def2tp(lam).sym.id
   val cache = cminfo.block_cache3
   val f: (Unit => MyScheduleChoice) = (u: Unit) => {
    val blockinfo = cache.blockinfo(lam.y)
    //t.getNewScheduleChoice(blockinfo,blockinfo.roots.tail,blockinfo.roots.head, Set.empty)

    //this is under the assumption that at the top level the only choice is the lambda
    val newtrav: MyScheduleChoice = new ScheduleChoice {
     val cminfo: self.cminfo.type = self.cminfo
     val scheduleoptions: Vector[(Int , Unit => MyScheduleChoice2)] = Vector.empty
     val explored: Vector[Int] = Vector()
    }
    newtrav
   }
   Vector((id,f))
  }

  val newfs2: Vector[(Int , Unit => MyScheduleChoice)] = {
   val cache = cminfo.block_cache3
   val globals = cminfo.block_cache3.globals
   val globalids = globals.map( p => p.sym.id)

   val lambdas: Vector[(Int,cminfo.reifiedIR.IR.AbstractLambda[_,_])] = globals.flatMap(p => {
    val t: Vector[(Int,cminfo.reifiedIR.IR.AbstractLambda[_,_])] = p.rhs match {
     case gl@cminfo.reifiedIR.IR.ExternalLambda(f,x,y,hot,args,returns,true, name) => {
      val w: (Int,cminfo.reifiedIR.IR.AbstractLambda[_,_]) = (p.sym.id,gl)
      Vector( w  )
     }
     case _ => Vector.empty
    }
    t
   }
   )
   val globalchoice = lambdas.map(
    lamtup => {
     val id = lamtup._1
     val lam = lamtup._2
     val f: (Unit => MyScheduleChoice) = (u: Unit) => {
      val blockinfo = cache.blockinfo(lam.y)
      //t.getNewScheduleChoice(blockinfo,blockinfo.roots.tail,blockinfo.roots.head, Set.empty)

      //this is under the assumption that at the top level the only choice is the lambda
      val newtrav: MyScheduleChoice = new ScheduleChoice {
       val cminfo: self.cminfo.type = self.cminfo
       val scheduleoptions: Vector[(Int , Unit => MyScheduleChoice2)] = globalids.map(p => (p, (u: Unit) => getNewGlobalScheduleChoice(Set(p,id))))
       val explored: Vector[Int] = Vector()
      }
      newtrav
     }
     (id,f)
    }
   )

   val id = cminfo.reifiedIR.def2tp(lam).sym.id

   val f: (Unit => MyScheduleChoice) = (u: Unit) => {
    val blockinfo = cache.blockinfo(lam.y)
    //t.getNewScheduleChoice(blockinfo,blockinfo.roots.tail,blockinfo.roots.head, Set.empty)

    //this is under the assumption that at the top level the only choice is the lambda
    val newtrav: MyScheduleChoice = new ScheduleChoice {
     val cminfo: self.cminfo.type = self.cminfo
     val scheduleoptions: Vector[(Int , Unit => MyScheduleChoice2)] = globalids.map(p => (p, (u: Unit) => getNewGlobalScheduleChoice(Set(p,id))))
     val explored: Vector[Int] = Vector()
    }
    newtrav
   }


   Vector((id,f) ) ++ globalchoice
  }


  val newtrav: MyScheduleChoice = new ScheduleChoice {
   val cminfo: self.cminfo.type = self.cminfo
   val scheduleoptions: Vector[(Int , Unit => MyScheduleChoice)]  = newfs2
   val explored: Vector[Int] = Vector()
  }
  newtrav
 }


 //returns a traversal iterator which traverses the DAG in Arguments -> Result direction
 def getForwardIterator(block: cminfo.reifiedIR.IR.Block): MyScheduleChoice = {
  val t: MyScheduleChoice = new ScheduleChoice {
   val cminfo: self.cminfo.type = self.cminfo
   val scheduleoptions = Vector()
   val explored: Vector[Int] = Vector()
  }

  val binfo = cminfo.block_cache3.blockinfo(block)
  val roots =  binfo.roots

  val newfs: Vector[(Int , Unit => MyScheduleChoice)] = roots.toVector map (
    node => {
     val block = binfo //t.cminfo.block_cache.getHead()
     val f: (Unit => MyScheduleChoice) = (u: Unit) => t.getNewScheduleChoice(block,block.roots,node, Set.empty)
     (node,f)
    }
    )
  /*
      val newfs: Vector[(Int , Unit => ScheduleChoice)] = roots.toVector map (
        node => {
          val block = t.cminfo.block_cache.getHead()
          val f: (Unit => ScheduleChoice) = (u: Unit) => t.getNewScheduleChoice(block,block.roots,node)
          (node,f)
        }
        )
  */
  val newtrav = new ScheduleChoice {
   val cminfo: self.cminfo.type = self.cminfo
   val scheduleoptions = newfs
   val explored: Vector[Int] = Vector()
  }
  newtrav
 }

 //returns a traversal iterator which traverses the DAG in Results -> Arguments direction
 def getBackwardIterator() = ???
}





object ExposeScheduleChoice {
 //def apply(cm : CodeMotion): ExposeScheduleChoice{ val cminfo: CodeMotion { val reifiedIR: ReificationPure{ val IR: cm.reifiedIR.IR.type }}  = {
 def apply(cm : CodeMotion): ExposeScheduleChoice{ val cminfo: cm.type}  = {
  val traversal = new ExposeScheduleChoice{
   override val cminfo: cm.type = cm
  }
  traversal
 }
}
