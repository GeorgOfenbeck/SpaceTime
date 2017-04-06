package scala.lms
package internal


trait Blocks extends Expressions with EffectSummary{

  class Block(val res: Vector[Exp[_]], val summary: Option[Summary], val effects: Vector[Exp[_]])

  var block2tps: Map[Block, Vector[TP[_]]] = Map.empty
  private var blocktpbuffer: Vector[Vector[TP[_]]] = Vector.empty

  object Block {
    def apply(dres: => Vector[Exp[_]]): Block = {
      addBlockTPBuffer()
      val block = new Block(dres, None, Vector.empty)
      block2tps = block2tps + (block -> getBlockTPBuffer())
      block
    }
    def apply(dres: => Vector[Exp[_]],summary: Summary, effects: Vector[Exp[_]] ): Block = {
      addBlockTPBuffer()
      val block = new Block(dres, Some(summary), effects)
      block2tps = block2tps + (block -> getBlockTPBuffer())
      block
    }
  }


  /*case class Block(val res: Vector[Exp[_]])  {
   require(res.isEmpty == false)
  }*/

  protected def addBlockTPBuffer() = {
    blocktpbuffer = blocktpbuffer :+ Vector.empty
  }

  protected def getBlockTPBuffer(): Vector[TP[_]] = {
    val last = blocktpbuffer.last
    blocktpbuffer = blocktpbuffer.dropRight(1)
    last
  }

  override def storeTP(tp: TP[_]): Unit = {
    if (!blocktpbuffer.isEmpty) {
      val rest = blocktpbuffer.dropRight(1)
      val newentry = blocktpbuffer.last :+ tp
      blocktpbuffer = rest :+ newentry
    }
    super.storeTP(tp)
  }

  def blocks(e: Any): Vector[Block] = e match {
    case b: Block => Vector(b)
    case p: Product => p.productIterator.toVector.flatMap(blocks(_))
    case _ => Vector.empty
  }

  /*def blocks(e: Any): Vector[Block[Any]] = e match {
    case b: Block[Any] => Vector(b)
    case p: Product => p.productIterator.toVector.flatMap(blocks(_))
    case _ => Vector()
  }

  def getBlockResult[A](s: Block[A]): Exp[A] = getBlockResultFull(s) // = s.res
  def getBlockResultFull[A](s: Block[A]): Exp[A] = s.res //RF why comment in original?*/
}

