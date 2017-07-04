package SpiralSThesisGui

/**
  * Created by rayda on 29-Dec-16.
  */



case class Node(psize: Int, pparent: FunctionAttributes,
                pleft: FunctionAttributes, pright: FunctionAttributes) extends FunctionAttributes with isParent with isChild {
  var size = psize
  var parent = pparent
  var left = pleft
  var right = pright
           }

case class Root(psize: Int, pleft: FunctionAttributes, pright: FunctionAttributes) extends FunctionAttributes with isParent{
  var size = psize
  var left = pleft
  var right = pright
}

case class Leaf(psize: Int, pparent: FunctionAttributes) extends FunctionAttributes with isChild{
  var size = psize
  var parent = pparent
}

trait isChild{
  var parent: FunctionAttributes
}

trait isParent{
  var left: FunctionAttributes
  var right: FunctionAttributes
}

abstract class FunctionAttributes(){
  var size: Int
}

object EnumVaraiants extends App{

  def DivisorPairs(n: Int): List[(Int,Int)] =  {    (2 to Math.sqrt(n).toInt ).filter(n%_== 0).flatMap(x=>(if(n/x == x) List(x) else List(n/x,x)) ).toList.sortWith(_>_).map(x=> (n/x,x))  }

  //val root = new Root(32, )
  println(DivisorPairs(32))


  def genLeaf(parent: FunctionAttributes with isParent, mysize: Int): Leaf = Leaf(mysize,parent)

  def genNodeStream(parent: FunctionAttributes with isParent, mysize: Int): Stream[(FunctionAttributes with isChild)] = {
    if (mysize == 2) Stream[(FunctionAttributes with isChild)](genLeaf(parent,mysize)) else{
      val childstream = genNodesStream(mysize)
      val nstream = childstream.map( c => {
        val newparent = new Node(mysize,parent,null,null)
        c._1.parent = newparent
        c._2.parent = newparent
        newparent.left = c._1
        newparent.right = c._2
        newparent
      })
      nstream
    }
  }

  def genNodesStream(parentsize: Int): Stream[(FunctionAttributes with isChild, FunctionAttributes with isChild)] = {
    val n = parentsize
    val possible_decompositions = DivisorPairs(n)
    possible_decompositions.map(p => {
      val left_size = p._1
      val right_size = p._2

    })
    ???
  }

  //new Root(32, le)


}
