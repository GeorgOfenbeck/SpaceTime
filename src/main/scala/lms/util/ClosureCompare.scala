package scala.virtualization.lms
package util

import java.io._


trait ClosureCompare extends Externalizable {

 // the whole thing must be serializable, since embedded closures
 // might refer to the context.

 def writeExternal(out: ObjectOutput) {
  //    println("in write object")
 }
 def readExternal(in: ObjectInput) {
  throw new NotSerializableException("this is just a mock-up!")
 }

 def canonicalize[A,R](f: Function[A,R]) = {
   //println(f.pickle.value)
  //println(f.toJson.prettyPrint)
  val s = new java.io.ByteArrayOutputStream()
  val o = new java.io.ObjectOutputStream(s)
  o.writeObject(f)
  s.toString("ASCII")
 }

 def sameFunction(f: Function[_,_], g: Function[_,_]): Boolean = {

  val s1 = canonicalize(f)
  val s2 = canonicalize(g)

  println(s1)
  println("---")
  println(s2)
  println("diff: "+ s1.diff(s2))

  def ser(f: Function[_,_]) = f.isInstanceOf[java.io.Serializable]

  if (f.getClass != g.getClass)
   return false

  if (ser(f) && ser(g)) {
   canonicalize(f) == canonicalize(g)
  } else {
   println("serizalizable(f): " + f.getClass + ": " + ser(f))
   println("serizalizable(g): " + g.getClass + ": " + ser(g))
   false
  }
 }

}
