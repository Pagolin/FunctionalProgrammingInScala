package weekFour

/**
  * Created by lisza on 22.05.17.
  */
object ObjectsEverywhere {
  /**
    * Just as with wrapper classes Integer, String aso in java,
    * everything in scala is basically an object
    * Following are two simplified example implementations of Boolean and
    * Natural Numbers as classes
    */
/*
In the two valued logic every other function can
 be derived from ifThenElse


package idealized.scala

 abstract class Boolean{

 def inThenElse[T](t: => T, e: => T): T

 def &&(x: =>Boolean):Boolean       = ifThenElse(x, false)
 //if the boolean object itself is true, evaluate x otherwise false
 def ||(x: => Boolean):Boolean      = ifThenElse(true, x)
 def unary_!(x: => Boolean):Boolean = ifThenElse(false, true)
 def ==(x: => Boolean):Boolean      = ifThenElse(x, x.unary_!)
 def !=(x: => Boolean):Boolean      = ifThenElse(x.unary_!, x)
 }

 object true extends boolean{
  def inThenElse[T](t: => T, e: => T): T = t
 }

 object false extends boolean{
  def inThenElse[T](t: => T, e: => T): T = e
 }
 */
//The following is an example implementation of natural numbers as Peano NUmber
  //i.e. Numbers defined as a set of its predecessors starting with an empty set for zero
  abstract class Nat {

    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat = new Succ(this)
    def +(that: Nat):Nat
    def -(that: Nat): Nat
  }

  object Zero extends Nat{
    override def isZero: Boolean = true
    override def predecessor: Nat = throw new Error("0.predecessor")
    override def +(that: Nat): Nat = that
    override def -(that: Nat): Nat = if(that.isZero) this
      else throw new Error("0 - x, Class Nat allows no negative values")

  }

  class Succ(n:Nat) extends Nat{
    override def isZero = false
    override def +(that: Nat): Nat = new Succ(n + that)
    override def -(that: Nat): Nat = if(that.isZero) this
    else this.predecessor - that.predecessor
    override def predecessor = n
  }
}
