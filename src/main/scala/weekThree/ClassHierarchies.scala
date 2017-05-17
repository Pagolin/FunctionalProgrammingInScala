package weekThree

/**
  * Created by lisza on 16.05.17.
  */
object ClassHierarchies {


  /** Rules for class hierarchies, inheritance and polymorphism are
    * (for some obscure reason :-D ) quite similar to those in java.
    * E.g. abstract classes don't need to implement their methods and can't
    * be instantiated.
    * Scala is also a single inheritance language. Instead of javas interfaces,
    * TRAITS are used to realize sort of multiple inheritance, e.g. :
    *
    * trait exampleTrait{
    *   def someAttribute: Int
    *   def anotherAttribute: Int
    *   def firstFunction: Int
    *   def secondFunction: (Int, Int)=>Boolean
    *   def thirdFunction(x:Int, y:Int): Int = someAttribute * anotherAttribute
    * }
    *
    * Unlike interfaces TRAITS may contain also fields and implemented/concrete
    * methods. Unlike classes they can not have value parameters
    */

  abstract class IntSet {
    def contains(x: Int): Boolean
    def incl(x: Int): IntSet
    def union(other : IntSet) : IntSet
  }

  /** Example implementation as a sorted  binary tree
    * Note: In the incl() method new elements are added to a structure by
    * creating new structures containing them.
    * The result is, that such trees are an example of PERSISTENT DATASTRUCTURES,
    * meaning that even after changes the old structure persists, which is crucial to
    * the parallel computation aspects of functional programming
    */

  // Instead of class Empty1 extends IntSet, creating an object leads
  // to Empty being a singleton, so every time we create a new NonEmpty, we
  // let it's "open ends" point to the same Empty object

  object Empty1 extends IntSet{
    override def contains(x: Int): Boolean = false

    override def incl(x: Int): IntSet = new NonEmpty(x, Empty1, Empty1)

    override def toString: String = "."

    override def union(other: IntSet): IntSet = other
  }

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    /**
      * As the TreeSet is implemented sorted i.e. the left sub-element is
      * always smaller the right one always bigger than the actual node
      * contains() is implemented as recursive binary search
      */
    override def contains(x: Int): Boolean =
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true

    override def incl(x: Int): IntSet =
      if (x < elem) new NonEmpty(elem, left incl x, right)
      else if (x > elem) new NonEmpty(elem, left, right incl x)
      else this

    override def union(other: IntSet): IntSet =
      ((left union right) union other) incl elem

    override def toString: String = "{" + left + elem + right + "}"
  }
// Exception and error handling 's also quit similar to java...
  class troubleShooting{
    def specialError(msg: String) = throw new Error(msg)
    def throwsException() = throw new IllegalArgumentException("Just because I can")
    def tryCatchBlock(): Unit = {
      try {
        throwsException()
        println("this line is never executed")
      } catch {
        case e: Exception => println("exception caught: " + e)
      }
    }


  }

  def main(args: Array[String]) {
    val t1 = new NonEmpty(5, new NonEmpty(7, Empty1,  Empty1),new NonEmpty(3, Empty1,  Empty1))
    val t2 = t1 incl 4
    println(t1 + "\n" + t2 + "\n union" + {t1 union t2} )

  }
}