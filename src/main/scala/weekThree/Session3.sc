import weekThree.ClassHierarchies.{Empty1, IntSet, NonEmpty}
import weekThree.Polymorphism._

object session{
  val t1 = new NonEmpty(5, new NonEmpty(7, Empty1,  Empty1),new NonEmpty(3, Empty1,  Empty1))
  val t2 = new NonEmpty(1, new NonEmpty(2, Empty1,  Empty1),new NonEmpty(4, Empty1,  Empty1))
  val uni: IntSet = t1 union t2
  def specialError(msg: String) = throw new Error(msg)
  val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
  getElement(2, list)
}

