package weekThree

import java.util.NoSuchElementException

/**
  * Created by lisza on 17.05.17.
  */
object Polymorphism {

  /**
    * A fundamental structure in functional programming is the
    * immutable Linked-List. It's constructed from two types of building blocks,
    * Nil and Cons (similar to binary trees), where Cons holds a reference
    * to the first element and the rest of the list
    *
    * The following hierarchy represents such lists of parametrized type T
    *  trait List[T]...
    *  class Cons[T](val head: T, val tail: List[T]) extends List[T]...
    *  class Nil[T] extends List[T]....
    *
    * Note:
    * Cons[T](val head: T, val tail: List[T])
    * --> defines parameters and field of the class at once
    * --> could be rewritten as
    * Cons[T]( _head: T, _tail:List[T]) extends List[T] {
    *   val head = _head
    *   val tail = _tail
    * }
    *
    * There are two principles of polymorphism:
    * -> subtyping: instances of subclass (e.g Nil, Cons) can be passed to a base class
    * -> generics: instances of a function or class are created with parametrized type
    */


trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T]{
  // head and tail are already implemented/ evaluated in the class header
  override def isEmpty = false
}

class Nil[T] extends List[T]{
  override def isEmpty : Boolean = true
  def head: Nothing= throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

//Create a List of objects[T], with one element
  def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

  singleton[Int](1)
  singleton(true) // is evaluated to singleton[Boolean](true) as type is inferred

  def getElement[T](index: Int, xs: List[T]): T =
    if(xs.isEmpty) throw new IndexOutOfBoundsException()
    else if(index==0) xs.head
    else getElement(index-1, xs.tail)

}