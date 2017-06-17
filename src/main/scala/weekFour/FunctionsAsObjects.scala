package weekFour

/**
  * Created by lisza on 24.05.17.
  */

/**
  * Just like basic Types, also function types and values
  * are treated as objects
  *
  * e.g. (x: Int) => x*x would expand to
  *
  *   {class AnonFun extends Function1[Int, Int]{
  *     def apply(x:Int) = x*x
  *   }
  *   new AnonFun
  *   }
  *   or shorter using anonymous class syntax
  *
  *   new Function[Int, Int]{
  *     def apply(x: Int)= x*x
  *   }
  *
  *   the call of a function as f(a, b) or f a b respectively
  *   would expand to f.apply(a, b)
  *
  *   => In contrast methods are not objects
  *   otherwise the apply() method of function objects would lead to
  */

/*Task: Define an object List{} with 3 functions to create lists of length 0-2
* created by the calls List(), List(x) and List(x,y)
* */

import weekThree.Polymorphism.{List, Cons, Nil}

object List {
  //List(1,2) would expand to List.apply(1,2)
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))
  //List(2) would expand to List.apply(1)
  def apply[T](x1: T): List[T] = new Cons(x1, new Nil)
  //List() would expand to List.apply()
  def apply[T](): List[T] = new Nil

}
