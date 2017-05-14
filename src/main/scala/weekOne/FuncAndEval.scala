/**
  * Created by lisza on 14.05.17.
  */
package weekOne

object FuncAndEval {

  /**
    * The following  methods are a first example and overall (in sqrtNewton())
    * cmpute the square roots estimation based on Newtons method    *
    * @param x Double for witch sqrt shall be calculated
    * @return guess approximation of sqrt of x
    */
  def abs(x: Double) = if(x < 0 ) -x else x

  def sqrtNewton(x: Double) = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }
  /**
    * This method returns the largest element in a list of integers. If the
    * list `xs` is empty it throws a `java.util.NoSuchElementException`.
    *
    * You can use the same methods of the class `List` as mentioned above.
    *
    * ''Hint:'' Again, think of a recursive solution instead of using looping
    * constructs. You might need to define an auxiliary method.
    *
    * @param xs A list of natural numbers
    * @return The largest element in `xs`
    * @throws java.util.NoSuchElementException if `xs` is an empty list
    */
  def max(xs: List[Int]): Int = {
    xs match {
      case Nil => 0
      case x::Nil => x
      case x::y::tail => if(x>y) max(x::tail) else max(y::tail)
    }
  }
  /**
    * This method is an example for tail recursion i.e.
    * If a recursive function call's last action (last in terms of been evaluated last
    * is the function call itself it can be executed using the same stack frame.
    * This is particularly usefull in deep recursion where stack frames are a limited resource
    * Example:
    * def euclidGCD() as below => tail recursive
    *
    * def euclidGCD(x: Int, y: Int): Int =
    *   if(y == 0) x else x* euclidGCD(y, x%y)
    * => would not be tail recursive because x* eclidGCD() is evaluated last
    */
  def euclidGCD(x: Int, y: Int): Int =
    if(y == 0) x else euclidGCD(y, x%y)

  def factorial(x: Int):Int =
    x match{
      case 1 => 1
      case x => x* factorial(x-1)
    }
  /*This is the tail recursive version of factorial since the
  * help function loop is the last term to be evaluated in every
  * recursion step*/
  def factTailRecursive(n:Int): Int ={
    def loop(acc: Int, n: Int):Int =
      if(n==0) acc
      else  loop(acc*n, n-1)
    loop(1, n)
  }
}
