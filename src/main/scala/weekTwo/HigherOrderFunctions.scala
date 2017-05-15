package weekTwo
import weekOne.FuncAndEval.abs

/**
  * Created by lisza on 15.05.17.
  * In functional programming functions can be treated as first-class values, i.e.
  * passed as parameters and returned as results by other functions.
  * In such cases the latter are called higher-order functions.
  */
object HigherOrderFunctions {
  /*
  Introductory example
    Sums of ints, cubes and factorials of numbers between two values a and b
  */

  //id, cube and fact are functions treated as first-class values
  def id(x: Int): Int = x
  def cube(x: Int): Int = x*x*x
  def fact(x: Int): Int = if(x==0) 1 else x*fact(x-1)
  //def sum is the higher-order function
  def sum(f: Int => Int, a: Int, b: Int): Int =
    if(a > b) 0
    else f(a) + sum(f, a+1, b)
  //application
  def sumCubes(a:Int, b: Int) = sum(cube, a, b)

  /*
  One can avoid writing numerous little functions
   using anonymous functions as syntactic sugar just
   (see lambda-calculus)
   */
  def sumCubesNew(a: Int, b: Int) = sum(x => x*x*x, a, b)

  // Or as tail recursive version
  def sumTR(f: Int => Int, a: Int, b: Int): Int ={
    def loop(a: Int, acc: Int): Int =
      if(a > b) acc
      else loop(a+1, acc+f(a))
    loop(a, 0)
  }
  sumTR(x => x*x, 2, 4)

  /**
    * Currying
    * Write functions even more concise (i.e. avoid the middle man
    * function like cube) by passing them a function and make them return another function build
    * it.
    */
  def sumCurry(f: Int => Int): (Int, Int) => Int = {
    def returnFunction(a: Int, b: Int): Int =
      if(a > b) 0
      else f(a) + returnFunction(a+1, b)
    //returnFunction is the return value
    returnFunction
  }

  sumCurry(x => x*x*x) (2,4)
  //First sumCurry is applied to the anonymous function x=> x*x*x,
  //Second values (2,4) are bound to the parameters of returnFunction

  /*
  * The following function generalizes the sumCurry and prodCurry (worksheet function)
  * */
  def genCurry(f: Int => Int, g: (Int, Int) => Int, neutral: Int): (Int, Int) => Int = {
    def returnFunction(a: Int, b: Int): Int =
      if(a > b) neutral
      else g(f(a), returnFunction(a+1, b))
    returnFunction
  }

  genCurry(x => x*x, (x, y) => x*y, 1)(3,4)

  /**
    * Finding functions fixed points
    * Fixed point of function f() is x such that  -> f(x) == x
    * Approximate x by iteration
    */
  val tolerance = 0.0001
  def isCloseEnough(x: Double, y: Double) =
    abs((x-y)/ x)/ x < tolerance
  def fixedPoint(f: Double => Double) (firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if(isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }
  //Application as Newton approximation to square root
  // i.e. x = sqrt(x)* sqrt(x) =>  at fixed point sqrt(x) = y = x/y
  // To make the function converge the successive guess values are averaged
  def sqrt(x : Double) = fixedPoint(y => (y+ x/y)/2) (1)

  // rewritten
  def averageDump(f: Double => Double)(x: Double) = (x + f(x))/2

  def sqrtAD(x: Double) =
    fixedPoint(averageDump(y => x/y))(1)
  sqrtAD(2)

}