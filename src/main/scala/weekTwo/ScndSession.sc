def sum(f: Int => Int, a: Int, b: Int): Int ={
  def loop(a: Int, acc: Int): Int =
    if(a > b) acc
    else loop(a+1, acc+f(a))
  loop(a, 0)
}

sum(x => x*x, 2, 4)

def prodCurry(f: Int => Int): (Int, Int) => Int = {
  def returnFunction(a: Int, b: Int): Int =
    if(a > b) 1
    else f(a) * returnFunction(a+1, b)
  //returnFunction is the return value
  returnFunction
}

prodCurry(x => x*x)(3,4)

def genCurry(f: Int => Int, g: (Int, Int) => Int, neutral: Int): (Int, Int) => Int = {
  def returnFunction(a: Int, b: Int): Int =
    if(a > b) neutral
    else g(f(a), returnFunction(a+1, b))
  //returnFunction is the return value
  returnFunction
}

genCurry(x => x*x, (x, y) => x+y, 0)(3,4)

class Rational(x: Int, y: Int) {
  def numerator = x

  def denominator = y

  //Methods of class objects
  def add(that: Rational) =
    new Rational(
      numerator * that.denominator + that.numerator * denominator,
      denominator * that.denominator)

  def neg: Rational = new Rational(-numerator, denominator)

  def subtr(that : Rational) : Rational = add(that.neg)
  override def toString = numerator + "/" + denominator
}

val x = new Rational(1,2)
val y = new Rational(3,4)
val z = x.subtr(y)
val w = x subtr y







