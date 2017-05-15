package weekTwo

/**
  * Created by lisza on 15.05.17.
  */
object DataStructures {

  //Datatypes are defined as classes like as follows
  //Types and values are kept in separate namespaces
  class Rational(x: Int, y: Int) {
    //Requirements throw Exception on violation, with optional message
    //require: Precondition test, throws IllegalArgumentException
    //assert: Code function test, throw AssertionError
    require(y != 0, "Denominator must be nonzero")

    //Alternative constructor with only one argument, it calls the implicit primary constructor
    def this(x: Int) = this(x, 1)

    //Make sure to have reduced rationals
    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
    private val g = gcd(x, y)

    def numerator = x / g
    def denominator = y / g

    //Methods of class objects
    def add(that: Rational) =
      new Rational(
        numerator * that.denominator + that.numerator * denominator,
        denominator * that.denominator)

    def neg: Rational = new Rational(-numerator, denominator)

    def subtr(that: Rational): Rational = add(that.neg)

    def less(that: Rational): Boolean =
      numerator * that.denominator < that.numerator * denominator

    def max(that: Rational): Rational =
      if (this.less(that)) that else this

    override def toString = numerator + "/" + denominator
  }

  //In scala symbols can also be used as identifiers enabling naming as follows
  class RationalFancyNotation(x: Int, y: Int) {
    require(y != 0, "Denominator must be nonzero")
    def this(x: Int) = this(x, 1)
    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
    private val g = gcd(x, y)

    def numerator = x / g
    def denominator = y / g

    //Methods of class objects
    def +(that: RationalFancyNotation) =
      new RationalFancyNotation(
        numerator * that.denominator + that.numerator * denominator,
        denominator * that.denominator)

  //since neg is representing a unary operation the notation is slightly different
    def unary_- : RationalFancyNotation = new RationalFancyNotation(-numerator, denominator)

    def -(that: RationalFancyNotation): RationalFancyNotation = this + -that

    def <(that: RationalFancyNotation): Boolean =
      numerator * that.denominator < that.numerator * denominator

    def >>(that: RationalFancyNotation): RationalFancyNotation =
      if (this < that) that else this

    override def toString = numerator + "/" + denominator
  }

  //Note: Methods, as any other functions can be used in infix notation
  def main(args: Array[String]) {
    val x = new Rational(1,2)
    val y = new Rational(3,4)
    val z = x.subtr(y)
    val w = x subtr y
    println(s"Obviously $w is the same as $z")
    val xf = new RationalFancyNotation(1,2)
    val yf = new RationalFancyNotation(3,4)
    println(s"The maximum of $xf and $yf is ${xf >> yf} ")
  }
}
