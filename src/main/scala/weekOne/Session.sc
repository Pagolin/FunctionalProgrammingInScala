
object session {

  println("Works")
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

  def euclidGCD(x: Int, y: Int): Int =
    if(y == 0) x else euclidGCD(y, x%y)

  sqrtNewton(9)

  euclidGCD(12, 8)

  println("Pascal's Triangle")
  for (row <- 0 to 10) {
    for (col <- 0 to row)
      print(pascal(col, row) + " ")
    println()
  }
  def pascal(c: Int, r: Int): Int =
    (c,r) match{
      case (0, _) => 1
      case (x, y) => if(x == y) 1 else pascal(x-1, y-1) + pascal(x, y-1)
    }

  def countChange(money: Int, coins: List[Int]): Int = {
    def innerLoop(money: Int, cList: List[Int]):Int ={
      println("$money $cList")
      (money, cList) match {
        case (0, _) => 1
        case (_, Nil) => 1
        case (m, cList) =>  if(m-cList.head < 0) 0 else innerLoop(money, cList.tail) + innerLoop(money-cList.head, cList)
      }
    }
    innerLoop(money, coins)
  }

  val x = List(1,2,3,4)
  countChange(4,x)
}