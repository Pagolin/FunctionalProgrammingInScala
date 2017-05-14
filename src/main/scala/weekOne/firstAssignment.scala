package weekOne

/**
  * Created by lisza on 14.05.17.
  */
object firstAssignment {

    def main(args: Array[String]) {
      println("Pascal's Triangle")
      for (row <- 0 to 10) {
        for (col <- 0 to row)
          print(pascal(col, row) + " ")
        println()
      }
    }

    /**
      * Exercise 1
      */
    def pascal(c: Int, r: Int): Int =
      (c, r) match {
        case (0, _) => 1
        case (x, y) => if (x == y) 1 else pascal(x - 1, y - 1) + pascal(x, y - 1)
      }

    /**
      * Exercise 2
      */
    def balance(chars: List[Char]): Boolean = {
      def innerLoop(chars: List[Char], opening: Int, closing: Int): Boolean =
        (chars, opening - closing) match {
          case (Nil, 0) => true
          case (_, x) => if (x < 0) false else chars.head match {
            case '(' => innerLoop(chars.tail, opening + 1, closing)
            case ')' => innerLoop(chars.tail, opening, closing + 1)
            case _ => innerLoop(chars.tail, opening, closing)
          }
        }

      innerLoop(chars, 0, 0)
    }

    /**
      * Exercise 3
      */
    def countChange(money: Int, coins: List[Int]): Int = {
      def innerLoop(money: Int, cList: List[Int]): Int =
        (money, cList) match {
          case (0, _) => 1
          case (_, Nil) => 0
          case (m, cList) => if (m < 0) 0 else innerLoop(money, cList.tail) + innerLoop(money - cList.head, cList)
        }

      innerLoop(money, coins)
    }

}
