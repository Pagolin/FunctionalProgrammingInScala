package weekSix

/**
  * Created by lisza on 14.06.17.
  */
object combSearch {

}

object nQueens {
  def queens(n: Int): Set [List[Int]] = {
    def placeQueens(k: Int): Set[List[Int]] = {
      if(k==0) Set(List())
      else
        for{
          queens <- placeQueens(k-1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col :: queens
    }
    placeQueens(n)
  }

  def isSafe(clm: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    val queensCoords = (row -1 to 0 by -1) zip queens
    queensCoords forall{
      case (r,c) => clm != c && math.abs(clm - c) != row - r
    }
  }

  def showBoard(queens: List[Int]) = {
    val lines =
      for(col <- queens.reverse)
        yield Vector.fill(queens.length)("* ").updated(col, " X").mkString
    "\n" + (lines mkString "\n")
  }

}