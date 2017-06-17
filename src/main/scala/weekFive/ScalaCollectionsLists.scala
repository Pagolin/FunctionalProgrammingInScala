package weekFive


/**
  * Created by lisza on 29.05.17.
  */
object ScalaCollectionsLists {
  /**
    * Lists, as a container class in scala are
    *   -immutable (i.e. elements can not be changed)
    *   -recursive List(1st, (List(2nd, List(3rd))))
    *   -constructed from the empty list Nil and ::
    *   -> val fruits = List("melon", "pepino", "jackfruit")
    *     ...is syntacic sugar for
    *       val fruit = "melon"::("pepino"::("jackfruit" :: Nil))
    *     ...by convention parenthesis can be omitted
    *
    */
  //Example for insertion sort on Lists
  def iSort(xs : List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, iSort(ys))
  }

  def insert(x : Int, xs: List[Int]): List[Int] = xs match{
    case List() => List(x)
    case y::ys => if(x <= y) x::xs else y::insert(x, ys)
  }

  /*
  * From the recursive structure of List arises some complexity
   * in seemingly simple tasks as getting the last element of a List*/

  def last[T](xs : List[T]): T = xs match{
    case List() => throw new Error("last of empty list")
    case List(x) => x
    case y::ys => last(ys)
  }
  def init[T](xs : List[T]): List[T] = xs match{
    case List() => throw new Error("init of empty list")
    case List(x) => List()
    case y::ys => y::init(ys)
  }

  def reverse[T](xs : List[T]): List[T] = xs match{
    case List() => xs
    case y::ys => reverse(ys)++List(y)
  }

  def removeAt[T](n : Int, xs : List[T]): List[T] = (xs take n) ::: (xs drop n+1)

  def mergeSort(xs : List[Int]): List[Int] = {
    val middle = xs.length/2
    if(middle == 0) xs
    else {
      def merge(xs: List[Int], ys:List[Int]): List[Int]=
        xs match {
          case Nil => ys
          case x :: xs1 =>
            ys match {
              case Nil => xs
              case y :: ys1 => if(x<y) x::merge(xs1,ys) else y:: merge(xs, ys1)
            }
        }
      val (fst, snd) = xs splitAt middle
      merge(mergeSort(fst), mergeSort(snd))
    }
  }
}
