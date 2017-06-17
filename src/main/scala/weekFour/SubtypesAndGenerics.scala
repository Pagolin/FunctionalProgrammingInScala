package weekFour

import weekThree.ClassHierarchies.{IntSet, NonEmpty, Empty1}
import weekThree.objectSetsAssignment.Empty

/**
  * Created by lisza on 24.05.17.
  */
/**
  * Working with generic types one could or will want
  * to restrict the possible types of unbound parameters
  *
  * This is realized with upperbounds restricting
  * the supertype to witch the argument has to conform
  * (like <T extends someClass> in java)...
  * or lowerbounds for thr unbound parameter,
  * meaning that is has to be a supertype of the given boundary type  *
  */

abstract class SubtypesAndGenerics {
  //Example of upperbound: S has to be subtype of IntSet
  def assertAllPos[S <: IntSet](r: S): S = ???

  //Example of lowerbound: S has to be  a supertype of Empty -> IntSet
  def assertAllPos2[S >: Empty](r: S): S = ???


  /**
    * Covariance:
    * Given the relation
    * Empty <: IntSet
    * follows, that also the projection of this relation to Lists
    * of IntSets makes sense
    * List[Empty] <: List[IntSet]
    *
    * While arrays are covariant in java, in scala they aer not, making the following
    * evaluation of b cause a type error
    **/
  //val a: Array[NonEmpty] = Array(new NonEmpty(1, Empty1, Empty1))
  //val b: Array[IntSet] = a
}
