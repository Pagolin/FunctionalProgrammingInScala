import weekThree.ClassHierarchies.NonEmpty

object session{
  val testset = new Empty
  testset
  val Tweet1= new Tweet("gizmodo","These new Apple patents give a sneak peek at what future iPhone cameras might have in store. http://t.co/0YT9rjxp",49)
  val set2 = new NonEmpty(Tweet1, new Empty, new Empty)
  testset.getClass.toString
}