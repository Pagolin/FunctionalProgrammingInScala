import weekSix.anagramsAssignment.forcomp.Anagrams.{Occurrences, Sentence, subtract, _}


val wrd = "abbcccddd"
val wrd2 = "abc"
val sent = List("I", "love", "you")
val ocmap = wrd.toLowerCase.groupBy((c:Char) => c).mapValues(_.length())
val ocList = ocmap.toList.sorted


val woc = wordOccurrences(wrd)
val woc2 = wordOccurrences(wrd2)
val foo1 = (woc).groupBy(_._1).mapValues(_ map (_._2))
val foo2 = (woc2).groupBy(_._1).mapValues(_ map (_._2))
val foo3 = foo1.map{case (k,v) => (k, v.head +1)}
val bar = woc map (elem =>
  if(foo2.contains(elem._1))(elem._1, elem._2 - foo2.get(elem._1).head.head) else elem)


def subtract(x: Occurrences, y: Occurrences): Occurrences = {
  val yAsMap = y.groupBy(_._1).mapValues(_ map (_._2))
  x map (elem => if(yAsMap.contains(elem._1))(elem._1, elem._2 - yAsMap.get(elem._1).head.head) else elem) filter(elem => elem._2!=0)
}

val bar2 = subtract(woc, woc2)

val longwoc = sentenceOccurrences(sent)
val typetest = combinations(longwoc)
val tt2 = typetest map (subset => dictionaryByOccurrences.getOrElse(subset, Nil))























val anagrams = wordAnagrams(wrd)

val test = woc map ((elem) => ((1 until elem._2+1) map (x => (elem._1, x))).toList)

val e = List()
val test2 = for {
  listcount <- test
  tupleNr <- listcount
} yield List(listcount, tupleNr)


def combi(wOccs: Occurrences): List[Occurrences] = wOccs match{
  case fstTuple :: rest =>{
    for {
      letterOcc <- ((0 until fstTuple._2+1) map ((x:Int) => (fstTuple._1, x))).toList
      otherletters <- combi(rest)
    } yield if(letterOcc._2 == 0) otherletters else letterOcc :: otherletters
  }
  case List() => List(List())
}

val testTuple = ('g', 4)
val x1 = (0 to testTuple._2).map((testTuple._1, _)).toList
val x2 = ((0 until testTuple._2+1) map ((x:Int) => (testTuple._1, x))).toList
val please = combi(woc)

