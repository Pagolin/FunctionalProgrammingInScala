package weekFour.patternMatchingAssignment

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import weekFour.patternMatchingAssignment.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times(\"hello, world\")") {
    assert(times(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd')) ===
      List(('h', 1),('e', 1),('l', 3),('o', 2),(',', 1),(' ', 1),('w', 1),('r',1),('d',1)))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1),('a', 4), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3), Leaf('a', 4)))
  }

  test("singleton: List (t1, t2), List(t1), List() should give false, true, false") {
    new TestTrees {
      assert(singleton(List(t1, t2)) === false)
      assert(singleton(List(t1)) === true)
      assert(singleton(List()) === false)
    }
  }

  test("combineCompl of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combineCompl(leaflist) === List(Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4), List('e','t','x'),7)))
  }

  test("decode 000111 to 'abdd' with t2") {
    new TestTrees {
      assert(decode(t2, List(0,0,0,1,1,1)) === List('a','b','d','d'))
    }
  }



  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("until(singleton, combine)() should give the same result as combineCompl") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(until(singleton, combine)(leaflist) === combineCompl(leaflist))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

}
