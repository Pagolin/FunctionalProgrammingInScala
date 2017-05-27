package patmat


/**
  * Assignment 4: Huffman coding
  *
  */
object Huffman {

  /**
    * A huffman code is represented by a binary tree.
    *
    * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
    * The weight of a `Leaf` is the frequency of appearance of the character.
    *
    * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
    * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
    * leaves.
    */
  abstract class CodeTree

  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree

  case class Leaf(char: Char, weight: Int) extends CodeTree


  // Part 1: Basics
  def weight[T <: CodeTree](tree: T): Int = tree match {
    case Leaf(c, w) => w
    case Fork(l, r, ch, w) => weight(l) + weight(r)
  }


  def chars(tree: CodeTree): List[Char] = tree match {
    case Leaf(c, w) => List(c)
    case Fork(l, r, ch, w) => ch
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))


  // Part 2: Generating Huffman trees

  /**
    * In this assignment, we are working with lists of characters. This function allows
    * you to easily create a character list from a given string.
    */
  def string2Chars(str: String): List[Char] = str.toList

  def times(chars: List[Char]): List[(Char, Int)] = {
    def loopOverText(chars: List[Char], acc: List[(Char, Int)]): List[(Char, Int)] = chars match {
      case Nil => acc
      case (c :: rest) => loopOverText(rest, addCharToList(acc, c))
    }

    def addCharToList(list: List[(Char, Int)], ch: Char): List[(Char, Int)] = list match {
      case Nil => List((ch, 1))
      case ((c1, i1) :: rest) => if (c1 == ch) List((c1, i1 + 1)) ++ rest else ((c1, i1) :: addCharToList(rest, ch))
    }

    loopOverText(chars, Nil)
  }

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = freqs match {
    case Nil => List()
    case ((c, i) :: rest) => sortedInsert(new Leaf(c, i), makeOrderedLeafList(rest))
  }

  def sortedInsert[T <: CodeTree](l: T, leafList: List[T]): List[T] = leafList match {
    case Nil => leafList ++ List(l)
    case (leaf :: rest) => if (weight(l) < weight(leaf)) List(l, leaf) ++ rest else List(leaf) ++ sortedInsert(l, rest)
  }

  /**
    * Checks whether the list `trees` contains only one single code tree.
    */
  def singleton(trees: List[CodeTree]): Boolean = trees match {
    case (x :: Nil) => true
    case _ => false
  }


  def combineCompl(trees: List[CodeTree]): List[CodeTree] = trees match {
    //if there are at least two elements in the list, combine them, insert the resulting tree into the
    // list and proceed with the resulting list
    case (t1 :: t2 :: something) => combine(sortedInsert(makeCodeTree(t1, t2), something))
    // if there are less than two tree's in the trees list just pass it back
    case _ => trees
  }

  def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
    case (t1 :: t2 :: something) => sortedInsert(makeCodeTree(t1, t2), something)
    case _ => trees
  }

  def until(f: List[CodeTree]=> Boolean, g : List[CodeTree] => List[CodeTree])(list: List[CodeTree]): List[CodeTree] =
    if(f(g(list))) g(list)
    else until(f, g)(g(list))


  def createCodeTree(chars: List[Char]): CodeTree =
    (until(singleton, combine)(makeOrderedLeafList(times(chars)))).head


  // Part 3: Decoding

  type Bit = Int

  /**
    * This function decodes the bit sequence `bits` using the code tree `tree` and returns
    * the resulting list of characters.
    */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    def inner(innerTree: CodeTree, bits: List[Bit], accu: List[Char]): List[Char] = innerTree match{
      case Leaf(c, w) => if(bits.isEmpty) accu++List(c) else inner(tree, bits, accu++List(c))
      case Fork(l, r, ch, w) => if (bits.head == 0) inner(l, bits.tail, accu) else inner(r, bits.tail, accu)
    }
    inner(tree,bits, Nil)
  }

  /**
    * A Huffman coding tree for the French language.
    * Generated from the data given at
    * http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
    */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)

  /**
    * What does the secret message say? Can you decode it?
    * For the decoding use the 'frenchCode' Huffman tree defined above.
    */
  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

  /**
    * Write a function that returns the decoded secret
    */
  def decodedSecret: List[Char] = decode(frenchCode, secret)


  // Part 4a: Encoding using Huffman tree

  /**
    * This function encodes `text` using the code tree `tree`
    * into a sequence of bits.
    */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def inner(innerTree: CodeTree)(c: Char): List[Bit] = innerTree match{
      case Leaf(c,w) => List()
      case Fork(l, r, ch, w) =>
        if(chars(l).contains(c)) List(0)++ inner(l)(c)
        else if(chars(r).contains(c)) List(1) ++ inner(r)(c)
        else throw new NoSuchElementException("Got lost in the tree")
    }
    text.flatMap(inner(tree))
  }


  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
    * This function returns the bit sequence that represents the character `char` in
    * the code table `table`.
    */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = { table.filter((entry)=> entry._1 == char).head._2}

  /**
    * Given a code tree, create a code table which contains, for every character in the
    * code tree, the sequence of bits representing that character.
    *
    * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
    * a valid code tree that can be represented as a code table. Using the code tables of the
    * sub-trees, think of how to build the code table for the entire tree.
    */
  def convert(tree: CodeTree): CodeTable = tree match {
    case Leaf(c, w) => List((c,List()))
    case Fork(l,r,ch,w) => mergeCodeTables(convert(l), convert(r))
  }


  /**
    * This function takes two code tables and merges them into one. Depending on how you
    * use it in the `convert` method above, this merge method might also do some transformations
    * on the two parameter code tables.
    */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
    a.map((elem) => (elem._1, 0::elem._2)) ++ b.map((elem) => (elem._1, 1::elem._2))
  }

  /**
    * This function encodes `text` according to the code tree `tree`.
    *
    * To speed up the encoding process, it first converts the code tree to a code table
    * and then uses it to perform the actual encoding.
    */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = text flatMap codeBits(convert(tree))

  def main(args: Array[String]) {
    val t2  = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val seq = List(0,0,0,1,1,1)
    decode(t2, seq)
  }


}
