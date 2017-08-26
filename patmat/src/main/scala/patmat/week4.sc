import patmat.Huffman.CodeTable

//abstract class CodeTree
//case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
//case class Leaf(char: Char, weight: Int) extends CodeTree
//
//
//// Part 1: Basics
//def weight(tree: CodeTree): Int = tree match {
//  case Fork(left,right,lst,weight) => weight
//  case Leaf(char,weight) => weight
//}// tree match ...
//
//def chars(tree: CodeTree): List[Char] = tree match {
//  case Fork(left,right,lst,weight) => lst
//  case Leaf(char,weight) => List(char)
//} // tree match ...
//
//def makeCodeTree(left: CodeTree, right: CodeTree) =
//  Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))
//def times(chars: List[Char]): List[(Char, Int)] = {
//  timesHelper(chars, List() )
//}
//def timesHelper(chars: List[Char],acc: List[(Char,Int)]):List[(Char,Int)]={
//  if(chars.isEmpty) acc
//  else {
//    if(acc.nonEmpty){
//      val ipair=acc.head
//      if(acc.exists( (ipair) => ipair._1 == chars.head)){
//        timesHelper(chars.tail,acc)
//      }else{
//        val pair: (Char,Int)= (chars.head,chars.count((x: Char) => x==chars.head))
//        timesHelper(chars.tail,acc.::(pair))
//      }
//    }else{
//      val pair: (Char,Int)= (chars.head,chars.count((x: Char) => x==chars.head))
//      timesHelper(chars.tail,acc.::(pair))
//    }
//
//  }
//}
//
//val t=times(List('a','b','c','a','a','b','d','d','d','d'))
//t.sortBy(_._2)
//def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
//  val sortedList=freqs.sortBy(_._2).reverse
//  def listHelper(sorted: List[(Char,Int)], acc: List[Leaf]): List[Leaf]={
//    if(sorted.isEmpty) acc
//    else {
//      val pair: Leaf= Leaf(sorted.head._1, sorted.head._2)
//      listHelper(sorted.tail,acc.::(pair))
//    }
//  }
//  listHelper(sortedList,List())
//}
//
//makeOrderedLeafList(t)
//
//val qwe=List(1,2,3)
//
////List[CodeTree](Fork(l,r,List('a'),5))
//
//List[Int](1,2,3).::(5)
abstract class CodeTree
case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
case class Leaf(char: Char, weight: Int) extends CodeTree


// Part 1: Basics
def weight(tree: CodeTree): Int = tree match {
  case Fork(left,right,lst,weight) => weight
  case Leaf(char,weight) => weight
}// tree match ...

def chars(tree: CodeTree): List[Char] = tree match {
  case Fork(left,right,lst,weight) => lst
  case Leaf(char,weight) => List(char)
} // tree match ...

def makeCodeTree(left: CodeTree, right: CodeTree) =
  Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))



// Part 2: Generating Huffman trees

/**
  * In this assignment, we are working with lists of characters. This function allows
  * you to easily create a character list from a given string.
  */
def string2Chars(str: String): List[Char] = str.toList

/**
  * This function computes for each unique character in the list `chars` the number of
  * times it occurs. For example, the invocation
  *
  *   times(List('a', 'b', 'a'))
  *
  * should return the following (the order of the resulting list is not important):
  *
  *   List(('a', 2), ('b', 1))
  *
  * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
  * character and an integer. Pairs can be constructed easily using parentheses:
  *
  *   val pair: (Char, Int) = ('c', 1)
  *
  * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
  *
  *   val theChar = pair._1
  *   val theInt  = pair._2
  *
  * Another way to deconstruct a pair is using pattern matching:
  *
  *   pair match {
  *     case (theChar, theInt) =>
  *       println("character is: "+ theChar)
  *       println("integer is  : "+ theInt)
  *   }
  */
def timesHelper(chars: List[Char],acc: List[(Char,Int)]):List[(Char,Int)]={
  if(chars.isEmpty) acc
  else {
    if(!acc.isEmpty){
      if(acc.exists( (ipair) => ipair._1 == chars.head)){
        timesHelper(chars.tail,acc)
      }else{
        val pair: (Char,Int)= (chars.head,chars.count((x: Char) => x==chars.head))
        timesHelper(chars.tail,acc.::(pair))
      }
    }else{
      val pair: (Char,Int)= (chars.head,chars.count((x: Char) => x==chars.head))
      timesHelper(chars.tail,acc.::(pair))
    }

  }
}

def times(chars: List[Char]): List[(Char, Int)] = {
  timesHelper(chars, List() )
}
val lstt=List('a','a','a','a','b','c','n','n','n','n','n','m','m','m','m','m','m','m','m','m')
val testing=times(lstt)


/**
  * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
  *
  * The returned list should be ordered by ascending weights (i.e. the
  * head of the list should have the smallest weight), where the weight
  * of a leaf is the frequency of the character.
  */
def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
  val sortedList=freqs.sortBy(_._2).reverse
  //println(sortedList)
  def listHelper(sorted: List[(Char,Int)], acc: List[Leaf]): List[Leaf]={
    if(sorted.isEmpty) acc
    else {
      val pair: Leaf= Leaf(sorted.head._1, sorted.head._2)
      listHelper(sorted.tail,acc.::(pair))
    }
  }
  listHelper(sortedList,List())
}

val ordList=makeOrderedLeafList(testing)
/**
  * Checks whether the list `trees` contains only one single code tree.
  */
def singleton(trees: List[CodeTree]): Boolean = trees.length match { case 1 => true ; case _ => false}

/**
  * The parameter `trees` of this function is a list of code trees ordered
  * by ascending weights.
  *
  * This function takes the first two elements of the list `trees` and combines
  * them into a single `Fork` node. This node is then added back into the
  * remaining elements of `trees` at a position such that the ordering by weights
  * is preserved.
  *
  * If `trees` is a list of less than two elements, that list should be returned
  * unchanged.
  */
def combine(trees: List[CodeTree]): List[CodeTree] = {
  if(trees.isEmpty) trees
  else if(singleton(trees)) trees
  else{
//    val tryi=makeCodeTree(trees.head,trees.tail.head)

    insertInList(trees.tail.tail,makeCodeTree(trees.head,trees.tail.head))
  }
}
def insertInList(trees: List[CodeTree], codeTree: CodeTree): List[CodeTree]={

  def find(leftTree: List[CodeTree],rightTree: List[CodeTree]): List[CodeTree]={
    if(rightTree.isEmpty) leftTree.reverse.::(codeTree).reverse
    else{
      rightTree.head match {
        case Fork(left,right,lst,wt) => if(weight(codeTree) <= wt) leftTree.::(codeTree).reverse ::: rightTree
        else find(leftTree.::(right),rightTree.tail)
        case Leaf(char,wet) => if(weight(codeTree) <= wet) leftTree.::(codeTree).reverse ::: rightTree
        else find(leftTree.::(rightTree.head),rightTree.tail)
      }
    }
  }
  find(List(),trees)
}


/**
  * This function will be called in the following way:
  *
  *   until(singleton, combine)(trees)
  *
  * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
  * the two functions defined above.
  *
  * In such an invocation, `until` should call the two functions until the list of
  * code trees contains only one single tree, and then return that singleton list.
  *
  * Hint: before writing the implementation,
  *  - start by defining the parameter types such that the above example invocation
  *    is valid. The parameter types of `until` should match the argument types of
  *    the example invocation. Also define the return type of the `until` function.
  *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
  */
def until( single: List[CodeTree] => Boolean,comb: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): CodeTree = {
  if(single(trees)) trees.head
  else until(single,comb)(comb(trees))
}

println(combine(List(Leaf('c',1),Leaf('b',1))))
/**
  * This function creates a code tree which is optimal to encode the text `chars`.
  *
  * The parameter `chars` is an arbitrary text. This function extracts the character
  * frequencies from that text and creates a code tree based on them.
  */
def createCodeTree(chars: List[Char]): CodeTree = {
  until(singleton,combine)(makeOrderedLeafList(times(chars)))
}


// Part 3: Decoding

type Bit = Int

/**
  * This function decodes the bit sequence `bits` using the code tree `tree` and returns
  * the resulting list of characters.
  */
def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
  def decodeHelper(itree: CodeTree, ibits:List[Bit], acc:List[Char] ): List[Char]={

    if(ibits.isEmpty) {
      itree match {
        case Leaf(char,weight) => acc.::(char).reverse
      }
    }
    else {
      itree match {
        case Fork(left, right, chars, weight) => if (ibits.head == 0) decodeHelper(left, ibits.tail, acc) else decodeHelper(right, ibits.tail, acc)
        case Leaf(char, weight) =>{ decodeHelper(tree,ibits,acc.::(char)) }
      }
    }
  }
  decodeHelper(tree,bits,List())
}

val tre=createCodeTree(lstt)

decode(tre,List(0,1,0,1,0))

/**
  * A Huffman coding tree for the French language.
  * Generated from the data given at
  *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
  */
val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

/**
  * What does the secret message say? Can you decode it?
  * For the decoding use the `frenchCode' Huffman tree defined above.
  */
val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

/**
  * Write a function that returns the decoded secret
  */
def decodedSecret: List[Char] = decode(frenchCode,secret)

decodedSecret

def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
  def encodeHelper(inputText: List[Char], acc: List[Bit]):List[Bit]={
    if(inputText.isEmpty) acc
    else {
//      println(findBitSeq(tree,inputText.head,List()))
      encodeHelper(inputText.tail,acc ::: findBitSeq(tree,inputText.head,List()))
    }
  }
  encodeHelper(text,List())
}
def findBitSeq(tree: CodeTree,char: Char,acc: List[Bit]):List[Bit]={
  tree match {
    case Fork(left,right,chars,weight) => if(isExist(left,char)) findBitSeq(left,char,acc.::(0)) else findBitSeq(right,char,acc.::(1))
    case Leaf(nchar,weight) => acc.reverse
  }
}
def isExist(tree: CodeTree,char: Char):Boolean={
  tree match {
    case Fork(left,right,list,weight) => list.contains(char) //list.exists((x: Char) => x==char)
    case Leaf(nchar,weight)=> nchar==char
  }
}
secret
val t=encode(frenchCode)(decodedSecret)

decode(frenchCode,secret)

//findBitSeq(frenchCode,'h',List())
//findBitSeq(frenchCode,'i',List())
//findBitSeq(frenchCode,'e',List())
//findBitSeq(frenchCode,'f',List())
//
//decode(frenchCode,List(0,1,1,1))
decode(frenchCode,List(1,1,1,1))

decode(frenchCode,List(0,0,1,1,1,0,1))
def convert(tree: CodeTree): CodeTable = {
  convHelp(tree,List(),List())
}
def convHelp(tree: CodeTree, acc: List[Bit],list: CodeTable): CodeTable ={
  tree match {
    case Fork(left,right,chars, weight) => {
      mergeCodeTables(list , mergeCodeTables(convHelp(left,acc.::(0),list) ,convHelp(right,acc.::(1),list)))
    }
    case Leaf(char,weight) => {
      mergeCodeTables(list , List((char,acc.reverse)))
    }
  }
}

/**
  * This function takes two code tables and merges them into one. Depending on how you
  * use it in the `convert` method above, this merge method might also do some transformations
  * on the two parameter code tables.
  */
def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a ::: b

/**
  * This function encodes `text` according to the code tree `tree`.
  *
  * To speed up the encoding process, it first converts the code tree to a code table
  * and then uses it to perform the actual encoding.
  */
def qEncode(codeTable: CodeTable, char: Char): List[Bit]={
  if(codeTable.head._1 == char) codeTable.head._2
  else qEncode(codeTable.tail,char)
}
def sqEncode(table: CodeTable, text: List[Char], acc: List[Bit]): List[Bit]={
  if(text.isEmpty) acc
  else sqEncode(table,text.tail, acc ::: qEncode(table,text.head))
}
def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
  val table = convert(tree)
  sqEncode(table,text,List())
}

quickEncode(frenchCode)(decodedSecret)