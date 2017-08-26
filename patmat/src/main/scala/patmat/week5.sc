def last[T](list: List[T]):T = list match {
  case List() => throw new Error("no elements in list")
  case List(x) => x
  case y :: yy => last(yy)
}

def init[T](list: List[T]): List[T] = list match {
  case List() => throw new Error("no elements in list")
  case List(x) => List()
  case y :: yy => y :: init(yy)
}
def removeAt[T](n: Int, xs: List[T]): List[T] = n match {
  case 0 => xs.tail
  case _ => xs.head :: removeAt(n-1,xs.tail)
}

def removeAt2(n: Int, xs: List[Int]): List[Int] = (xs take n) ::: (xs drop n+1)
val ls=List(1,2,3,4,5)

(ls take 2) ::: (ls drop 3)
removeAt2(0,ls)