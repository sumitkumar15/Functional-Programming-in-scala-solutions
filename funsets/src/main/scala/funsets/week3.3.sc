trait List[T]{
  def isEmpty:Boolean
  def head:T
  def tail:List[T]
}
class Nil[T] extends List[T]{
  val isEmpty=true
  def head:T = throw new NoSuchElementException("Nil.head")
  def tail:List[T] = throw new NoSuchFieldException("Nil.tail")
}

class Cons[T](val head:T, val tail:List[T]) extends List[T]{
  def isEmpty=false
}

def singletonList[T](elem: T) = new Cons[T](elem,new Nil[T])

var x=singletonList(5)
 var y=singletonList(10,x)

val lis= new Cons(1,new Cons(2,new Cons(3,new Nil)))

def nth[T](n:Int , list: List[T]):List[T] = {
  if(list.isEmpty) throw new IndexOutOfBoundsException
  else {
    if (n == 0) list
    else nth(n - 1, list.tail)
  }
}

nth(2,lis).head
