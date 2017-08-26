def balance(chars: List[Char]): Boolean = {
  matchParenthesis(chars,0)
}
def matchParenthesis(chars:List[Char],x:Int):Boolean={
  if(chars.isEmpty){
    if(x==0) true else false
  }else{
    chars.head match {
      case '(' =>matchParenthesis(chars.tail,x+1)
      case ')' =>if(x==0) false else matchParenthesis(chars.tail,x-1)
      case _ => matchParenthesis(chars.tail,x)
    }
  }
}
balance(List())
balance(List('(',')','d','(',')','c',')'))