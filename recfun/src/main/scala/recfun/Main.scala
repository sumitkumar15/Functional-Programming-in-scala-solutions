package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if(c == 0 || c == r)
        1 else{
        pascal(c-1,r-1)+pascal(c,r-1)
      }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      matchParenthesis(chars,0)
    }
  def matchParenthesis(chars:List[Char],x:Int):Boolean= {
    if (chars.isEmpty) {
      if (x == 0)
        true
      else
        false
    } else {
      chars.head match {
        case '(' => matchParenthesis(chars.tail, x + 1)
        case ')' => if (x == 0) false else matchParenthesis(chars.tail, x - 1)
        case _ => matchParenthesis(chars.tail, x)
      }
    }
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(money==0)
        1
      else if(money<=0)
        0
      else if(money>=0 && coins.isEmpty)
        0
      else{
        countChange(money,coins.tail)+countChange(money-coins.head,coins)
      }
    }

  def sum(f:Int =>Int,a:Int,b:Int): Int ={
    if(a<b)
      return 0
    else f(a) + sum(f,a+1,b)
  }

  def square(x:Int): Int ={
      x*x
  }
  }
