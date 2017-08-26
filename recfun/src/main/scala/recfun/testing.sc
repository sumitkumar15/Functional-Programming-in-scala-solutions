def sum(f:Int =>Int,a:Int,b:Int): Int ={
  if(a>b)
    return 0
  else f(a) + sum(f,a+1,b)
}

def square(x:Int): Int ={
  x*x
}
def cube(x:Int):Int={
  x*x*x
}
sum(square(cube),1,3)