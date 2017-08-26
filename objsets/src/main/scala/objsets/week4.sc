abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}
object Zero extends Nat{
  def isZero =true
  def predecessor =throw new NoSuchElementException
  def successor = new Succ(this)
  def + (that: Nat)=that
  def - (that: Nat)= if(that.isZero) this else throw new NoSuchElementException
}
class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false

  override def predecessor: Nat = n

  override def successor: Nat = new Succ(this)

  override def +(that: Nat): Nat = new Succ(n + that)

  override def -(that: Nat): Nat = if(that.isZero) this else n-that.predecessor
}
//package scala
//trait Function1[A,B]{
//  def apply(x: A): B
//}
//trait Function2[A,B,C]{
//  def apply(x: A,y: B):C
//}
//trait Function3[A,B,C,D]{
//  def apply(x: A,y: B,z: C):D
//}
object List {
  def apply(x: Int) = new List
}