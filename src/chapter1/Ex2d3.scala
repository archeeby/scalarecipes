package chapter1

object Ex2d3 extends App {
  //currying
  def curry[A,B,C](f: (A, B) => C): A => B => C = {
    a => b => f(a, b)
  }

  val a = curry[Int, Int, Int](_ - _)
  val b = a(5)
  val c = b(1)
  println(c)

  //or
  val a1 = curry[Int, Int, Int](_ - _)
  val b1 = a1(5)(1)
  println(b1)

  println(curry[Int, Int, Int](_ - _)(5)(1))
}