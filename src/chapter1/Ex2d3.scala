package chapter1

object Ex2d3 extends App {
  //currying
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }
}