package chapter1.practice

object Ex2d5 extends App {
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
  //-----------------------
  def func1(x: Int) = 2 * x   //f
  def func2(y: Int) = 1 + y   //g

  val a = 5
  val b = func2(a)
  val c = func1(b)

  println(c)
  println(func1(func2(a)))
}