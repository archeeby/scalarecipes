package chapter1

object Ex2d4 extends App {
  //reverses the transformation of curry
  def uncurry[A,B,C](f: A => (B => C)): (A, B) => C = {
    (a : A, b : B) => f(a)(b)
  }

  val func = uncurry[String, String, String](a => b => a + b)
  println(func("Hello ", "world"))
}