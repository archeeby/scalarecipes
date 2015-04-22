package chapter1.practice

object PartialApp extends App {
  def partial[A,B,C](a: A, f: (A,B) => C): B => C =
    b => f(a, b)     // or  (b: B) => f(a, b)

  val part = partial[String, String, String]("Hello ", _ + _)
  println(part("world"))
}