package chapter1.practice

class PartialApp {
  def partial[A,B,C](a: A, f: (A,B) => C): B => C =
    b => f(a, b)     // or  (b: B) => f(a, b)
}