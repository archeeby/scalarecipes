package chapter1

class Ex2d1 {
  def fib(n : Int) : Int = {
    @annotation.tailrec
    def go(n : Int, a : Int, b : Int) : Int = {
      if (n <= 0) a
      else go(n - 1, b, a + b)
    }

    go(n, 0, 1)
  }

}