package chapter1

object Ex2d1 {

  //gets the n-th Fibonacci number
  def fib(n : Int) : Int = {
    @annotation.tailrec
    def go(n : Int, a : Int, b : Int) : Int = {
      if (n <= 0) a
      else go(n - 1, b, a + b)
    }

    go(n, 0, 1)
  }

  def main(args: Array[String]) : Unit = {
    println(fib(10))
  }
}