package chapter1_3.practice

object Loops extends App {
  def factorial(n : Int) : Int = {
    def go(n : Int, accum : Int) : Int =
      if (n <= 0) accum
      else go(n-1, n * accum)

    go(n, 1)
  }

  //RUN
  val n = 5
  print("factorial of " + n + " is " + factorial(n))
}