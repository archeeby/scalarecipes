package chapter1_3.practice

object Ex2d2 extends App {

  //checks whether an Array[A] is sorted according to a given comparison function
  //polymorphic function (or generic function)
  def isSorted[A](arr: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n == arr.length - 2) true
      else if (ordered(arr(n), arr(n + 1))) loop(n + 1)
      else false

    loop(0)
  }

  def isNumbersSorted(n1: Int, n2: Int): Boolean =
    n1 <= n2

  val arr3 = Array(-1, 0, 1, 3, 9)
  val arr4 = Array(3, 9, 12, 5, 15)

  println(isSorted(arr3, isNumbersSorted))
  println(isSorted(arr4, isNumbersSorted))
}