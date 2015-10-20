package chapter1_3.practice

object HOF extends App {

  //returns the first index in an array where the key occurs, or -1 if it’s not found
  //polymorphic function (or generic function)
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)
  }

  def isZero(number: Int) : Boolean =
    number == 0

  def isEmpty(str: String) : Boolean =
    str != null && str.length == 0


  //RUN
  val arr1 = Array(1, 2, 3, 0, -3, 8)
  val arr2 = Array("str1", "str0", "1", null, "", "test")

  println(findFirst(arr1, isZero))
  println(findFirst(arr2, isEmpty))
}