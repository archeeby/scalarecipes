package chapter1.datasharing

sealed trait List[+A]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

case object Nil extends List[Nothing]

object List {
  //returns size of list
  def size[A](list: List[A]): Integer = {
    @annotation.tailrec
    def go(n: Integer, innerList: List[A]): Integer = innerList match {
      case Nil => n
      case Cons(_, xs) => go(n + 1, xs)
    }
    go(0, list)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  //returns a tail of
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(_, xs) => xs
  }

  //replace first element of collection
  def setHead[A](a: A, list: List[A]): List[A] = list match {
    case Nil => List(a)
    case Cons(_, xs) => Cons(a, xs)
  }

  //removes n first elements of list
  //TODO use pattern matching
  def drop[A](n: Int, list: List[A]): List[A] = {
    @annotation.tailrec
    def go(n: Int, innerList: List[A]) : List[A] = {
      if (n == 0) innerList
      else go(n - 1, List.tail(innerList))
    }

    go(n, list)
  }


}

object Ex3d2 extends App {
  val l1 = List(1, 3, 6, 3, 4, 2, -1, 7, 0)
  val n = 3

  println(l1)
  println("size: " + List.size(l1))
  println("tail: " + List.tail(l1))
  println("setHead: " + List.setHead(0, l1))
  println("remove " + n + " elements: " + List.drop(n, l1))
}