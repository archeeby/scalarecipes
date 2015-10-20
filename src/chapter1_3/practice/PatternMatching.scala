package chapter1_3.practice

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

object PatternMatching extends App {
  val list = List(1, 2, 3, 4)
  val doubleList = List(0.4, 0.1, 1.0, 0.9)
  val doubleListWithZero = List(0.4, 0.1, 0.0, 1.0, 0.9)

  println(List.sum(list))

  println(List.product(Nil))
  println(List.product(doubleListWithZero))
  println(List.product(doubleList))

  println(List(1,2,3) match { case _ => 42 })
  println(List(1,2,3) match { case Cons(h,_) => h })
  println(List(1,2,3) match { case Cons(_,t) => t })
  //println(List(1,2,3) match { case Nil => 42 }) -> produces scala.MatchError
}