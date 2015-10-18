package chapter1.datasharing

sealed trait CustomTree[+A]

case class Leaf[A](value: A) extends CustomTree[A]

case class Branch[A](left: CustomTree[A], right: CustomTree[A]) extends CustomTree[A]

object CustomTree {

}