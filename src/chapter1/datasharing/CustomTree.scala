package chapter1.datasharing

sealed trait CustomTree[+A]

case class Leaf[A](value: A) extends CustomTree[A]

case class Branch[A](left: CustomTree[A], right: CustomTree[A]) extends CustomTree[A]

object CustomTree {
  def size[A](tree: CustomTree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maxValue(tree: CustomTree[Int]): Int = tree match {
    case Leaf(x) => x
    case Branch(x, y) => maxValue(x) max maxValue(y)
  }

}