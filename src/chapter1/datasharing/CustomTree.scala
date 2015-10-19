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
    case Branch(left, right) => maxValue(left) max maxValue(right)
  }

  def depth[A](tree: CustomTree[A]): Int = {
    def loop(t: CustomTree[A], n: Int): Int = t match {
      case Leaf(_) => n
      case Branch(left, right) => loop(left, n + 1) max loop(right, n + 1)
    }

    loop(tree, 0)
  }

  def depth2[A](tree: CustomTree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + (depth2(left) max depth2(right))
  }
}