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

  def map[A, B](tree: CustomTree[A])(f: A => B): CustomTree[B] = tree match {
    case Leaf(x) => Leaf(f(x))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](tree: CustomTree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(x) => f(x)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def sizeViaFold[A](tree: CustomTree[A]): Int =
    fold(tree)(x => 1)(1 + _ + _)

  def maxValueViaFold(tree: CustomTree[Int]): Int =
    fold(tree)(x => x)(_ max _)

  def depthViaFold[A](tree: CustomTree[A]): Int = {
    fold(tree)(x => 0)(1 + _ max _)
  }

  def mapViaFold[A, B](tree: CustomTree[A])(f: A => B): CustomTree[B] =
    fold(tree)(x => Leaf[B](f(x)): CustomTree[B])(Branch(_, _))

}