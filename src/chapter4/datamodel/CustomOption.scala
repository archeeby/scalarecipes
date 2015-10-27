package chapter4.datamodel

sealed trait CustomOption[+A]

case class CustomSome[A](get: A) extends CustomOption[A]

case object CustomNone extends CustomOption[Nothing]

object CustomOption {
  def mean(xs: Seq[Double]): CustomOption[Double] = {
    if (xs == null || xs.isEmpty) CustomNone
    else CustomSome[Double](xs.sum / xs.length)
  }
}