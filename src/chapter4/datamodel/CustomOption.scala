package chapter4.datamodel

sealed trait CustomOption[+A] { self =>
  def map[B](f: A => B): CustomOption[B] = self match {
    case CustomNone => CustomNone
    case CustomSome(x) => CustomSome(f(x))
  }

/*  def flatMap[B](f: A => CustomOption[B]): CustomOption[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => CustomOption[B]): CustomOption[B]
  def filter(f: A => Boolean): CustomOption[A]*/
}

case class CustomSome[A](get: A) extends CustomOption[A]

case object CustomNone extends CustomOption[Nothing]

object CustomOption {
  def mean(xs: Seq[Double]): CustomOption[Double] = {
    if (xs == null || xs.isEmpty) CustomNone
    else CustomSome(xs.sum / xs.length)
  }
}