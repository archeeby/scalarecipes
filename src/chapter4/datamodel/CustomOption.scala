package chapter4.datamodel

sealed trait CustomOption[+A] { self =>
  //apply f if the CustomOption is not CustomNone
  def map[B](f: A => B): CustomOption[B] = self match {
    case CustomNone => CustomNone
    case CustomSome(x) => CustomSome(f(x))
  }

  //apply f, which may fail, to the CustomOption if not CustomNone
  def flatMap[B](f: A => CustomOption[B]): CustomOption[B] = self match {
    case CustomNone => CustomNone
    case CustomSome(x) => f(x)
  }

  /* returns the result inside the CustomSome case of the CustomOption,
  * or if the CustomOption is CustomNone, returns the given default value. */
  def getOrElse[B >: A](default: => B): B = self match {
    case CustomNone => default
    case CustomSome(x) => x
  }

  /* returns the list CustomOption if it's defined;
  * otherwise, it returns the second CustomOption */
  def orElse[B >: A](ob: => CustomOption[B]): CustomOption[B] = self match {
    case CustomNone => ob
    case CustomSome(x) => CustomSome(x)
  }

  //converts CustomSome to CustomNone if the value doesn't satisfy f
  def filter(f: A => Boolean): CustomOption[A] = self match {
    case CustomNone => CustomNone
    case CustomSome(x) =>
      if (f(x)) CustomSome(x)
      else CustomNone
  }
}

case class CustomSome[A](get: A) extends CustomOption[A]

case object CustomNone extends CustomOption[Nothing]

object CustomOption {
  def mean(xs: Seq[Double]): CustomOption[Double] = {
    if (xs == null || xs.isEmpty) CustomNone
    else CustomSome(xs.sum / xs.length)
  }
}