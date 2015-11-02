package chapter4.datamodel

import chapter1_3.datasharing.{Cons, Nil, CustomList}

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
  def mean(xs: Seq[Double]): CustomOption[Double] =
    if (xs == null || xs.isEmpty) CustomNone
    else CustomSome(xs.sum / xs.length)


  /* The variance function in terms of flatMap. If the mean of a sequence is m,
  the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.
  See the definition of variance on Wikipedia (http://mng.bz/0Qsr).
  As the implementation of variance demonstrates, with flatMap we can construct a
  computation with multiple stages, any of which may fail, and the computation will
  abort as soon as the first failure is encountered, since CustomNone.flatMap(f) will immediately
  return CustomNone, without running f.*/
  //TODO need to sort out
  def variance(xs: Seq[Double]): CustomOption[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  //turns a function f of type A => B into a function of type Option[A] => Option[B]
  def lift[A,B](f: A => B): CustomOption[A] => CustomOption[B] = _ map f

  //combines two CustomOption values using a binary function; if either CustomOption value is CustomNone, then the return value is too.
  def map2[A,B,C](a: CustomOption[A], b: CustomOption[B])(f: (A, B) => C): CustomOption[C] = (a, b) match {
    case (CustomSome(x), CustomSome(y)) => CustomSome(f(x, y))
    case _ => CustomNone
  }

  //TODO need to sort out
  def map3[A,B,C](a: CustomOption[A], b: CustomOption[B])(f: (A, B) => C): CustomOption[C] =
    a flatMap (x => b map (y => f(x, y)))

  def map31[A,B,C](a: CustomOption[A], b: CustomOption[B])(f: (A, B) => C): CustomOption[C] =
    for {
      x <- a
      y <- b
    } yield f(x, y)

/*  def map1[A,B](a: CustomOption[A])(f: A => B): CustomOption[B] =
    a map f

  def map4[A,B,C,D](a: CustomOption[A], b: CustomOption[B], c: CustomOption[C])(f: (A, B, C) => D): CustomOption[D] =
    a flatMap (x => b flatMap (y => c map (z => f(x,y,z)))) */

/*  def sequence[A](a: CustomList[CustomOption[A]]): CustomOption[CustomList[A]] = {
    def loop(list: CustomList[CustomOption[A]]): CustomList[A] = list match {
      case Nil => Nil
      case Cons(optX, optXS) => (optX, optXS) match {
        case (CustomNone, _) => Nil
        case (CustomSome(h), CustomSome(t)) => Cons(h, loop(optXS))
      }
    }

    loop(a) match {
      case Nil => CustomNone
      case Cons(h, t) => CustomSome(Cons(h, t))
    }
  }*/
}