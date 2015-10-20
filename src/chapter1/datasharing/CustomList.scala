package chapter1.datasharing

sealed trait CustomList[+A]

case class Cons[+A](head: A, tail: CustomList[A]) extends CustomList[A]

case object Nil extends CustomList[Nothing]

object CustomList {
  //returns size of list
  def size[A](list: CustomList[A]): Integer = {
    @annotation.tailrec
    def go(n: Integer, innerList: CustomList[A]): Integer = innerList match {
      case Nil => n
      case Cons(_, xs) => go(n + 1, xs)
    }
    go(0, list)
  }

  def apply[A](as: A*): CustomList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  //returns a tail of
  def tail[A](list: CustomList[A]): CustomList[A] = list match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(_, xs) => xs
  }

  //replace first element of collection
  def setHead[A](a: A, list: CustomList[A]): CustomList[A] = list match {
    case Nil => CustomList(a)
    case Cons(_, xs) => Cons(a, xs)
  }

  //removes n first elements of list
  //TODO use pattern matching
  def drop[A](n: Int, list: CustomList[A]): CustomList[A] = {
    @annotation.tailrec
    def go(n: Int, innerList: CustomList[A]) : CustomList[A] = {
      if (n == 0) innerList
      else go(n - 1, CustomList.tail(innerList))
    }

    go(n, list)
  }

  //removes elements from the List prefix as long as they match a predicate.
  def dropWhile[A](l: CustomList[A], f: A => Boolean): CustomList[A] = {
    @annotation.tailrec
    def loop(innerList: CustomList[A]): CustomList[A] = innerList match {
      case Cons(x, xs) =>
        if (f(x)) loop(xs)
        else Cons(x, xs)
      case Nil => Nil
    }
    loop(l)
  }

  def dropWhile2[A](l: CustomList[A])(f: A => Boolean): CustomList[A] = {
    l match {
      case Cons(x, xs) if f(x) => dropWhile2(xs)(f)
      case _ => l
    }
  }

  // adds all the elements of one list to the end of another  !!!
  def append[A](a1: CustomList[A], a2: CustomList[A]): CustomList[A] =
    a1 match {
      case Nil => a2
      case Cons(x, xs) => Cons(x, append(xs, a2))
    }


  def init[A](l: CustomList[A]): CustomList[A] =
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

  /////////////////////////////////////////////////////////////////////////
  def foldRight[A,B](list: CustomList[A], z: B)(f: (A, B) => B): B =
    list match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  //tail-recursive fold-left
  @annotation.tailrec
  def foldLeft[A, B](list: CustomList[A], z: B)(f: (B, A) => B): B = list match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum2(ns: CustomList[Int]) : Int =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: CustomList[Double]) : Double =
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: CustomList[A]): Int = {
    foldRight(as, 0)((_, count) => count + 1)
  }

  def length2[A](as: CustomList[A]): Int = {
    foldLeft(as, 0)((count, _) => count + 1)
  }

  def reverse[A](in: CustomList[A]): CustomList[A] =
    foldLeft(in, CustomList[A]())((x, y) => Cons(y, x))

  def foldRightViaFoldLeft[A,B](list: CustomList[A], z: B)(f: (A,B) => B): B =
    foldLeft(list, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

  def foldLeftViaFoldRight[A,B](list: CustomList[A], z: B)(f: (B,A) => B): B =
    foldRight(list, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

  def append2[A](first: CustomList[A], second: CustomList[A]) : CustomList[A] = {
    foldRight(first, second)(Cons(_, _)) // or   (x, y) => Cons(x, y)
  }

  def concat[A](in: CustomList[CustomList[A]]): CustomList[A] = {
    foldLeft(in, CustomList[A]())(append2(_, _)) // or (x, y) => append2(x, y)
  }

  def transform(in: CustomList[Int]): CustomList[Int] = {
    foldRight(in, CustomList[Int]())((x, y) => Cons(x + 1, y))
  }

  def map[A,B](as: CustomList[A])(f: A => B): CustomList[B] = {
    foldRight(as, CustomList[B]())((x, y) => Cons(f(x), y))
  }

  def filter[A](as: CustomList[A])(f: A => Boolean): CustomList[A] = {
    def loop(list: CustomList[A], out: CustomList[A]) : CustomList[A] = {
      list match {
        case Cons(x, xs) =>
          if (f(x)) loop(xs, Cons(x, out))
          else  loop(xs, out)
        case Nil => out
      }
    }

    loop(reverse(as), CustomList[A]())
  }

  def filter2[A](as: CustomList[A])(f: A => Boolean): CustomList[A] = {
    foldRight(as, CustomList[A]())((x, y) => if (f(x)) Cons(x, y) else y)
  }

  def flatMap[A,B](as: CustomList[A])(f: A => CustomList[B]): CustomList[B] = {
    concat(foldRight(as, CustomList[CustomList[B]]())((x, y) => Cons(f(x), y)))
  }

  //via flatMap
  def filter3[A](list: CustomList[A])(f: A => Boolean): CustomList[A] = {
    flatMap(list)((x) => if (f(x)) CustomList[A](x) else Nil)
  }

  def zipWith[A](first: CustomList[A], second: CustomList[A])(f: (A, A) => A): CustomList[A] = (first, second) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(f(x1, x2), zipWith(xs1, xs2)(f))
  }


  def hasSubsequence[A](sup: CustomList[A], sub: CustomList[A]): Boolean = {
    @annotation.tailrec
    def find(list: CustomList[A], subList: CustomList[A], stepsLeft: Integer) : Boolean = {
      @annotation.tailrec
      def run[A](innerList: CustomList[A], sequence: CustomList[A], flag: Boolean): Boolean = {
        (innerList, sequence) match {
          case (Cons(x1, _), Cons(x2, Nil)) => x1 == x2 && flag
          case (Cons(x1, xs1), Cons(x2, xs2)) =>
            if (x1 == x2 && flag) run(xs1, xs2, flag)
            else false
        }
      }

      (list, subList) match {
        case (_, Nil) => true
        case (Nil, _) => false
        case (Cons(x, xt), Cons(y, yt)) =>
          if (stepsLeft >= 0)
            if ((x == y) && run(list, subList, true)) true
            else find(xt, subList, stepsLeft - 1)
          else
            run(list, subList, true)
      }
    }

    find(sup, sub, size(sup) - size(sub))
  }

  @annotation.tailrec
  def hasSubsequence2[A](l: CustomList[A], sub: CustomList[A]): Boolean = {
    // if subsequence is empty
    if(isEmpty(sub)) return false

    //head of subsequence structure
    val hd = head(sub)

    //checking sequences equality
    //note: sequences are considered to be equal, if
    @annotation.tailrec
    def eqCheck(a: CustomList[A], b: CustomList[A]): Boolean = (a, b) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(h1, t1), Cons(h2, t2)) => if(h1 == h2) eqCheck(t1, t2) else false
    }

    dropWhile2(l)(_ == hd) match {
      case Nil => false
      case Cons(h, t) => {
        if(eqCheck(Cons(hd, Cons(h, t)), sub)) true else hasSubsequence2(t, sub)
      }
    }
  }

  def head[A](l: CustomList[A]): A = {
    l match {
      case Nil => sys.error("no head")
      case Cons(h, t) => h
    }
  }

  def isEmpty[A](l: CustomList[A]): Boolean = l match {
    case Nil => true
    case Cons(h, t) => false
  }
}