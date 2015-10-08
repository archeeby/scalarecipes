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
}

object Ex3d2 extends App {
  val l1 = CustomList(1, 3, 6, 3, 4, 2, -1, 7, 0)
  val l11 = CustomList(1.0, 3.0, 6.0, 3.0, 4.0, 2.0, -1.0, 7.0)
  val n = 3

  println(l1)
  println("size: " + CustomList.size(l1))
  println("tail: " + CustomList.tail(l1))
  println("setHead: " + CustomList.setHead(0, l1))
  println("remove " + n + " elements: " + CustomList.drop(n, l1))

  val l2 = CustomList(0, 1, 2, 3, 4)
  val f = (x : Int) => x != 2
  println("drop while: " + CustomList.dropWhile(l2, f)) //remove elements from left to right until x!=2

  val c1 = CustomList("A", "B", "C")
  val c2 = CustomList("D", "E")
  println("append: " + CustomList.append(c1, c2))
  println("init: " + CustomList.init(c1))

  println("sum2: " + CustomList.sum2(l1))
  println("product2: " + CustomList.product2(l11))
  println("length: " + CustomList.length(l1))
  println("length2: " + CustomList.length(l1))
  println("-----------------------\n")

  val l3 = CustomList(1, 2, 3)
  def func(x : Int, y : Int) : Int = {
    println("x: " + x + ", y: " + y)
    x - y
  }

  println(l3)
  println("foldRightViaFoldLeft: ")
  println("result: " + CustomList.foldRightViaFoldLeft(l3, 0)((x, y) => func(x, y)))

  println("foldLeftViaFoldRight: ")
  println("result: " + CustomList.foldLeftViaFoldRight(l3, 0)((x, y) => func(x, y)))

  println(CustomList.reverse(l3))
  println(CustomList.append2(c1, c2))

  val nestedList = CustomList(CustomList("A", "B", "C"), CustomList("D"), CustomList("E", "F", "G"))
  println(CustomList.concat(nestedList))

  println(CustomList.transform(CustomList(1,2,3)))
}