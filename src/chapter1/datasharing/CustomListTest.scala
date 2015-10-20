package chapter1.datasharing

object CustomListTest extends App {
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
  println(CustomList.map(CustomList(1,2,3))(x => "ITEM" + x.toString))
  println(CustomList.filter(CustomList(1,2,3,4,5,6,7,8,9,10))(x => x % 2 != 0))
  println(CustomList.filter2(CustomList(1,2,3,4,5,6,7,8,9,10))(x => x % 2 != 0))
  println(CustomList.filter3(CustomList(1,2,3,4,5,6,7,8,9,10))(x => x % 2 != 0))
  println(CustomList.flatMap(CustomList(1,2,3))(x => CustomList(x, x, 2 * x)))
  println(CustomList.zipWith(CustomList(1, 2, 3), CustomList(2, 2, 2))(_ + _))

  val sequence = CustomList(1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,3,6,1)
  val subSequence = CustomList(1,2,1,2,1,2,3)
  var t1 = System.currentTimeMillis()
  println(CustomList.hasSubsequence(sequence, subSequence))
  println("first: " + (System.currentTimeMillis() - t1))
  t1 = System.currentTimeMillis()
  println(CustomList.hasSubsequence2(sequence, subSequence))
  println("second: " + (System.currentTimeMillis() - t1))
}