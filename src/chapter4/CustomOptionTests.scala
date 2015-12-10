package chapter4

import chapter1_3.datasharing.CustomList
import chapter4.datamodel.{CustomNone, CustomSome, CustomOption}

object CustomOptionTests extends App {
  val seq1 = Seq(1.0, 2.0, 5.0)
  val seq2 = Seq[Double](7, 7, 7, 7, 7)
  val num = -2.0

  println(CustomOption.mean(seq1))
  println(CustomOption.mean(null))

  val val1 = CustomSome(5)
  val val2 = CustomNone

  println("map:")
  println(val1.map(2 * _))
  //println(val2.map(2 * _))

  println("flatMap:")
  println(val1.flatMap(x => CustomSome(x + 2)))
  println(val2.flatMap(x => CustomSome(x)))
  println(val2.flatMap(x => CustomNone))

  println("getOrElse:")
  println(val1.getOrElse("empty"))
  println(val2.getOrElse("empty"))

  println("orElse:")
  println(val1.orElse(CustomSome("test")))
  println(val2.orElse(CustomSome("test")))

  println("filter:")
  println(val1.filter(x => x != 5))

  println("variance:")
  println(CustomOption.variance(seq1))
  println(CustomOption.variance(seq2))

  println("lift:")
  val abs:  Double => Double = math.abs
  val absWithOption: CustomOption[Double] => CustomOption[Double] = CustomOption.lift(math.abs)
  println(abs(num))
  println(absWithOption(CustomSome(num)))

  println("sequence:")
  val list1 = CustomList[CustomOption[Int]](CustomSome(1), CustomSome(0), CustomSome(10), CustomSome(-1))
  println(CustomOption.sequence(list1))

  val list2 = CustomList[CustomOption[Int]](CustomSome(1), CustomNone, CustomSome(0), CustomSome(-1))
  println(CustomOption.sequence(list2))

  val list3 = CustomList()
  println(CustomOption.sequence(list3))
}