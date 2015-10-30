package chapter4

import chapter4.datamodel.{CustomNone, CustomSome, CustomOption}

object CustomOptionTests extends App {

  println(CustomOption.mean(Seq(1.0, 2.0, 5.0)))
  println(CustomOption.mean(null))

  val val1 = CustomSome(5)
  val val2 = CustomNone

  println("map:")
  println(val1.map(2 * _))
  //println(val2.map(2 * _))

  println("flatMap:")
  println(val1.flatMap(x => CustomSome(x + 2)))
  println(val1.flatMap(x => CustomNone))
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
}