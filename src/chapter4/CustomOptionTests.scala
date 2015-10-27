package chapter4

import chapter4.datamodel.{CustomSome, CustomOption}

object CustomOptionTests extends App {
  val seq = CustomSome(1.0)

  println(CustomOption.mean(Seq(1.0, 2.0, 5.0)))
  println(CustomOption.mean(null))

  println(seq.map(2 * _))

}