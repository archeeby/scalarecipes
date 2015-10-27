package chapter4

import chapter4.datamodel.CustomOption

object CustomOptionTests extends App {
  println(CustomOption.mean(Seq(1.0, 1.0, 2.0)))
  println(CustomOption.mean(null))

}