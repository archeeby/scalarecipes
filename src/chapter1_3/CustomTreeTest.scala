package chapter1_3

import chapter1_3.datasharing.{Branch, CustomTree, Leaf}

object CustomTreeTest extends App {
  val tree = Branch(Branch(Branch(Leaf(8), Leaf(9)), Branch(Leaf(0), Leaf(2))), Branch(Leaf(6), Leaf(1)))

  println(CustomTree.size(tree))
  println(CustomTree.maxValue(tree))
  println(CustomTree.depth(tree))
  println(CustomTree.depth2(tree))
  println(CustomTree.map(tree)(x => "str" + x))

  println(CustomTree.sizeViaFold(tree))
  println(CustomTree.maxValueViaFold(tree))
  println(CustomTree.depthViaFold(tree))
  println(CustomTree.mapViaFold(tree)(x => "str" + x))
}