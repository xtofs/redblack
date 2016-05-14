package io.xtof

import io.xtof.collections.Tree


object Main {
  def main(args: Array[String]) {


    val t = Tree.from(Seq( 1, 2, 3, 5, 3, 6, 7))

    println(t)
    println(t.size)
    println(t.depth)
    println(t.contains(4))
    println(t.contains(5))
    println(t.show)
  }
}



