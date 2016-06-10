package io.xtof.collections

import io.xtof.collections.Tree.{Black, Color, Empty, Node, Red, Tree}
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties


object TreeSpecification extends Properties("Tree") {

  import TreeGen._

  // https://wiki.rice.edu/confluence/download/attachments/2761212/Okasaki-Red-Black.pdf

  property("No tree has red node with a red parent Node") = {
    forAll { (t: Tree[Int]) =>
      !hasRepeatedColor(Red, t)
    }
  }

  property("every path from root to leaf contains the same number of black nodes") = {
    forAll { (t: Tree[Int]) =>
      // println(t.show)

      val ps = paths(t)
      // println(ps)

      // count number of Black in each path
      val counts = ps.map(_.count(_ == 'B'))

      // check that number of blacks is the for all paths
      counts.distinct.size <= 1
    }
  }

  private def hasRepeatedColor[T](c: Color, t: Tree[T]): Boolean =
    t match {
      case Empty =>
        false
      case Node(`c`, Node(`c`, _, _, _), _, _) =>
        true
      case Node(`c`, _, _, Node(`c`, _, _, _)) =>
        true
      case Node(_, a, _, b) =>
        hasRepeatedColor(c, a) || hasRepeatedColor(c, b)
    }

  private def paths[T](t: Tree[T]): Seq[String] = {
    def id(c: Color) = c match {
      case Red => "R"
      case Black => "B"
    }

    t match {
      case Empty =>
        Seq("B")
      case Node(c, Empty, x, b) =>
        paths(b).map(p => id(c) + p)
      case Node(c, a, x, Empty) =>
        paths(a).map(p => id(c) + p)
      case Node(c, a, x, b) =>
        paths(a).map(p => id(c) + p) ++ paths(b).map(p => id(c) + p)
    }
  }
}
