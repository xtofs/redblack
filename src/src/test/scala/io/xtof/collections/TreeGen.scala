package io.xtof.collections

import org.scalacheck.Gen.{listOfN, sized}
import org.scalacheck.{Arbitrary, Gen}

object TreeGen {


//  // http://booksites.artima.com/scalacheck/examples/html/ch06.html
//  def genTree[T](genT: Gen[T]): Gen[Tree[T]] = lzy {
//    oneOf(genEmpty(genT), genNode(genT))
//  }
//
//  def genEmpty[T](genT: Gen[T]): Gen[Tree[T]] =
//    Gen.const( Empty)
//
//  def genNode[T](genT: Gen[T]): Gen[Node[T]] =
//    sized { size =>
//      for {
//        col <- Gen.oneOf(Red, Black)
//        a <- genTree(genT)
//        x <- genT
//        b <- genTree(genT)
//      } yield Node(col, a, x, b)
//    }

  import Tree._

  def genTree[T: Ordering](genT: Gen[T]): Gen[Tree[T]] =
    sized { size =>
      listOfN(size, genT).map( _.foldLeft(Tree.empty[T])((t: Tree[T], x: T) => t.insert(x)))
    }

  implicit val intTrees = Arbitrary(genTree[Int](Gen.choose(1, 99)))
  implicit val doubleTrees = Arbitrary(genTree[Double](Gen.choose(0.0, 1000000.0)))
}
