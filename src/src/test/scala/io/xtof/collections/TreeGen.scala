package io.xtof.collections

import org.scalacheck.Gen.{listOfN, sized}
import org.scalacheck.{Arbitrary, Gen}

object TreeGen {
  import Tree._

  def genTree[T: Ordering](genT: Gen[T]): Gen[Tree[T]] =
    sized { size =>
      listOfN(size, genT).map( _.foldLeft(Tree.empty[T])((t: Tree[T], x: T) => t.insert(x)))
    }

  implicit val intTrees = Arbitrary(genTree[Int](Gen.choose(1, 99)))
  implicit val doubleTrees = Arbitrary(genTree[Double](Gen.choose(0.0, 1000000.0)))
}
