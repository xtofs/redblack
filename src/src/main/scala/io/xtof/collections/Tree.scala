package io.xtof.collections

import java.io.StringWriter

import scala.text.Document
import scala.text.Document._


object Tree {

  sealed trait Color

  final case object Red extends Color
  final case object Black extends Color


  sealed trait Tree[+T]

  final case object Empty extends Tree[Nothing]
  final case class Node[T](c: Color, a: Tree[T], x: T, b: Tree[T]) extends Tree[T]



  def empty[T] = Empty.asInstanceOf[Tree[T]]

  implicit class TreeOps[T:Ordering](t: Tree[T]) {

    def size: Int = fold(0)((_, a, _, b) => a + 1 + b)

    def depth: Int = fold(0)((_, a, _, b) => 1 + Math.max(a, b))

    def fold[R](z: R)(f: (Color, R, T, R) => R): R = t match {
      case Empty =>
        z
      case Node(c, a, x, b) =>
        f(c, a.fold(z)(f), x, b.fold(z)(f))
    }

    def show: String = {
      val w = new StringWriter()
      doc(t).format(120, w)
      w.toString
    }

    //    def show: String = {
    //      def shows(indent: Int, t: Tree[T]): String = {
    //        t match {
    //          case Empty =>
    //            ""
    //          case Node(c, a, x, b) =>
    //            s"\n${"  " * indent}${if(c==Red) "R" else "B"} $x${shows(indent + 1, a)}${shows(indent + 1, b)}"
    //        }
    //      }
    //      shows(0, t)
    //    }
  }

  def doc[T](t: Tree[T]) : Document = {
    import scala.text.Document.{empty=>eod,_}

    def col(c: Color): Document = c match {
      case Black => text("B")
      case Red => text("R")
    }

    t match {
      case Empty =>
        text("Ø")
      case Node(c, Empty, x, Empty) =>
        col(c) :: "(" :: text(x.toString) :: ")" :: eod
      case Node(c, a, x, b) =>
        col(c) :: "(" :: nest(2, doc(a) :: "," :/: text(x.toString) :: "," :/: doc(b)) :: ")" :: eod
    }
  }

  import scala.math.Ordering.Implicits._

  implicit class SetOps[T:Ordering](t: Tree[T]) {
    def contains(x: T): Boolean = member(x, t)
    def insert(x: T): Tree[T] = Tree.insert(x, t)
  }

  def from[T: Ordering](seq: Seq[T]): Tree[T] =
    seq.foldLeft(Tree.empty[T])((t: Tree[T], x) => insert(x, t))

  def member[T: Ordering](x: T, tree: Tree[T]): Boolean = {

    tree match {
      case Empty =>
        false

      case Node(_, a, y, b) if x < y =>
        member(x, a)

      case Node(_, a, y, b) if x == y =>
        true

      case Node(_, a, y, b) if x > y =>
        member(x, b)
    }
  }


  def insert[T: Ordering](x: T, t: Tree[T]): Tree[T] = {
    mkBlack(insert0(x, t))
  }

  private def insert0[T: Ordering ](x:T, t: Tree[T]): Node[T] = t match {
    case Empty =>
      Node(Red, Empty, x, Empty)

    case Node(col, a, y, b) if x < y =>
      balance(col, insert0(x, a), y, b)

    case Node(col, a, y, b) if x == y =>
      Node(col, a, y, b)

    case Node(col, a, y, b) if x > y =>
      balance(col, a, y, insert0(x, b))
  }

  def balance[T](col: Color, a: Tree[T], x: T, b: Tree[T]): Node[T] = {
    Node(col, a, x, b) match {
      case Node(Black, Node(Red, Node(Red, a, x, b), y, c), z, d) =>
        Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))

      case Node(Black, Node(Red, a, x, Node(Red, b, y, c)), z, d) =>
        Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))

      case Node(Black, a, x, Node(Red, Node(Red, b, y, c), z, d)) =>
        Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))

      case Node(Black, a, x, Node(Red, b, y, Node(Red, c, z, d))) =>
        Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))

      case n =>
        n
    }
  }

  private def mkBlack[T](node : Node[T]): Node[T] = node match {
    case Node(_, a, x, b) =>  Node(Black, a, x, b)
  }
}
