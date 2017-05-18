package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](tree: Tree[A])(zf: A => B)(f: (B, B) => B): B = tree match {
    case Leaf(v) => zf(v)
    case Branch(left, right) => f(fold(left)(zf)(f), fold(right)(zf)(f))
  }

  def size2[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(1 + _ + _)

  def maximum2(tree: Tree[Int]): Int = fold(tree)(identity)(_ max _)

  def depth2[A](tree: Tree[A]): Int = fold(tree)(_ => 0)((l, r) => 1 + (l max r))

  def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(x => Leaf(f(x)): Tree[B])((l, r) => Branch(l, r))
}

object TreeMain {
  import Tree._

  def main(args: Array[String]): Unit = {
    val aTree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    println(aTree)
    println(fold(aTree)(Leaf(_): Tree[_])(Branch(_, _)))
    println(map2(aTree)(_ + 1))
  }
}