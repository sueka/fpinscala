package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  // Exercise 25
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  // Exercise 26
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  // Exercise 27
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  // Exercise 28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // Exercise 29
  def fold[A, B](tree: Tree[A])(init: A => B)(acc: (B, B) => B): B = tree match {
    case Leaf(v) => init(v)
    case Branch(l, r) => acc(fold(l)(init)(acc), fold(r)(init)(acc))
  }

  def size2[A](tree: Tree[A]) = fold(tree)(_ => 1)(_ + _ + 1)

  def miximum2(tree: Tree[Int]) = fold(tree)(identity)(_ max _)

  def depth2[A](tree: Tree[A]) = fold(tree)(_ => 0)((l, r) => (l max r) + 1)

  def map2[A, B](tree: Tree[A])(f: A => B) =
    fold(tree)(v => Leaf(f(v)): Tree[B])((l, r) => Branch(l, r))




}
