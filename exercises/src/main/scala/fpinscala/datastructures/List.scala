package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // Exercise 2
  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) => t
    case Nil => null
  }

  // Exercise 3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, t) => Cons(h, t)
    case Nil => Nil
  }

  // Exercise 4
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else l match {
      case Cons(_, t) => drop(t, n - 1)
      case Nil => null
    }

  // Exercise 5
  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  // Exercise 6
  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
    case Nil => null
  }

  // Exercise 9
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, res) => res + 1)

  // Exercise 10
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
    case Nil => z
  }

  // Exercise 11
  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) = foldLeft[Double, Double](ns, 1)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((res, _) => res + 1)

  // Exercise 12
  def reverse[A](l: List[A]) = foldLeft(l, Nil: List[A])((res, h) => Cons(h, res))

  // Exercise 13
  def reverseViaFoldLeft[A](l: List[A]) = foldLeft(l, Nil: List[A])((res, h) => Cons(h, res))

  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverseViaFoldLeft(l), z)((b, a) => f(a, b))

  def :+[A](l: List[A], a: A) = foldRight(l, Cons(a, Nil))(Cons(_, _))

  def reverseViaFoldRight[A](l: List[A]) = foldRight(l, Nil: List[A])((a, res) => :+(res, a))

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverseViaFoldRight(l), z)((a, res) => f(res, a))

  def foldLeft3[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (res: B) => res)((a, lazyRes) => res => lazyRes(f(res, a)))(z)

  def foldRight3[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (res: B) => res)((lazyRes, a) => res => lazyRes(f(a, res)))(z)

  // Exercise 14
  def append2[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  // Exercise 15
  def flatten[A](ll: List[List[A]]) = foldRight(ll, Nil: List[A])(append2)

  // Exercise 16
  def succ(ns: List[Int]) = foldRight(ns, Nil: List[Int])((h, t) => Cons(h + 1, t))

  // Exercise 17
  def stringifyAll(ds: List[Double]) =
    foldRight(ds, Nil: List[String])((d, t) => Cons(d.toString, t))

  // Exercise 18
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((a, t) => Cons(f(a), t))

  // Exercise 19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)
}
