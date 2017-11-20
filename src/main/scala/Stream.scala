//https://github.com/fpinscala/fpinscala/blob/master/exercises/src/main/scala/fpinscala/laziness/Stream.scala
package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = ???

  def drop(n: Int): Stream[A] = ???

  def takeWhile(p: A => Boolean): Stream[A] = ???

  def forAll(p: A => Boolean): Boolean = ???

  def headOption: Option[A] = ???

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  //GOTO EXERCISE 7

  //EXERCISE 12: Use unfold to implement map, take, takeWhile, zip and zipAll.

  //EXERCISE 13 (hard)
  def startsWith[B](s: Stream[B]): Boolean = ???

  //EXERCISE 14
  //implement tails using unfold.
  //So, given Stream(1,2,3), it would return
  //Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream.empty).
  def tails: Stream[Stream[A]] = ???

  //EXERCISE 15 (hard, optional): Generalize tails to the function
  //scanRight, which is like a foldRight that returns a stream of the
  //intermediate results.
  // Stream(1,2,3).scanRight(0)(_ + _).toList ===> List(6,5,3,0)

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  //EXERCISE 7
  def constant[A](a: A): Stream[A] = ???

  //EXERCISE 8
  def from(n: Int): Stream[Int] = ???

  //EXERCISE 9
  val fibs: Stream[Int] = ???

  //EXERCISE 10
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???

  //EXERCISE 11
  val fibsViaUnfold: Stream[Int] = ???
  def fromViaUnfold(n: Int): Stream[Int] = ???
  def constantUnfold[A](a: A): Stream[A] = ???
  val onesUnfold: Stream[Int] = Stream.cons(1, ones)

  def hasSubsequence[A](s1: Stream[A], s2: Stream[A]): Boolean =
    s1.tails.exists(_.startsWith(s2))

}