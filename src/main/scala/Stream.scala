//https://github.com/fpinscala/fpinscala/blob/master/exercises/src/main/scala/fpinscala/laziness/Stream.scala
package fpinscala.laziness

import Stream._

import scala.annotation.tailrec

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

  def toList(): List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList()
  }

  def take(n: Int): Stream[A] = (n, this) match {
    case (0, _) => this
    case (_, Empty) => empty
    case (_, Cons(h, t)) => cons(h(), t().take(n-1))
  }

  def drop(n: Int): Stream[A] = (n, this) match {
    case (0, _) => this
    case (_, Empty) => Empty
    case (_, Cons(_, t)) => t().drop(n-1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => this
  }

  //TODO Cons => cons
  def takeWhileViaFold(p: A => Boolean): Stream[A] = foldRight[Stream[A]](Empty){
    case (a, strA) => if(p(a)) cons(a, strA) else Empty
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) if p(h()) => t().forAll(p)
    case _ => false
  }

  def headOption: Option[A] = this match  {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = foldRight[Stream[B]](empty){
    case (a, bs) => cons(f(a), bs)
  }

  def filter(p: A => Boolean): Stream[A] = foldRight[Stream[A]](empty) {
    case (a, bs) if p(a) => cons(a, bs)
    case (_, bs) => bs
  }

  def append[B>:A](s: => Stream[B]): Stream[B] = foldRight[Stream[B]](s){
    case (a, bs) => cons(a, bs)
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight[Stream[B]](empty) {
    case (a, bs) => f(a).append(bs)
  }

  //GOTO EXERCISE 7

  //EXERCISE 12: Use unfold to implement map, take, takeWhile, zip and zipAll.

//  def map[B](f: A => B): Stream[B] = foldRight[Stream[B]](empty){
//    case (a, bs) => cons(f(a), bs)
//  }

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold[B, Stream[A]](this){s =>
    s match {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold[A, (Int, Stream[A])]((n, this))( s =>
    s match {
      case (0, _) => None
      case (_, Empty) => None
      case (i, Cons(h, t)) => Some(h(), (i-1, t()))
    }
  )

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = ???

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

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
case object Empty extends Stream[Nothing]

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
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  //EXERCISE 8
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  //EXERCISE 9
  def fibs: Stream[Int] = {

    def go(n0: Int, n1: Int): Stream[Int] = Stream.cons(n0, go(n1, n0 + n1))

    go(0, 1)
  }

  //EXERCISE 10
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    case None => empty
  }

  //EXERCISE 11
  val fibsViaUnfold: Stream[Int] = unfold((1,1)){
    case (n0, n1) => Some((n0, (n1, n0 + n1)))
  }

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(i => Some((i, i + 1)))


  def constantUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))
  val onesUnfold: Stream[Int] = constantUnfold(1)

  def hasSubsequence[A](s1: Stream[A], s2: Stream[A]): Boolean =
    s1.tails.exists(_.startsWith(s2))

}

object AppSpec extends App {
  val s1: Stream[Int] = Cons(() => 1, () => Cons(() => 2, () => Cons(() => 3, () => Cons(() => 4, () => Empty))))

  val s2: Stream[Int] = Cons(() => 5, () => Cons(() => 6, () => Cons(() => 7, () => Cons(() => 8, () => Empty))))

  val s3 = s1.flatMap(x => Stream(Seq.fill(x)(x): _*))



  println(s3.toList())

  //println(s3.mapViaUnfold(_ + 1).toList())

  println(s3.takeViaUnfold(3).toList())
  println(Empty.takeViaUnfold(3).toList())
  println(s3.takeViaUnfold(10).toList())

//  println(s.takeWhileViaFold(_ % 2 == 1).toList())
//  println(s.takeWhileViaFold(_ < 3).toList())

  //println(s1.append(s2).toList())
}