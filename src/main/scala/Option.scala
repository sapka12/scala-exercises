// source:
// https://github.com/fpinscala/fpinscala/blob/master/exercises/src/main/scala/fpinscala/errorhandling/Option.scala
package fpinscala.errorhandling

import scala.annotation.tailrec
import scala.{Either => _, Option => _, Some => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case _ => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case _ => default
  }

  def flatMap3[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case _ => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)

  def orElse2[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(_) => this
    case _ => ob
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

  def filter2(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch {
      case e: Exception => 43
    } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch {
      case e: Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance0(xs: Seq[Double]): Option[Double] = mean(xs) match {
    case Some(m) =>
      val sum = xs.map(x => ((m - x) * (m - x))).sum
      val vari = sum / xs.size
      Some(vari)
    case None => None
  }

  def variance3(xs: Seq[Double]): Option[Double] = mean(xs).flatMap { mean =>
    Some(
      xs.map(x => ((mean - x) * (mean - x))).sum / xs.size
    )
  }

  def variance(xs: Seq[Double]): Option[Double] =
    for {
      mean <- mean(xs)
    } yield xs.map(x => ((mean - x) * (mean - x))).sum / xs.size

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
    aa <- a
    bb <- b
  } yield f(aa, bb)

  def map2_v2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(aa => b.map(bb => f(aa, bb)))

  def sequence2[A](oas: List[Option[A]]): Option[List[A]] = oas match {
    case Cons(h, t) =>
      h match {
        case Some(a) => sequence(t).map(tt => Cons(a, tt))
        case None => None
      }
    case _ => Some(List())
  }

  def sequence[A](oas: List[Option[A]]): Option[List[A]] = List
    .foldRightViaFoldLeft[Option[A], Option[List[A]]](oas, Some(List())) {
    case (elem, elements) => for {
      as <- elements
      a <- elem
    } yield Cons(a, as)
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sequence(List.map(a)(f))

}
