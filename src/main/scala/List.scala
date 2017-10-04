import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Cons(h, t) => t
    case _ => Nil
  }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, tail(l))

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = if (n == 0) l else drop(tail(l), n - 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if (f(h)) => dropWhile(tail(l), f)
    case _ => l
  }

  //  Returns the list without its last element.
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = l match {
    case Cons(_, t) => 1 + length(t)
    case _ => 0
  }

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(a, as) => foldLeft(as, f(z, a))(f)
  }

  def sum2FL(ns: List[Int]) = foldLeft(ns, 0)((x, y) => x + y)

  def product2FL(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def tailFL[A](l: List[A]): List[A] = foldLeft(l, Nil) { case (b, _) => b }

  @tailrec
  def concat[A](l1: List[A], l2: List[A]): List[A] = if (length(l1) > 0) {

    @tailrec
    def last(l: List[A]): A = l match {
      case Cons(a, Nil) => a
      case Cons(_, t) => last(t)
      case _ => throw new UnsupportedOperationException
    }

    concat(init(l1), Cons(last(l1), l2))
  } else l2

  def reverse0[A](l: List[A]): List[A] = l match {
    case Cons(a, as) => concat(reverse0(as), List(a))
    case _ => Nil
  }

  def reverse[A](l: List[A]): List[A] = foldLeft[A, List[A]](l, Nil) {
    case (as, a) => Cons(a, as)
  }

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft[A, B](reverse(as), z) {
      case (b, a) => f(a, b)
    }

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft[A, List[B]](l, Nil) {
      case (a, b) => Cons(f(a), b)
    }

  //flatten
  def flatten[A](l: List[List[A]]): List[A] = foldLeft[List[A], List[A]](reverse0(l), Nil) {
    case (aggr, elem) => concat(elem, aggr)
  }

  //flatmap
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = flatten(map(l)(f))

  //  toString???

  def fromSeq[A](seq: Seq[A]): List[A] = {
    if (seq.size > 0) Cons(seq.head, fromSeq(seq.tail))
    else Nil
  }

}

object TestList extends App {

  import List._

  assert(init(Nil) ==
    Nil
  )

  assert(init(List(1)) ==
    Nil
  )

  assert(init(List(1, 2)) ==
    List(1)
  )

  assert(init(List(1, 2, 3)) ==
    List(1, 2)
  )

  assert(reverse(List(1, 2, 3)) ==
    List(3, 2, 1)
  )

  assert(reverse0(List(1, 2, 3)) ==
    List(3, 2, 1)
  )

  assert(map(List(1, 2, 3))(_ * 3) ==
    List(3, 6, 9)
  )

  assert(map(List[Int]())(_ * 3) ==
    List()
  )

  assert(flatten(List(List(1, 2, 3), List(4, 5), List(6))) ==
    List(1, 2, 3, 4, 5, 6)
  )

  assert(flatten(List(List(1, 2, 3), List(4, 5), List(), List(6))) ==
    List(1, 2, 3, 4, 5, 6)
  )

  def wordLengths(i: Int): List[String] = {

    def go(n: Int): List[String] =
      if (n > 0) Cons(n.toString, go(n - 1))
      else List()

    reverse0(go(i))
  }

  assert(wordLengths(2) ==
    List("1", "2")
  )

  assert(wordLengths(3) ==
    List("1", "2", "3")
  )

  assert(wordLengths(0) ==
    List()
  )

  assert(length(wordLengths(50)) == 50)

  assert(flatMap(List(2))(wordLengths) ==
    List("1", "2")
  )
  assert(flatMap(List())(wordLengths) ==
    List()
  )

  assert(flatMap(List(0))(wordLengths) ==
    List()
  )

  assert(flatMap(List(0, 0))(wordLengths) ==
    List()
  )

  assert(flatMap(List(0, 1))(wordLengths) ==
    List("1")
  )

  assert(flatMap(List(0, 2))(wordLengths) ==
    List("1", "2")
  )

  assert(flatMap(List(0, 2, 1))(wordLengths) ==
    List("1", "2", "1")
  )

  assert(flatMap(List(0, 2, 1, 3))(wordLengths) ==
    List("1", "2", "1", "1", "2", "3")
  )

  assert(flatMap(List(0, 2, 1, 3, 0, 1))(wordLengths) ==
    List("1", "2", "1", "1", "2", "3", "1")
  )
}