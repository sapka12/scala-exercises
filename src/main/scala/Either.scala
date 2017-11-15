package fpinscala.errorhandling

import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(a) => Left(a)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(_) => this
    case Left(_) => b
  }


//  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
//    aa <- a
//    bb <- b
//  } yield f(aa, bb)

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
//    this.flatMap{aa =>
//      b.map(bb => f(aa, bb))
//    }
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)

  }
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {

  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    List.foldRightViaFoldLeft[A, Either[E, List[B]]](es, Right(Nil))(f(_).map2(_)(Cons(_, _)))

//    es match {
//    case Nil => Right(Nil)
//    case Cons(a, as) =>
//      val either = f(a)
//      val next = traverse(as)(f)
//
//      either.map2(next)(Cons(_, _))
//
//      for {
//        aa <- either
//        list <- next
//      } yield Cons(aa, list)
//  }


//  def sequence[A](oas: List[Option[A]]): Option[List[A]] = List
//    .foldRightViaFoldLeft[Option[A], Option[List[A]]](oas, Some(List())) {
//    case (elem, elements) => for {
//      as <- elements
//      a <- elem
//    } yield Cons(a, as)
//  }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    List.foldRightViaFoldLeft[Either[E,A],Either[E, List[A]]](es, Right(Nil)){
        for {
          ax <- _
          aax <- _
        } yield Cons(ax, aax)
    }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}

object Exaxmple extends  App {

  def sqrt(d: Double): Either[String, Double] =
    if (d >= 0) Right(Math.sqrt(d))
    else Left(s"$d is less than 0")

  println(Right(1).map(_ * 3))

  val left : Either[Int, Int] = Left(1)

  println(left.map(_ * 3))

  val rightPos: Either[String, Double] = Right(4)
  val leftStr: Either[String, Double] = Left(":(")
  val rightNeg: Either[String, Double] = Right(-1)

  println(rightPos.flatMap(sqrt))
  println(
    for {
      d      <- rightPos
      result <- sqrt(d)
    } yield result
  )
  println(leftStr.flatMap(sqrt))
  println(rightNeg.flatMap(sqrt))

}