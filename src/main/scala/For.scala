import scala.concurrent.{Await, ExecutionContext, Future}

/*
https://www.youtube.com/watch?v=WDaw2yXAa50

Philosophy of FOR
- work on what's inside
- let what's outside take care of itself
- what's my happy case
- sad case decided by container
- my logic matters, context -> not yet
 */
object For extends App {


  def fun1() = {
    val range = (1 to 3) map (_ * 3)

    val xy = for {
      i <- range
      j <- range
      if (i % 2 == 0)
    //          if (2 / 0 == 0)
    } yield (i, j)

    assert(xy ==
      Vector(
        (6, 3),
        (6, 6),
        (6, 9)
      )
    )

    val xy3 = range.map(i =>
      range.map(j =>
        (i, j)
      ).filter {
        case (i, j) => i % 2 == 0
      }
    ).flatten

    assert(xy == xy3)

    val xy2 = range.flatMap(i =>
      range
        .map(j =>
          (i, j)
        )
        .filter { case (x, y) =>
          x % 2 == 0
        }
    )

    assert(xy == xy2)
  }

  fun1()

  def collections2() = {

    case class X(a: Int, b: Int, aXb: Int)

    val range = (1 to 3) map (_ * 3)

    val xy = for {
      i <- range
      j <- range
      if (i % 2 == 0)
    } yield X(i, j, i * j)

    assert(xy ==
      Vector(
        X(6, 3, 18),
        X(6, 6, 36),
        X(6, 9, 54)
      )
    )

    val xy3 = range.map(i =>
      range.map(j =>
        (i, j)
      ).filter {
        case (i, j) => i % 2 == 0
      }.map {
        case (i, j) => X(i, j, i * j)
      }
    ).flatten

    assert(xy == xy3)

    val xy2 = range.flatMap(i =>
      range.map(j =>
        (i, j)
      ).filter {
        case (i, j) => i % 2 == 0
      }.map {
        case (i, j) => X(i, j, i * j)
      }
    )

    assert(xy == xy2)
  }

  collections2()

  def options() = {

    val a = 1
    val b = 2
    val c = 3

    val d = a * b * c

    assert(d == 6)

    val oa = Some(1)
    val ob = Some("asdf")
    val oc = Some(3)

    val od = for {
      a <- oa
      b <- ob
      c <- oc
    } yield a * b.size * c

    assert(od == Some(12))

    val od4 = oa.flatMap { a =>
      ob.flatMap { b =>
        oc.map { c =>
          a * b.size * c
        }
      }
    }

    assert(od == od4)

    val od2 = for {
      a <- oa
      b <- None
      b <- oc
    } yield a * b * c

    assert(od2 == None)
  }

  options()

  def futures() = {
    import scala.concurrent.duration._

    implicit val ec = ExecutionContext.Implicits.global

    def longCall(x: String, t: Long) = {
      Thread.sleep(t)
      s"$x was waiting for $t ms"
    }

    val f1 = Future(longCall("f1", 100))
    val f2 = Future(longCall("f2", 110))
    val f3 = Future(longCall("f3", 120))

    val futureResult = for {
      ff1 <- f1
      ff2 <- f2
      ff3 <- f3
      if (ff1.length + ff2.length > 10)
    } yield
      s"""
         |$ff1
         |$ff2
         |$ff3
      """.stripMargin

    //DO NOT DO THIS
    val result = Await.result(futureResult, 1 second)

    assert(result ==
      """
        |f1 was waiting for 100 ms
        |f2 was waiting for 110 ms
        |f3 was waiting for 120 ms
      """.stripMargin)

  }

  futures()

  def x() = {

    val o = for {
      x <- 1 to 3
      y <- 1 to 5

      //inline assignment
      op = 2 * y + x

      if (op % 4 == 0)
    } yield (x, y, op)

    assert(o == Vector(
      (2, 1, 4),
      (2, 3, 8),
      (2, 5, 12)
    ))

  }

  x()

  //either
  //state

  def cannotMix() = {
    val option = Some("a")
    val list = List(1)

    //    it won't be compiled

    //    val useDifferentKindOfMonads = for {
    //      o <- option
    //      l <- list
    //    } yield o.length + l

    //    Error:(_, _) type mismatch;
    //    found   : List[Int]
    //    required: Option[?]
    //    l <- list

//    val optionAsList = for {
//      o <- option.toList
//      l <- list
//    } yield o.length + l
//
//    assert(optionAsList == List(2))
//
//    val none: Option[String] = None
//    val optionAsList2 = for {
//      l <- list
//      o <- none.toList
//    } yield o.length + l

//    assert(optionAsList2 == List())
  }

  cannotMix()
}
