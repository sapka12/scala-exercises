import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import scala.{Either => _, Left => _, Option => _, Right => _, _}
import fpinscala.errorhandling.Either
import fpinscala.errorhandling.Left
import fpinscala.errorhandling.Right
import fpinscala.errorhandling.Either._
import fpinscala.errorhandling.List
import fpinscala.errorhandling.List._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Gen.{Parameters, alphaLowerChar, alphaUpperChar, listOf}
object EitherSpec extends Properties("Either") {

  def rightGen: Gen[Right[Int]] = for {
    i <- arbitrary[Int]
  } yield Right(i)

  def leftGen: Gen[Left[String]] = for {
    str <- Gen.alphaLowerStr
  } yield Left(str)

  def eitherGen: Gen[Either[String, Int]] = for {
    bool <- arbitrary[Boolean]
    l <- leftGen
    r <- rightGen
  } yield if(bool) r else l

  implicit lazy val arbLeft: Arbitrary[Left[String]] = Arbitrary(leftGen)
  implicit lazy val arbRight: Arbitrary[Right[Int]] = Arbitrary(rightGen)
  implicit lazy val arbEither: Arbitrary[Either[String, Int]] = Arbitrary(eitherGen)

  property("map left") = forAll{l: Left[String] =>
    l.map(_.toString) == l
  }

  property("map right") = forAll{r: Right[Int] =>
    r.map(_ - 1) == Right(r.get - 1)
  }

  property("map either") = forAll{ei: Either[String, Int] =>

    val there = ei.map(_ - 1)
//    val there = ei.map(_ * 3)

    val forth = there.map(_ + 1)
//    val forth = there.map(_ / 3)

//    println(ei)
//    println(there)
//    println(forth)
//    println()

    ei == forth
  }

  property("map2 either") = forAll{(e1: Either[String, Int], e2: Either[String, Int]) =>

    def sumStr(i1: Int, i2: Int) = s"sum=${i1 + i2}"

    val mapped = e1.map2(e2)(sumStr)

    val expected = (e1, e2) match {
      case (Right(r1), Right(r2)) => Right(sumStr(r1, r2))
      case (Right(_), Left(l)) => Left(l)
      case (Left(l), _) => Left(l)
    }

    expected == mapped
  }


}
