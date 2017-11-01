import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers

//https://www.youtube.com/watch?v=l5-5w8A-Ong
class ScalaCheckExamples extends FlatSpec with Checkers {

  "string" should "be longer or eq than 0" in {
    check {
      forAll { s: String =>
        s.length >= 0
      }
    }
  }

  "abs" should "be bigger or eq than 0" in {
    check {
      forAll { i: Int =>
        i > Integer.MIN_VALUE ==>
          Math.abs(i) >= 0
      }
    }
  }

  //  "list" should "?" in {
  //    check {
  //      forAll { (l1: List[Int], l2: List[Int]) =>
  //        l1.size < (l1 ::: l2).size
  //      }
  //    }
  //  }

  "list" should "?" in {
    check {
      forAll(
        Gen.nonEmptyListOf(arbitrary[Int]),
        Gen.nonEmptyListOf(arbitrary[Int])
      ) { (l1, l2) =>
        l1.size < (l1 ::: l2).size
      }
    }
  }

  def cappedString: Gen[String] = for {
    c <- alphaUpperChar
    s <- listOf(alphaLowerChar)
  } yield (c :: s).mkString

  "cappedString" should "be cappedString" in {
    check {
      forAll(
        cappedString
      ) { s =>
        s.head.isUpper && s.tail.forall(_.isLower)
      }
    }
  }

  "user" should "be cappedString" in {
    check {

      case class User(name: String, age: Int)

      val genUser: Gen[User] = for {
        name <- cappedString
        age <- Gen.posNum[Int]
      } yield User(name, age)

      implicit val arbUser: Arbitrary[User] = Arbitrary(genUser)

      val user = arbitrary[User].sample
      println(user)

      forAll(
        genUser
      ) { user =>
        user.age > 0
      }
    }
  }
}
