import org.scalacheck.Prop.forAll
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers

class EitherSpec extends FlatSpec with Checkers {

  behavior of "either"

  it should "???" in {
    check {
      forAll { e: Either[String, Int] =>
        ???
      }
    }
  }
}
