import org.scalatest.{FlatSpec, Matchers}

class OptionSpec extends FlatSpec with Matchers {

  import fpinscala.errorhandling._
  import fpinscala.errorhandling.Option
  import fpinscala.errorhandling.Option._

  behavior of "map"

  it should "handle None" in {
    val non: Option[Int] = None
    non.map(_ + 2) shouldBe None
  }

  it should "handle Some" in {
    val non: Option[Int] = Some(2)
    non.map(_ + 2) shouldBe Some(4)
  }

}
