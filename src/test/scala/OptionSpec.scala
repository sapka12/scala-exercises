import org.scalatest.{FlatSpec, Matchers}

class OptionSpec extends FlatSpec with Matchers {

  import fpinscala.errorhandling._
  import fpinscala.errorhandling.Option
  import fpinscala.errorhandling.Option._

  def sqrt(d: Double): Option[Double] =
    if (d < 0) None
    else Some(Math.sqrt(d))

  behavior of "map"

  it should "handle None" in {
    val non: Option[Int] = None
    non.map(_ + 2) shouldBe None
  }

  it should "handle Some" in {
    val non: Option[Int] = Some(2)
    non.map(_ + 2) shouldBe Some(4)
  }

  behavior of "flatMap"

  it should "handle Some" in {
    val valid: Option[Double] = Some(4)
    valid.flatMap(sqrt) shouldBe Some(2)
  }

  it should "handle None" in {
    val non: Option[Double] = None
    non.flatMap(sqrt) shouldBe None
  }

  it should "handle invalid case" in {
    val invalid: Option[Double] = Some(-4)
    invalid.flatMap(sqrt) shouldBe None
  }

  behavior of "orElse"

  it should "handle Some + Some" in {
    val some: Option[Int] = Some(1)
    def some2: Option[Int] = {
      Thread.sleep(500000)
      Some(2)
    }
    some.orElse(some2) shouldBe some
  }

  it should "handle Some + None" in {
    val some: Option[Int] = Some(1)
    val none: Option[Int] = None
    some.orElse(none) shouldBe some
  }

  it should "handle None + Some" in {
    val none: Option[Int] = None
    val some: Option[Int] = Some(1)
    none.orElse(some) shouldBe some
  }

  it should "handle None + None" in {
    val none: Option[Int] = None
    val none2: Option[Int] = None
    none.orElse(none2) shouldBe none
    none.orElse(none2) shouldBe none2
  }

  behavior of "filter"

  it should "handle Some with true predicate" in {
    val some: Option[Int] = Some(1)
    some.filter(_ > 0) shouldBe Some(1)
  }

  it should "handle Some with false predicate" in {
    val some: Option[Int] = Some(-1)
    some.filter(_ > 0) shouldBe None
  }

  it should "handle None" in {
    val some: Option[Int] = None
    some.filter(_ > 0) shouldBe None
    some.filter(_ < 0) shouldBe None
    some.filter(_ == 0) shouldBe None
  }

  behavior of "variance"

  it should "handle None" in {
    variance(Seq()) shouldBe None
    variance(Seq(-5, 1, 8, 7, 2, 11 )) shouldBe Some(28)
  }

  behavior of "map2"

  it should "handle" in {
    map2[Int, String, Double](None, None)(_ + _.toDouble) shouldBe None
    map2[Int, String, Double](Some(2), None)(_ + _.toDouble) shouldBe None
    map2[Int, String, Double](Some(2), Some("3.5"))(_ + _.toDouble) shouldBe Some(5.5)
  }

  behavior of "sequence"

  it should "handle" in {
    sequence[Int](List[Option[Int]](Some(1), Some(2))) shouldBe Some(List(1, 2))
  }

  behavior of "traverse"

  it should "handle valid inputs" in {
    traverse[Int, Double](List[Int](1, 4, 9))(i => sqrt(i.toDouble)) shouldBe Some(List(1, 2, 3))
  }

  it should "handle invalid input" in {
    traverse[Int, Double](List[Int](1, 4, -9))(i => sqrt(i.toDouble)) shouldBe None
  }

  it should "handle empty list" in {
    traverse[Int, Double](List[Int]())(i => sqrt(i.toDouble)) shouldBe Some(List())
  }

}
