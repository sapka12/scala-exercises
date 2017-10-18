sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(x) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(x) => 1
    case Branch(left, right) => (depth(left) max depth(right)) + 1
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(x) => f(x)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _ + 1)

  def maximumViaFold(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ max _ + 1)

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](t)(a => Leaf(f(a)))(Branch(_, _))

  //TODO
  def max[A](t: Tree[A])(gt: (A, A) => Boolean): A = ???

  //TODO
  def sort[A](t: Tree[A])(implicit gt: (A, A) => Boolean): Tree[A] = ???

}

object TreeTest extends App {

  import Tree._

  val small = Branch(Leaf(4), Leaf(5))
  val medium = Branch(Branch(Leaf(10), Leaf(12)), Branch(Leaf(4), Leaf(5)))

  println(sizeViaFold(small))
  println()
  println(maximum(medium))
  println(maximumViaFold(medium))
  println()
  println(depth(medium))
  println(depthViaFold(medium))
  println(depthViaFold(medium))

  //test sort
  implicit val gt:(Int, Int) => Boolean = _ > _

  assert(sort(
    Branch(Leaf(4), Leaf(5))) ==
    Branch(Leaf(4), Leaf(5)))
  assert(sort(
    Branch(Leaf(5), Leaf(4))) ==
    Branch(Leaf(4), Leaf(5)))
  assert(sort(
    Branch(Leaf(5), Leaf(4))) ==
    Branch(Leaf(4), Leaf(5)))
  assert(sort(
    Branch(Branch(Leaf(10), Leaf(12)), Branch(Leaf(4), Leaf(5)))) ==
    Branch(Branch(Leaf(4), Leaf(5)), Branch(Leaf(10), Leaf(12))))
  assert(sort(
    Branch(Branch(Branch(Branch(Leaf(3), Leaf(4)), Branch(Leaf(2), Leaf(1))), Leaf(12)), Branch(Leaf(4), Leaf(5)))) ==
    Branch(Branch(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))), Leaf(4)), Branch(Leaf(5), Leaf(12))))
}
