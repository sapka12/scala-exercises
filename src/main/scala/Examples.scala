import java.io.File
import scala.concurrent._

object Examples extends App {

  def immutable(): String = {
    val thisIsImmutable = "Cannot change"

    var mutable = "We DO NOT like this"
    mutable = "because we can change its value"

    //possible to use "return keyword" and/or semicolon
    return thisIsImmutable;
  }

  //but

  def typeInference() = {
    "the last value in an expression can be the return value without semicolon" +
      "and Scala knows the return type, do not need to declare it"
  }


  /*
  expression oriented
   */


  def patternMatching(): Unit = {

    val x: Any = 1

    println {
      x match {
        case s: String if s.isEmpty => s"$s is an empty String"
        case s: String => s"$s is a String"
        case 2 => "2"
        case i: Int if (i % 2) == 1 => "%1"
        case _ => "i do not care"
      }
    }

    sealed trait Animal {
      val name: String
    }

    case class Dog(name: String) extends Animal
    case class Cat(name: String) extends Animal

    val dog: Animal = Dog("Beethoven")

    dog match {
      case Cat(_) => println("this is a cat")
      case a: Animal => println(a.name)
    }

  }


  def laziness() = {

    val x = 3

    lazy val y = {
      println("lazy y init")
      x + 1
    }

    lazy val z = {
      println("lazy z init")
      x + y
    }

    println("breakpoint 0")
    val z1 = z
    println(s"breakpoint 1 --- z1: $z1")
    val y2 = y
    println(s"breakpoint 2 --- y2: $y2")

  }

  //https://alvinalexander.com/source-code/scala/simple-scala-call-name-example
  def callByName() = {

    def time(): Long = {
      println("Entered time() ...")
      System.nanoTime
    }

    // uses a by-name parameter here
    def exec(t: => Long) = {
      println("Entered exec, calling t ...")
      println("t = " + t)
      println("Calling t again ...")
      t
    }

    println(s"time before: ${time()}")
    println(exec(time()))
  }

  def futures() = {
    implicit val ec = ExecutionContext.Implicits.global

    def longCall(x: String, t: Long) = {
      Thread.sleep(t)
      s"$x was waiting for $t ms"
    }

    val f1 = Future(longCall("f1", 100))

    f1.foreach(println)

    val f2 = Future(longCall("f2", 110))
    val f3 = Future(longCall("f3", 120))

    val futures = for {
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

    //    futures.foreach(println)

    Thread.sleep(1000)
  }

  def curry() = {

    def mod0(dividend: Int, divisor: Int): Boolean = dividend % divisor == 0

    def even(a: Int): Boolean = mod0(a, 2)

    //with 2 parameterlists
    def mod0_v2(divisor: Int)(dividend: Int): Boolean = dividend % divisor == 0

    val even_v2: Int => Boolean = mod0_v2(2)

    (1 to 10).foreach(i =>
      println(s"$i : ${even_v2(i)}")
    )

    //    val range: Range = 0.until(10)
  }

  def companionObject() = {

    object Factory {
      def apply(a: Int, b: String): Factory = new Factory(a, b)
    }

    class Factory(val a: Int, val b: String) {
      def c(d: Int) = a + d

      val e = b.length
    }

    val f = Factory(1, "asdf")

    println(s"{f.a}   : ${f.a}")
    println(s"{f.b}   : ${f.b}")
    println(s"{f.c(8)}: ${f.c(8)}")
    println(s"{f.e}   : ${f.e}")
  }

  def operators() = {

    implicit class RichInt(i: Int) {

      val notEq: (Int, Int) => Boolean = _ != _

      def =/=(i2: Int): Boolean = notEq(i, i2)
    }

    println("4 =/= 4: " + (4 =/= 4))
    println("4 =/= 5: " + (4 =/= 5))

    // +unary


    implicit class RichFile(f: File) {
      def /(folderName: String): File = {
        new File(f.getAbsolutePath + File.separator + folderName)
      }
    }

    val f: File = new File("c:\\folder") / "subfolder"

    println(f.getAbsolutePath)

  }

//  immutable()
//  operators()
//  patternMatching()
//  laziness()
//  callByName()
//  futures()
//  curry()
//  companionObject()
//  operators()

}