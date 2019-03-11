package cosc250.recursion

import org.scalatest._

import scala.collection.immutable.TreeSet


/**
  * This is a specification file for ScalaTest. It's a set of unit tests written in a way that's designed to be
  * read easily.
  */
class TailRecursiveMadnessSpec extends FlatSpec with Matchers {

  import TailRecursiveMadness._

  "The tests" should "run" in {
    true should be (true)
  }

  "NonTail" should "calculate triangular numbers" in {
    import NonTail._

    triangular(1) should be (1)
    triangular(2) should be (3)
    triangular(3) should be (6)
    triangular(4) should be (10)
  }

  it should "calculate factorials" in {
    import NonTail._

    factorial(1) should be (1)
    factorial(2) should be (2)
    factorial(3) should be (6)
    factorial(4) should be (24)
  }

  it should "calculate Pascal's triangle" in {
    import NonTail._

    pascal(0) should be (List(1))
    pascal(1) should be (List(1, 1))
    pascal(2) should be (List(1, 2, 1))
    pascal(3) should be (List(1, 3, 3, 1))
    pascal(4) should be (List(1, 4, 6, 4, 1))
  }

  it should "calculate the Fibonacci sequence" in {
    import NonTail._

    fibonacci(0) should be (0)
    fibonacci(1) should be (1)
    fibonacci(2) should be (1)
    fibonacci(3) should be (2)
    fibonacci(4) should be (3)
    fibonacci(5) should be (5)
    fibonacci(6) should be (8)
    fibonacci(7) should be (13)
    fibonacci(8) should be (21)
    fibonacci(9) should be (34)
    fibonacci(10) should be (55)
  }

  "Tail" should "calculate triangular numbers" in {
    import Tail._

    triangular(1) should be (1)
    triangular(2) should be (3)
    triangular(3) should be (6)
    triangular(4) should be (10)
  }

  it should "calculate factorials" in {
    import Tail._

    factorial(1) should be (1)
    factorial(2) should be (2)
    factorial(3) should be (6)
    factorial(4) should be (24)
  }

  it should "calculate Pascal's triangle" in {
    import Tail._

    pascal(0) should be (List(1))
    pascal(1) should be (List(1, 1))
    pascal(2) should be (List(1, 2, 1))
    pascal(3) should be (List(1, 3, 3, 1))
    pascal(4) should be (List(1, 4, 6, 4, 1))
  }

  it should "calculate the Fibonacci sequence" in {
    import Tail._

    fibonacci(0) should be (0)
    fibonacci(1) should be (1)
    fibonacci(2) should be (1)
    fibonacci(3) should be (2)
    fibonacci(4) should be (3)
    fibonacci(5) should be (5)
    fibonacci(6) should be (8)
    fibonacci(7) should be (13)
    fibonacci(8) should be (21)
    fibonacci(9) should be (34)
    fibonacci(10) should be (55)
  }
}
