package cosc250.recursion

import scala.annotation.tailrec

object TailRecursiveMadness {

  /**
    * Let's start off with some non-tail recursive exercises.
    *
    * Just implement these as ordinary recusions
    */
  object NonTail {

    /**
      * triangular number 4 is 1 + 2 + 3 + 4
      * triangular number n is 1 + 2 + 3 + ... n
      */
    def triangular(n:Int):Int = {
      if (n <= 1) 1 else n + triangular(n - 1)
    }

    /**
      * factorial(4) is 1 * 2 * 3 * 4
      * factorial(n) is 1 * 2 * 3 * ... n
      */
    def factorial(n:Int):Int = {
      ???
    }

    /**
      * The Fibonacci sequence begins
      * 0 1 1 2 3 5 8 13 21 36
      * with fib(n) = fib(n-1) + fib(n-2) for n > 1
      *
      * It gets big very quickly, so I'm using BigInt
      * BigInt(3) produces a BigInt containing 3
      * BigInt(3) + BigInt(2) = BigInt(5)
      * But unlike Int, summing BigInts has no maximum value.
      */
    def fibonacci(n:Int):BigInt = {
      ???
    }

    // Puzzle: try calling it with a large number - say 100. What breaks?
    // (Comment out this line, and then run the tests. The println will be run as part of the object
    // initialisation for the NonTail object)
    // println(fibonacci(100))


    /**
      * Let's do one using Lists
      *
      * Pascal's triangle goes
      *     1
      *    1 1
      *   1 2 1
      *  1 3 3 1
      * 1 4 6 4 1
      *
      * To calculate the next line, take
      * left = 0 +: pascal(n-1)     // +: is append
      * right = pascal(n=1) :+ 0    // :+ is append at the other end
      *
      * That gives the list twice, offset by one.
      * Now add them up element by element to get pascal(n)
      */
    def pascal(n:Int):List[Int] = {
      ???
    }

  }

  object Tail {

    /**
      * Let's now make factorial use an inner tail recursive function
      */
    def triangular(n:Int) = {

      /**
        * The key is realising that as we can't do the calculation on the returned
        * value, so we have to do it on the arguments that are passed in.
        */
      @tailrec
      def innerTri(n:Int, accum:Int = 0):Int = {
        if (n <= 0) accum else innerTri(n - 1, accum + n)
      }

      innerTri(n, 0)
    }

    /**
      * Your turn - try making a tail recursive factorial
      * @param n
      * @return
      */
    def factorial(n:Int):Int = {

      //@tailrec
      def fac(n:Int, accum:Int):Int = {
        ???
      }

      ???
    }


    /**
      * Now for a tail-recursive fibonacci
      *
      */
    def fibonacci(n:Int):BigInt = {

      // For this one, we have to count down but calculate upwards
      // ie, start at n=k but a=0 b=1
      // then n=k-1, a=1 b=1
      // then n=k-2, a=1 b=2
      // then n=k-3, a=2 b=3
      // until you reach n=0
      //@tailrec
      def fibInt(n:Int, a:BigInt=0, b:BigInt=1):BigInt = {
        ???
      }

      ???
    }

    // Try fibonacci(60), but not too large or it will take a really long time.
    // println(fibonacci(60))

    /**
      * Now for a tail recursive Pascal's triangle
      */
    def pascal(n:Int):List[Int] = {

      // I'll let you fill in the middle this time

      ???
    }

  }


}
