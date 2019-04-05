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
			if(n <= 1) 1 else n * factorial(n - 1)
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
			if(n <= 0) 0
			else if(n == 1) 1
			else fibonacci(n - 2) + fibonacci(n - 1)

		} //note: the tail recursive version isn't as inefficient

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
		//note: This is the school solution
		def pascal(n:Int):List[Int] = {

			// You'll need a function that can sum tuples in a list
			// Let's define that recursively for you
			def sumPairs(l:List[(Int, Int)]):List[Int] = {

				l match {
					case Nil => Nil
					case (a,b) :: tail => (a + b) :: sumPairs(tail)
				}
			}

			// Now write the body of pascal(n)
			if(n <= 0) {
				List(1)
			}
			else {
				val previous = pascal(n-1)
				val left = 0 +: previous
				val right = previous :+ 0
				val paired = left.zip(right)
				sumPairs(paired)
				/*val p = pascal(n - 1)
				val paired = (0 +: p).zip(p :+ 0)
				sumPairs(paired)*/
			}
		}

		//Another pasccal
		//NOTE: c = column index, starts at 0, r = row index, starts at 0 in the triangle
		//returns: the pascal number at this position
		def pascalAtPos(c: Int, r: Int): Int = {
			if(c == 0 || c == r) 1
			else pascalAtPos(c - 1, r - 1) + pascalAtPos(c, r - 1)
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

			@tailrec
			def fac(n:Int, accum:Int):Int = {
				if(n <= 0) accum else fac(n-1, accum * n)
			}

			fac(n, 1)
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
			@tailrec
			def fibInt(stepsLeft:Int, a:BigInt=0, b:BigInt=1):BigInt = {
				if(stepsLeft <= 0) a
				else {
					fibInt(stepsLeft - 1, b, a + b)
				}
			}

			fibInt(n, 0, 1)
		}

		// Try fibonacci(60), but not too large or it will take a really long time.
		// println(fibonacci(60))

		/**
		  * Now for a tail recursive Pascal's triangle
		  */
		def pascal(n:Int):List[Int] = {


			/**
			  * You're going to need a function that can sum a List[(Int, Int)].
			  * Let's do that tail recursively for you
			  */
			@tailrec
			def sumPairs(source:List[(Int, Int)], dest:List[Int] = Nil):List[Int] = {
				// Notice we build up the destination by adding to the head.
				// This gives the list in reverse, so we have to reverse it in the terminal case
				source match { //note: can reverse these two cases and no issue ..
					case Nil => dest.reverse
					case (a, b) :: tail => sumPairs(tail, (a + b) :: dest)
				}
			}
			/*def sumPairs(l:List[(Int, Int)]):List[Int] = {

				l match {
					case Nil => Nil
					case (a,b) :: tail => (a + b) :: sumPairs(tail)
				}
			}*/
			// (1, 2) :: (3,4) :: Nil,  dest = Nil
			// sumPairs((3,4), (1+2) :: Nil)
			// sumPairs(Nil, (3:4) :: 3 :: Nil)
			// 3 :: 7 :: Nil


			// I'll let you define the inner tail-recursive function this time.

			//note: tricky - List(1) must be the base case, not Nil list
			@tailrec
			def innerPasc(n: Int, previous: List[Int] = List(1)): List[Int] = {
				if(n <= 0) previous
				else {
					val left = 0 +: previous
					val right = previous :+ 0
					val paired = left.zip(right)

					innerPasc(n - 1, sumPairs(paired))
				}
			}

			innerPasc(n, previous=List(1))
		}

	}

	/**
	  * Oh no, it's the Roman numerals again.
	  *
	  * But that means I can put them in the solution!
	  * Let's define a tail recursive function for producing Roman numerals
	  */
	object Roman {

		/*
		 * I've put these in a List so we can recursively take the head off it, and work
		 * from big numerals to small.
		 */
		val allNumerals = List(
			"M" -> 1000, "CM" -> 900,
			"C" -> 100, "XC" -> 90,
			"L" -> 50, "XL" -> 40,
			"X" -> 10, "IX" -> 9,
			"V" -> 5, "IV" -> 4, "I" -> 1
		)

		/**
		  * First, define a tail recursive function that will select the first (numeral, number) pair
		  * from our list of numerals that is smaller than n.
		  *
		  * Keep the (numeral, value)s that are yet to be considered in the second argument.
		  *
		  * This is going to be our function for choosing the nextNumeral
		  */
		// @tailrec
		def nextNumeral(n:Int, numerals:List[(String, Int)] = allNumerals):(String, Int) = {
			// either the head pair is the one we want, or call ourselves recursively for the tail
			numerals match {
				case (romanLetter, value) :: _  if value <= n  => (romanLetter, value) //returning this pair
				case (_, value) :: tail if value > n          => nextNumeral(n, tail)
				case _ => throw new IllegalArgumentException(s"can cope with $n")
			}
		}

		/**
		  * Now we'll define our Roman numerals function
		  */
		def roman(n:Int):String = {

			// Now define an inner tail recursive function that will build up our Roman numeral
			// We keep the string we've built so far in s
			@tailrec
			def intRom(n:Int, accRoman:String = ""):String = {

				/*if(n == 0){
				  return accRomanLetters
				} else {
				  val (romanLetter, value) = nextNumeral(n)
				  intRom(n - value, accRomanLetters + romanLetter)
				}*/
				if(n <= 0){
					return accRoman
				}
				nextNumeral(n) match {
					case (_, value) if value == 0 => accRoman //when no more numerals, return the accumulator string
					case (romanLetter, value)  => intRom(n - value, accRoman + romanLetter)
				}
				//terminal case: n <= 0 = just return the built up string
				// else keep looking: plug in the remainder (value subtracted from n), and add the found roman
				// numeral.
			}

			intRom(n)
		}


	}


	/**
	  * A puzzle list
	  *
	  * 1
	  * 1 1
	  * 2 1
	  * 1 2 1 1
	  * 1 1 1 2 2 1
	  * 3 1 2 2 1 1
	  * 1 3 1 1 2 2 2 1
	  * 1 1 1 3 2 1 3 2 1 1
	  *
	  *
	  * List(3, 1, 1, 3, 1, 2, 1, 1, 1, 3, 1, 2, 2, 1)
	  * List(1, 3, 2, 1, 1, 3, 1, 1, 1, 2, 3, 1, 1, 3, 1, 1, 2, 2, 1, 1)
	  * List(1, 1, 1, 3, 1, 2, 2, 1, 1, 3, 3, 1, 1, 2, 1, 3, 2, 1, 1, 3, 2, 1, 2, 2, 2, 1)
	  *
	  * What's happening?
	  * Well, the top line has "one 1"
	  * And the next has "two 1s"
	  * And the next has "one 2, one 1"
	  * And the next has "one 1, one 2, two 1s"
	  *
	  */

	object Puzzle {

		/**
		  * Start off by defining a tail-recursive function that can produce the next line from the
		  * previous one. It's going to consume a "source" list and produce a "dest" list.
		  * The source list will get shorter each step until we've worked our way through it.
		  * We'll build up the dest list *backwards* because it's easier to append to the head of a list.
		  *
		  * Our terminal condition is if the source list is empty.
		  * If not, have two outer cases. Is the destination list empty or not?
		  * If not, does the number at the head of the source list match or not match dest(1)?
		  */
		//TODO study more: got this solution

		@tailrec
		def nextLine(source:List[Int], acc:List[Int] = Nil):List[Int] = {
			source match {

				case Nil => acc.reverse
				case x :: xs => acc match {

					case a :: b :: tail if a == x   => nextLine(xs, a :: b + 1 :: tail)
					case _                          => nextLine(xs, x :: 1 :: acc)
				}
			}
		}

		//Different variable names
		def next(source: List[Int], dest:List[Int] = Nil): List[Int] = {
			source match {
				case Nil => dest.reverse //because we append on the head of the dest list
				case  sHead :: sTail => dest match {

					//if source head and dest head match, just want to keep the dest head, increment
					// the next dest element and keep the tail, and process rest of source tail
					case dChar :: dCount :: dTail if sHead == dChar =>
						next(sTail, dChar :: (dCount + 1) :: dTail)

					case _ => next(sTail, sHead :: 1 :: dest)
				}
			}
		}

		/**
		  * And now we write our puzzle call
		  */
		def puzzle(n:Int):List[Int] = {

			@tailrec
			def intPuz(n:Int, line:List[Int] = List(1)):List[Int] = {
				if (n == 0){ //if reached the end, return the line
					line
				} //eslse recurse, count down num lines by 1, and pass next line
				else intPuz(n - 1, nextLine(line))
			}

			intPuz(n)
		}

		/**
		List(1)
List(1, 1)
List(2, 1)
List(1, 2, 1, 1)
List(1, 1, 1, 2, 2, 1)
List(3, 1, 2, 2, 1, 1)
List(1, 3, 1, 1, 2, 2, 2, 1)
List(1, 1, 1, 3, 2, 1, 3, 2, 1, 1)
List(3, 1, 1, 3, 1, 2, 1, 1, 1, 3, 1, 2, 2, 1)
List(1, 3, 2, 1, 1, 3, 1, 1, 1, 2, 3, 1, 1, 3, 1, 1, 2, 2, 1, 1)
List(1, 1, 1, 3, 1, 2, 2, 1, 1, 3, 3, 1, 1, 2, 1, 3, 2, 1, 1, 3, 2, 1, 2, 2, 2, 1)
List(3, 1, 1, 3, 1, 1, 2, 2, 2, 1, 2, 3, 2, 1, 1, 2, 1, 1, 1, 3, 1, 2, 2, 1, 1, 3, 1, 2, 1, 1, 3, 2, 1, 1)
List(1, 3, 2, 1, 1, 3, 2, 1, 3, 2, 1, 1, 1, 2, 1, 3, 1, 2, 2, 1, 1, 2, 3, 1, 1, 3, 1, 1, 2, 2, 2, 1, 1, 3, 1, 1, 1, 2, 2, 1, 1, 3, 1, 2, 2, 1)
List(1, 1, 1, 3, 1, 2, 2, 1, 1, 3, 1, 2, 1, 1, 1, 3, 1, 2, 3, 1, 1, 2, 1, 1, 1, 3, 1, 1, 2, 2, 2, 1, 1, 2, 1, 3, 2, 1, 1, 3, 2, 1, 3, 2, 2, 1, 1, 3, 3, 1, 2, 2, 2, 1, 1, 3, 1, 1, 2...

		  */

	}


}
