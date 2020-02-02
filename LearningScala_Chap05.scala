/**
 * Learning Scala Chapter 5 Exercises
 */


/**
 * Write a function literal that takes two integers and returns the higher number. Then write a higher-order
 * function that takes a 3-sized tuple of integers plus this function literal, and uses it to return the
 * maximum value in the tuple.  The library function util.Random.nextInt
 *
 */
def ex1_max2(a: Int, b: Int) : Int = {
  if (b > a) b else a
}
def ex1(t: (Int, Int, Int), maxer: (Int, Int)=>Int): Int = {
  maxer(maxer(t._1, t._2), t._3)
}

/**
 * EX2: The library function util.Random.nextInt returns a random integer. Use it to invoke the “max” function
 * with two random integers plus a function that returns the larger of two given integers.
 * Do the same with a function that returns the smaller of two given integers, and then a function that
 * returns the second integer every time.
 */
def ex2_select(a: Int, b: Int, f:(Int, Int)=>Int): Int = {
  f(a, b)
}
def ex2() : Unit = {
  val a = util.Random.nextInt()
  val b = util.Random.nextInt()
  println(s"Max($a, $b) = " + ex2_select(a, b, (a, b) => if (b>a) b else a))
  println(s"Min($a, $b) = " + ex2_select(a, b, (a, b) => if (b<a) b else a))
  println(s"2nd($a, $b) = " + ex2_select(a, b, (a, b) => b))

}
// ex2()

/**
 * Ex3: Write a higher-order function that takes an integer and returns a function. The returned function should
 * take a single integer argument (say, “x”) and return the product of x and the integer passed to the
 * higher-order function.
 */
def ex3MultBy(n: Int) : (Int) => Int = {
  (x: Int) => n * x
}
def ex3() :Unit = {
  println("3 x 4 = " + ex3MultBy(3)(4))
  println("10 x 20 = " + ex3MultBy(10)(20))
}
// ex4()

/**
 * EX4: Let’s say that you happened to run across this function while reviewing another developer’s code:
 * def fzero[A](x: A)(f: A => Unit): A = { f(x); x }
 * What does this function accomplish? Can you give an example of how you might invoke it?
 */
def fzero[A](x: A)(f: A => Unit): A = { f(x); x }
def ex4() : Unit = {
  val ret = fzero(20)((n: Int) => println(s"The integer value is = $n"))
  println(s"Return = $ret")
}
// ex4()
/**
 * EX5: There’s a function named “square” that you would like to store in a function value. Is this the right way
 * to do it? How else can you store a function in a value?
 * def square(m: Double) = m * m
 * val sq = square
 * NO - square on single arg function invokes it (compiler error missing arg)
 * Need a placeholder square _ or val should have explicit type (Double)=>Double
 */
def ex5() : Unit = {
  def square(m: Double) = m * m
  val sq1 = square _
  val sq2: (Double) => Double = square
  sq1(10)
  sq2(10)
}

/**
 * EX6: Write a function called “conditional” that takes a value x and two functions, p and f,
 * and returns a value of the same type as x. The p function is a predicate, taking the value x and
 * returning a Boolean b. The f function also takes the value x and returns a new value of the same type.
 * Your “conditional” function should only invoke the function f(x) if p(x) is true, and otherwise
 * return x. How many type parameters will the “conditional” function require?
 */
def conditional[T](x: T, predicate:T=>Boolean, f:T=>T) : T = {
  if (predicate(x)) f(x) else x
}
def ex6() : Unit = {
  // div by 2 if even
  for (n <- Array(2, 3, 10, 21, 32)) {
    println(s"Div2IfEven: $n = " + conditional(n, (x: Int) => x % 2 == 0, (x: Int) => x/2))
    println(s"Div2IfEven: $n = " + conditional[Int](n, _ % 2 == 0, _/2))

  }
}
// ex6()
/**
 * Do you recall the “typesafe” challenge from the exercises in Chapter 3? There is a popular coding interview
 * question I’ll call “typesafe,” in which the numbers 1-100 must be printed one per line. The catch is
 * that multiples of 3 must replace the number with the word “type,” while multiples of 5 must replace the
 * number with the word “safe.” Of course, multiples of 15 must print “typesafe.”
 * Use the “conditional” function from exercise 6 to implement this challenge.
 * Would your solution be shorter if the return type of “conditional” did not match the type of the
 * parameter x? Experiment with an altered version of the “conditional” function that works better with
 * this challenge.
 */

def ex7() : Unit = {
  def conditional[T](x: T, predicate:T=>Boolean, f:T=>String) : String = {
    if (predicate(x)) f(x) else x.toString()
  }
  for (n <- 1 to 100) {
    val s3 = conditional[Int](n, _%3 == 0, (x: Int)=> "type")
    val s5 = conditional[Int](n, _%5 == 0, (x: Int)=> "safe")
    val r = (s3, s5) match {
      case ("type", "safe") => "typesafe"
      case ("type", _) => "type"
      case (_, "safe") => "safe"
      case (_, _) => s3
    }
    println(s"$r")
  }
}
//ex7()
def ex7_1() : Unit = {
  def conditional[T](x: T, predicate:T=>Boolean, r: String, f:T=>String) : String = {
    if (predicate(x)) r else f(x)
  }
  for (n <- 1 to 100) {
    val r = conditional[Int](n, _%15 == 0, "typesafe", (n: Int)=> {
      conditional[Int](n, _%3 == 0, "type", (n: Int)=> {
        conditional[Int](n, _%5 == 0, "safe", _.toString())
      })
    })
    println(s"$r")
  }
}
ex7_1()