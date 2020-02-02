/**
 * Learning Scala - Champter 4 Exercises
 */

/**
 * Ex 1
 * Write a function that computes the area of a circle given its radius.
 */
def ex1(r: Double) : Double = {
    math.Pi * r * r
}
// ex(1); ex(1.5)

/**
 * EX2: Provide an alternate form of the function in exercise 1 that takes the radius as
 * a String. What happens if your function is invoked with an empty String ?
 */
def ex2(rs: String) : Double = {
    val r = rs.toDouble
    math.Pi * r * r
}
// ex2("1")
// ex2("1.532")
// ex2("") // Raise empty string exception.

/**
 * EX: 3: Write a recursive function that prints the values from 5 to 50 by fives, without using for
 * or while loops. Can you make it tail-recursive?
 */
def ex3(start: Int, end: Int): Unit = {
    println(start)
    if (start < end) ex3(start + 5, end)
}
//ex3(5, 50)

/**
 * EX: 4: Write a function that takes a milliseconds value and returns a string describing the value
 * in days, hours, minutes, and seconds. What’s the optimal type for the input value?
 */
def ex4(ms: Long): String = {
    def fmtToString(n: Long, u: String): String = {
        n match {
            case _ if (n > 1) => s" $n ${u}s"
            case _ if (n == 1) => s" $n ${u}"
            case _ => ""
        }
    }
    var secs = ms / 1000
    val days = secs / (24*3600)
    secs = secs % (24*3600)
    val hours = secs / 3600
    secs = secs % 3600
    val mins = secs / 60
    secs = secs % 60
    fmtToString(days, "day") + fmtToString(hours, "hour") + fmtToString(mins, "minute") +
      fmtToString(secs, "second")
}

//ex4(((((((3*24)+6)*60)+43)*60)+12)*1000)
//ex4(((((((3*24)+1)*60)+43)*60)+12)*1000)
//ex4(((((((1*24)+1)*60)+0)*60)+12)*1000)

/**
 * EX5: Write a function that calculates the first value raised to the exponent of the second value. Try writing
 * this first using math.pow, then with your own calculation. Did you implement it with variables? Is there
 * a solution available that only uses immutable data? Did you choose a numeric type that is large enough for
 * your uses?
 */
def ex5(n : Long, p : Int) : Long = {
    // math.pow(n, p).toLong
    var r : Long = 1
    for (i <- 1 to p)
        r = r * n
    r
}
// ex5(2, 38)
/**
 * EX6: Write a function that calculates the difference between a pair of 2D points (x and y) and returns the
 * result as a point. Hint: this would be a good use for tuples (see Tuples).
 */
def ex6(p1: (Double, Double), p2: (Double, Double)) : Double = {
    math.sqrt(math.pow(p1._1 - p2._1, 2) + math.pow(p1._2 - p2._2, 2))
}
// ex6((0,0), (3,4))

/**
 * Write a function that takes a 2-sized tuple and returns it with the Int value (if included) in the
 * first position.
 * This is ambigous - not clear
 */
def ex7[T1, T2](t : (T1, T2)) : (Int, T2) = {
    if (t._1.isInstanceOf[Double] || t._1.isInstanceOf[Int]) {
        (t._1.toString.toDouble.toInt, t._2)
    } else {
        (0, t._2)
    }
}
// ex7()
/**
 * Write a function that takes a 3-sized tuple and returns a 6-sized tuple, with each original parameter
 * followed by its String representation. For example, invoking the function with (true, 22.25, "yes")
 * should return (true, "true", 22.5, "22.5", "yes", "yes"). Can you ensure that tuples of all possible types
 * are compatible with your function? When you invoke this function, can you do so with explicit types
 * not only in the function result but in the value that you use to store the result?
 */
def ex8[T1,T2,T3](t: (T1, T2, T3)) : (T1, String, T2, String, T3, String) = {
    (t._1, t._1.toString(), t._2, t._2.toString(), t._3, t._3.toString())
}
// val x : (Boolean, String, Double, String, String, String) = ex8(false, 2.2, "no")
