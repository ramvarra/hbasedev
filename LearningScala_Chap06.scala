import javax.print.DocFlavor.BYTE_ARRAY

/**
 * EX1: Create a list of the first 20 odd Long numbers. Can you create this with a for-loop, with the filter
 * operation, and with the map operation? What’s the most efficient and expressive way to write this?
 */
def ex1(): Unit = {
  val showList2 = (ln: String, l: List[Long]) => s"$ln: ${l.head}...${l.last}  size = ${l.size}"
  // using for loop
  val l1 = (for (i <- 1L to 20L*2 by 2) yield i).toList
  println(showList2("l1", l1))

  // using filter
  val l2 = (1L to 40L).toList.filter(_%2 == 1)
  println(showList2("l2", l2))
  // using map
  val l3 = (0L until 20L).toList.map(_*2+1)
  println(showList2("l3", l3))

}

/**
 * EX2: Write a function titled “factors” that takes a number and returns a list of its factors, other than 1 and the
 * number itself. For example, factors(15) should return List(3, 5).
 * Then write a new function that applies “factors” to a list of numbers. Try using the list of Long numbers you
 * generated in exercise 1. For example, executing this function with List(9, 11, 13, 15) should return
 * List(3, 3, 5), because the factor of 9 is 3 while the factors of 15 are 3 again and 5. Is this a good
 * place to use map and flatten? Or would a for-loop be a better fit?
 */
def factors(n: Long): List[Long] = {
  (2L to math.pow(n, .5).toLong+1).toList.filter(n % _ == 0).flatMap(x => List(x, n/x)).distinct
}

def ex2() = {
  List(9L, 11L, 13L, 15L).flatMap(x => factors(x))
}
//ex2()
/**
 * EX3: Write a function, first[A](items: List[A], count: Int): List[A], that returns the first x number of items in
 * a given list. For example, first(List('a','t','o'), 2) should return List('a','t'). You could make this a
 * one-liner by invoking one of the built-in list operations that already performs this task, or (preferably) implement
 * your own solution. Can you do so with a for-loop? With foldLeft? With a recursive function that only accesses
 * head and tail?
 */
def first1[A](items: List[A], count: Int): List[A] = items.take(count)
def first2[A](items: List[A], count: Int): List[A] = {
  var res: List[A] = Nil
  var l = items
  for (i <- 0 until count) {
    if (l.nonEmpty) {
      res = l.head :: res
      l = l.tail
    }
  }
  res.reverse
}

def first3[A](items: List[A], count: Int): List[A] = {
  // this may be not be efficient since we are appending list
  //items.foldLeft(List[A]())((a, b) => {if (a.size < count) a ::: List(b) else a})
  // natural list cons
  items.foldLeft(List[A]())((a, b) => {if (a.size < count) b :: a else a}).reverse
}

def first4[A](items: List[A], count: Int): List[A] = {
  // not tail recursive
  if (count == 0 || items.isEmpty) Nil else items.head :: first4(items.tail, count-1)
}

/**
 * EX4:Write a function that takes a list of strings and returns the longest string in the list. Can you avoid
 * using mutable variables here? This is an excellent candidate for the list-folding operations (Table 6-5) we
 * studied. Can you implement this with both fold and reduce? Would your function be more useful if it took a
 * function parameter that compared two strings and returned the preferred one? How about if this function was
 * applicable to generic lists, i.e., lists of any type?
 */
def longest_fold(l: List[String]): String = {
  l.fold("")((prev, cur) => if (prev.length > cur.length) prev else cur)
}
def longest_reduce(l: List[String]): String = {
  l.reduce((prev, cur) => if (prev.length > cur.length) prev else cur)
}

def generic_longest_reduce[A](l: List[A], chooser: (A,A)=>A): A = {
  l.reduce(chooser(_, _))
}
def ex4(): Unit = {
  val l = List("a", "aa", "bb", "ccc", "a", "bb")
  generic_longest_reduce[String](l, (prev, cur) => if (prev.length > cur.length) prev else cur)
}

/**
 * EX5:Write a function that reverses a list. Can you write this as a recursive function? This may be a good place
 * for a match expression.
 */
def reverseList[A](l: List[A]): List[A] = {
  l match {
    case x::xs => reverseList(xs) ::: List(x)
    case Nil => Nil
  }
}
def ex5() : Unit = {
  val l = List(1, 2, 3, 4, 5, 6)
  reverseList(l)
}

/**
 * Write a function that takes a List[String] and returns a (List[String],List[String]), a tuple of string lists.
 * The first list should be items in the original list that are palindromes (written the same forward and backward,
 * like “racecar”). The second list in the tuple should be all of the remaining items from the original list.
 * You can implement this easily with partition, but are there other operations you could use instead?
 */
def pal1(l: List[String]) : (List[String], List[String]) = {
  l.partition(s => s == s.reverse)
}
def pal2(l: List[String]) : (List[String], List[String]) = {
  (l.filter(s => s.reverse == s), l.filter(s => s.reverse != s))
}
def ex6(): Unit = {
  val l = List("car", "carrac", "long", "gone", "racecar", "xxllxx")
  pal1(l)
  pal2(l)
}

/**
 *
 */
def fetchWeather() : List[String] = {
  val owmApiKey = sys.env.get("OPENWEATHERMAP_API_KEY")
  if (owmApiKey.isEmpty) {
    System.err.println("Error: OPENWEATHERMAP_API_KEY environment variable not set")
    return List()
  }
  val url = "http://api.openweathermap.org/data/2.5/forecast?mode=xml&lat=55&lon=0" + s"&APPID=${owmApiKey.get}"
  println(s"Fetching url: $url")
  io.Source.fromURL(url).getLines().toList
}

val l = fetchWeather()
val pat = """.*<name>(.+)</name>.*<country>(.+)</country>.*""".r
val pat(city, country) = l(1)