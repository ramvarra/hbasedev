println("Learning Scala exercise 1")

//---------------------------------------------------------------------------------------
//1. Given string name return string if nonempty else "n/a"
def ex1() : Unit = {
    val strings = Array("good", "")
    val results = for (s <- strings) yield {
        s match {
            case "" => "n/a"
            case _  => s
        }
    }
}

//---------------------------------------------------------------------------------------
//2.For a double, return "greater", "less", "same" if > 0, < 0 or == 0
def ex2() : Unit = {
    val values = Array(12.8, -48.2, 0.0)
    def ex2ByIfElse(v: Double) : String = {
        if (v < 0) "less" else if (v > 0) "greater" else "same"
    }
    def ex2ByMatchCase(v: Double) : String = {
        v match {
            case _ if v < 0 => "less"
            case _ if v > 0 => "greater"
            case _ => "same"
        }     
    }
    for (v <- values) {
        val r = (ex2ByIfElse(v), ex2ByMatchCase(v))
        println(s"$v == '${r._1}' '${r._2}'")
    }
}

//---------------------------------------------------------------------------------------
// Ex 3
// Write an expression to convert one of the input values cyan, magenta, yellow to
// their six-char hexadecimal equivalents in string form. 
// What can you do to handle error conditions?
def ex3() : Unit = {
    def colorNameToHex(color : String) : String = {
        color match {
            case "cyan" => "00ffff"
            case "magenta" => "ff00ff"
            case  "yellow" => "ffff00"
            case _ => {
                println(s"Error Invalid color name: $color")
                null
            }
        }
    }
    val colors = Array("cyan", "magenta", "yellow", "badcolor")
    for (color <- colors) {
        val r = colorNameToHex(color)
        println(s"Color '$color' value = '$r'")
    }
}
// ex3()

//---------------------------------------------------------------------------------------
// Ex 4: 
// Print the numbers 1 to 100, with each line containing a group of five numbers.
def ex4() : Unit = {
    for (i <- 1 to 100) {
        print(i)        
        if (i % 5 == 0) print("\n") else print(",")
    }
}
// ex4()

//---------------------------------------------------------------------------------------
// Ex 5: 
// Write an expression to print the numbers from 1 to 100, except that for multiples of 3, 
// print “type,” and for multiples of 5, print “safe.” For multiples of both 3 and 5, 
// print “typesafe.”
def ex5() : Unit = {
    for (i <- 1 to 100; t = (i % 3, i % 5)) {               
        println(t match {
            case (0, 0) => "typesafe"
            case (0, _) => "type"
            case (_, 0) => "safe"
            case (_, _) => i.toString
        })
    }
}
ex5()

//---------------------------------------------------------------------------------------
// Ex 6: 
// Can you rewrite the answer to exercise 5 to fit on one line? 
// It probably won’t be easier to read, but reducing code to its shortest form is an 
// art, and a good exercise to learn the language.
def ex6() : Unit = {
    for (i <- 1 to 100; m3 = (i % 3 == 0); m5 = (i % 5 == 0)) println(if (m3 && m5) "typesafe" else if (m3) "type" else if (m5) "safe" else i)
}
//ex6()