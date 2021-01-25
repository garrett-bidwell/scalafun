val xs = Array(1, 2, 3, 44)
xs map (x => x * 2)

val s = "Hello World"
s filter(c => c.isUpper)
s exists(c => c.isUpper)
s forall(c => c.isUpper)

val pairs = List(1, 2, 3) zip s
pairs.unzip

s flatMap(c => List('.', c))

xs.sum
xs.max

def isPrime(n: Int): Boolean = (2 until n) forall(d => n % d != 0)
isPrime(49)
isPrime(13)

// scalar product using zip and for-expression
def scalarProduct(xs: List[Double], ys: List[Double]): Double =
  (for ((x, y) <- xs zip ys) yield x * y).sum

// a set is written analogously to a sequence:
val fruits = Set("apple", "banana", "pear")
val s1 = (1 to 6).toSet

s1 map(_ + 2)
fruits filter(_.startsWith("app"))
s.nonEmpty

// a map of type Map[Key, Value] associates keys of type Key with values of type Value
val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10)
val countryCapitals = Map("US" -> "Washington DC", "Switzerland" -> "Bern")
// Map[Key, Value] also extends the function type Key => Value, so maps can be used everywhere functions can
countryCapitals("US")
// get returns Option[String]
countryCapitals get("US")
countryCapitals get("Andorra")

def showCapital(country: String): String = countryCapitals.get(country) match {
  case Some(capital) => capital
  case None => "missing data"
}
showCapital("US")
showCapital("Andorra")

//Two useful operations in SQL queries are groupBy and orderBy
val fruit = List("apple", "pear", "orange", "pineapple")
// sort by length
fruit sortWith(_.length < _.length)
// sort by natural ordering
fruit.sorted
// groupBy partitions a collection into a map of collections according to a discriminator function f
fruit groupBy(_.head)

"aacdBB".toLowerCase.groupBy(c => c)
"aacdBB".toLowerCase.groupBy(c => c).toList.map({ case (char, occList) => (char, occList.length()) }).sortBy(_._1)
//def charOccurrences(str: String): List[(Char, Int)] =
//  str.toLowerCase.groupBy(c => c).toList//.map({ case (char, occList) => (char, occList.length()) }) sorted
val occurrences = List(('a', 2), ('b', 2))
val ocs = occurrences.map(x => (for(i <- 1 to x._2) yield (x._1, i)).toList)
val combs = ocs.foldRight(List[List[(Char, Int)]](Nil))((x, y) => y ++ (for(i <- x; j <- y) yield (i :: j)))