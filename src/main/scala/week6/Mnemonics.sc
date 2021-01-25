import scala.io.Source

val input = Source.fromURL("file:///Users/garrett.bidwell/Downloads/forcomp/src/main/resources/forcomp/linuxwords.txt")

// in.getLines is an iterator so we need to convert it to a Sequence
// some words contain a hyphen and our program can't deal with that. So we'll drop all the hyphen
val words = input.getLines.toList filter (word => word forall (char => char.isLetter))

val mnem = Map(
  '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
  '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

// Invert the mnem map to give a map from 'A' ... 'Z' tp '2' ... '9'
val charCode: Map[Char, Char] =
  for {
    (digit, str) <- mnem
    ltr <- str
  } yield ltr -> digit

// Maps a word to the digit string it can prepresent, e.g., "Java" -> "5282"
def wordCode(word: String): String = word.toUpperCase map charCode

wordCode("JAVA")
wordCode("Java")

 /*
  * A map from digit strings to the words that represent them,
  * e.g., "5282 -> List("Java", "Kata", "Lava", ...)
  * Note: A missing number should map to the empty set, e.g., "111" -> "5282"
  */
val wordsForNum: Map[String, Seq[String]] = words groupBy wordCode withDefaultValue Seq()

// Return all ways to encode a number as a list of words
def encode(number: String): Set[List[String]] =
  if (number.isEmpty) Set(List())
  else {
    for {
      split <- 1 to number.length
      word <- wordsForNum(number take split)
      rest <- encode(number drop split)
    } yield word :: rest
  }.toSet

encode("7225247386")

def translate(number: String): Set[String] =
  encode(number) map (_ mkString " ")

translate("7225247386")
