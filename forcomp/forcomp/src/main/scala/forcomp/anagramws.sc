import forcomp.Anagrams._
import forcomp._

val w = "Robert"

val lol = loadDictionary take 3

lol ::: lol

val lol2 = (lol ::: lol) groupBy (word => wordOccurrences(word))

val x = wordOccurrences("aaabbcde")

val y = wordOccurrences("aac")


val qqq = sentenceOccurrences(List("Yes", "man"))
val qqa = combinations(qqq)

sentenceAnagrams(List("Yes", "man")).toSet

