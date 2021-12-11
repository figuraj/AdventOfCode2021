import scala.io.Source
val in = Source.fromFile(getClass.getResource("/inputs/08_input.txt").getFile)
val inputs = in.getLines().toList

val output_patterns = inputs.map(line => (line.split(" \\Q|\\E ").toList)(1).split(" " ).toList)

val result = output_patterns
              .flatten
              .groupBy(digit => digit.length)
              .filter(x => (x._1 == 2) || (x._1 == 4) || (x._1 == 3) || (x._1 == 7))

val result2 = result.map(x => x._2.length).sum

// **** Part 2

def bruteForceSolution(pattern: List[String], source: List[String]) : Map[Char,Char] = {
  (for {a <- 'a' to 'g'
       b <- 'a' to 'g'
       c <- 'a' to 'g'
       d <- 'a' to 'g'
       e <- 'a' to 'g'
       f <- 'a' to 'g'
       g <- 'a' to 'g'
       candidate: Map[Char,Char] = Map('a' -> a, 'b' -> b, 'c' -> c, 'd' -> d , 'e' -> e, 'f' -> f , 'g' -> g)
       if pattern.map(number => number map candidate).map(_.sorted).forall(source.contains(_))
       } yield candidate)(0)
}


def decodeOutputPattern(pattern: List[String], cipher: Map[Char,Char], string_to_num: Map[String, Int]): Int = {
  pattern.map(number => number map cipher)
         .map(_.sorted).map(string_to_num)
         .foldLeft("")((accum,elem) => accum + elem.toString).toInt
}

val input_patterns = inputs.map(line => line.split(" \\Q|\\E ").toList.head.split(" " ).toList)
val source = List("abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg")
val nums = List(0,1,2,3,4,5,6,7,8,9)
val string_to_num: Map[String, Int] = (source zip nums).toMap
val ciphers = for (pattern <- input_patterns) yield bruteForceSolution(pattern,source)


val decoded_outputs = for ((pattern, cipher) <- output_patterns zip ciphers)
  yield decodeOutputPattern(pattern, cipher, string_to_num)
decoded_outputs.sum




//val line = "bg gcdaeb aebg efabdcg abdce cafdbe fcbdeg bdacg gbd cafgd".split(" ").toList//.map(number => number.sorted)

//List("daecb", "dcbae", "gb", "eabg").map(number => number map result).map(_.sorted).map(string_to_num).foldLeft("")((accum,elem) => accum + elem.toString).toInt



//val x: Map[Char,Char] = Map('a' -> 'b', 'b' -> 'f')
//"ab" map x
//List("ab","ba").map(number =>  number map x).forall(List("fe","x","x").contains(_))

//"aba".replace("abcdefg".toSeq, Seq(a,b,c,d,e,f,g)))