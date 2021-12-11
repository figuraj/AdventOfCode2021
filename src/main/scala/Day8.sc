import scala.io.Source
val in = Source.fromFile(getClass.getResource("/inputs/08_input.txt").getFile)
val inputs = in.getLines().toList

val (input_patterns, output_patterns) = {
  val temp = inputs.map(line => line.split(" \\Q|\\E ").toList)
  (temp.map(_.head.split(" ").toList), temp.map(_.last.split(" " ).toList))
}

val results_part1 = output_patterns
  .flatten
  .groupBy(digit => digit.length)
  .filter(x => (x._1 == 2) || (x._1 == 4) || (x._1 == 3) || (x._1 == 7))
  .map(x => x._2.length).sum

// **** Part 2

def bruteForceSolution(pattern: List[String], valid_segments: List[String]): Map[Char, Char] = {
  (for {a <- 'a' to 'g'
        b <- 'a' to 'g'
        c <- 'a' to 'g'
        d <- 'a' to 'g'
        e <- 'a' to 'g'
        f <- 'a' to 'g'
        g <- 'a' to 'g'
        candidate_solution: Map[Char, Char] = Map('a' -> a, 'b' -> b, 'c' -> c, 'd' -> d, 'e' -> e, 'f' -> f, 'g' -> g)
        if pattern.map(digit => digit map candidate_solution).map(_.sorted).forall(valid_segments.contains(_))
        } yield candidate_solution) (0)
}

def decodeOutputPattern(pattern: List[String], cipher: Map[Char,Char], segments_to_digit: Map[String, Int]): Int = {
  pattern.map(digit => digit map cipher)
         .map(_.sorted).map(segments_to_digit)
         .foldLeft("")((accum,elem) => accum + elem.toString).toInt
}

val valid_segments_definition = List("abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg")
val segments_to_digit: Map[String, Int] = (valid_segments_definition zip (0 to 9)).toMap
val ciphers = for (pattern <- input_patterns) yield bruteForceSolution(pattern,valid_segments_definition)

val decoded_outputs = for ((pattern, cipher) <- output_patterns zip ciphers)
  yield decodeOutputPattern(pattern, cipher, segments_to_digit)

val results_part2 = decoded_outputs.sum