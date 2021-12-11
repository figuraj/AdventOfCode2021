import scala.io.Source

val in = Source.fromFile(getClass.getResource("/inputs/08_input.txt").getFile)
val inputs = in.getLines().toList

val (input_patterns, output_patterns) = {
  val temp = inputs.map(line => line.split(" \\Q|\\E ").toList)
  (temp.map(_.head.split(" ").toList), temp.map(_.last.split(" ").toList))
}

val results_part1 = output_patterns
  .flatten
  .groupBy(digit => digit.length)
  .filter(x => (x._1 == 2) || (x._1 == 4) || (x._1 == 3) || (x._1 == 7))
  .map(x => x._2.length).sum

// **** Part 2
def solve(pattern: List[String]): Map[Char, Char] = {
  val segment_count: Map[Char, Int] = pattern
    .flatten
    .groupBy(x => x)
    .map { case (char, charlist) => char -> charlist.length }

  val count_to_segment: Map[Int, Char] = segment_count
    .filter { case (_, int) => List(9, 4, 6).contains(int) }
    .map { case (char, int) => int -> char }

  def segments_of_digit_by_length(len: Int): Set[Char] = pattern.filter(_.length == len).head.toSet

  val f = count_to_segment(9)
  val e = count_to_segment(4)
  val b = count_to_segment(6)
  val c = (segments_of_digit_by_length(2) - f).head
  val a = (segments_of_digit_by_length(3) - f - c).head
  val d = (segments_of_digit_by_length(4) - f - c - b).head
  val g = (segments_of_digit_by_length(7) - a - b - c - d - e - f).head

  Map(a -> 'a', b -> 'b', c -> 'c', d -> 'd', e -> 'e', f -> 'f', g -> 'g')
}

def decodeOutputPattern(pattern: List[String], cipher: Map[Char, Char], segments_to_digit: Map[String, Int]): Int = {
  pattern.map(digit => digit map cipher)
    .map(_.sorted).map(segments_to_digit)
    .foldLeft("")((accum, elem) => accum + elem.toString).toInt
}

val valid_segments_definition = List("abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg")
val segments_to_digit: Map[String, Int] = (valid_segments_definition zip (0 to 9)).toMap

val decoded_outputs = for {i <- input_patterns.indices
                           input_pattern = input_patterns(i)
                           cipher = solve(input_pattern)
                           output_pattern = output_patterns(i)}
yield decodeOutputPattern(output_pattern, cipher, segments_to_digit)

val results_part2 = decoded_outputs.sum