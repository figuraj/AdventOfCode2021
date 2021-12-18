import scala.io.Source

val in = Source.fromFile(getClass.getResource("/inputs/14_input.txt").getFile)
val inputs = in.getLines().toList

def parseInputsPolymerTemplate(inputs: List[String]): Map[String, List[String]] = {
  inputs.map {
    case s"$from -> $to"=> from -> List(from.substring(0,1) + to.head, to.substring(0,1) + from.tail)
  }.toMap
}

def parseInputsPolymer(inputs: String, polymer: Map[String, Long] = Map().withDefaultValue(0)): Map[String, Long] = {
  if (inputs.length == 1){
    polymer
  } else {
    inputs.toList match {
      case x1 :: x2 :: xs =>
        val pair = s"$x1$x2"
        parseInputsPolymer(inputs.tail, polymer.updated(pair, polymer(pair) + 1))
    }
  }
}

def iter(polymer: Map[String, Long], new_polymer: Map[String, Long] = Map().withDefaultValue(0) )
        (implicit polymerMap: Map[String, List[String]]): Map[String, Long] = {
  if (polymer.isEmpty) {
    new_polymer
  } else {
    val source_pair = polymer.head
    val generated_pairs = polymerMap(source_pair._1)
    val updated_polymer = new_polymer
      .updated(generated_pairs.head, new_polymer(generated_pairs.head) + source_pair._2)
      .updated(generated_pairs.last, new_polymer(generated_pairs.last) + source_pair._2)
    iter(polymer.removed(source_pair._1), updated_polymer)
  }
}

def solve(max_rounds: Int, roundNum: Int, polymer: Map[String, Long])(implicit polymerMap: Map[String, List[String]]): Map[String, Long] = {
  if (roundNum == max_rounds) {
    polymer
  } else {
    solve(max_rounds, roundNum + 1, iter(polymer))
  }
}

def pairsToLetters(polymer: Map[String, Long], first_letter: Char, last_letter: Char): Map[Char, Long] = {
  val zero: Map[Char, Long] = Map().withDefaultValue(0)
  val letter_counts = polymer.foldLeft(zero) {(accum, elem) =>
    val first = accum.updated(elem._1.head, accum(elem._1.head) + elem._2)
    first.updated(elem._1.last, first(elem._1.last) + elem._2)
  }
  letter_counts.map { case (k,v) => if ((k==first_letter) || (k == last_letter)) k -> (v/2 + 1) else k -> v/2}
}

val polymerMap = parseInputsPolymerTemplate(inputs.drop(2))
val polymer = parseInputsPolymer(inputs.head)

val pair_counts_part1: Map[String, Long] = solve(10,0, polymer)(polymerMap)
val letter_counts_part1: Map[Char, Long] = pairsToLetters(pair_counts_part1,inputs.head.head, inputs.head.last)
val result_part1 = letter_counts_part1.values.max - letter_counts_part1.values.min

val pair_counts_part2: Map[String, Long] = solve(40,0, polymer)(polymerMap)
val letter_counts_part2: Map[Char, Long] = pairsToLetters(pair_counts_part2,inputs.head.head, inputs.head.last)
val result_part2 = letter_counts_part2.values.max - letter_counts_part2.values.min