import scala.io.Source

val in = Source.fromFile(getClass.getResource("/inputs/14_input.txt").getFile)
val inputs = in.getLines().toList


def parseInputsPolymerTemplate(inputs: List[String]): Map[String, String] = {
  inputs.map {
    case s"$from -> $to"=> from -> to
  }.toMap
}

def parseInputsPolymer(inputs: String): Vector[String] = {
  inputs.toVector.map(_.toString)
}

def insertPolymer(twoPolymers: Vector[String])(implicit polymerMap: Map[String, String]): Vector[String] = {
  val split = twoPolymers.splitAt(1)
  Vector(split._1.head, polymerMap(twoPolymers.reduce(_++_)))
}

val polymer_template = parseInputsPolymerTemplate(inputs.drop(2))
val polymer = parseInputsPolymer(inputs.head)


def iter(prefix: Vector[String], remaining: Vector[String])(implicit polymerMap: Map[String, String]): Vector[String] = {
  if (remaining.length == 1) {
    prefix :+ remaining.head
  } else {
    val new_prefix = prefix ++ insertPolymer(remaining.take(2))
    iter(new_prefix, remaining.tail)
  }
}

def solve(max_rounds: Int, roundNum: Int, polymer: Vector[String]): Vector[String] = {
  if (roundNum == max_rounds) {
    polymer
  } else {
    solve(max_rounds, roundNum + 1, iter(Vector(), polymer)(polymer_template) )
  }
}

val temp = solve(10,0,polymer).groupBy(identity).map(x => x._1 -> x._2.length)
val result_part1 = temp.values.max - temp.values.min

