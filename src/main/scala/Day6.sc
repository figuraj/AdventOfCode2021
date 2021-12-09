import scala.io.Source
val in = Source.fromFile(getClass.getResource("/inputs/06_input.txt").getFile)
val inputs = in.getLines().toList

def parseInputs(inputs: List[String]): Seq[Int] = {
  inputs.flatMap(line => line.split(",")).map(_.toInt).toVector
}

case class FishByAge(zero: Long, one: Long, two: Long, three: Long,
                     four: Long, five: Long, six: Long, seven: Long, eight: Long) {
  def getSize: Long = zero+one+two+three+four+five+six+seven+eight
}

val fishmap = parseInputs(inputs).groupBy(identity).map(x => (x._1,x._2.length)).withDefaultValue(0)
val pool = FishByAge(fishmap(0), fishmap(1), fishmap(2),
                     fishmap(3), fishmap(4), fishmap(5),
                     fishmap(6), fishmap(7), fishmap(8))

def round(pool: FishByAge): FishByAge = {
  pool match {
    case FishByAge(zero, one, two, three, four, five, six, seven, eight) =>
      FishByAge(one, two, three, four, five, six, seven+zero, eight, zero)
    }
}

def main(pool: FishByAge, rounds: Int): FishByAge = {
  def iter(pending: FishByAge, accum: Int): FishByAge = {
    if (accum == rounds) pending
    else iter(round(pending), accum+1)
  }
  iter(pool,0)
}

val result_part2 = main(pool, 80).getSize
val result_part2 = main(pool, 256).getSize


