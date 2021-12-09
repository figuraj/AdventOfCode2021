import scala.io.Source
val in = Source.fromFile(getClass.getResource("/inputs/06_input.txt").getFile)
val inputs = in.getLines().toList

case class Fish(age: Int) {
  def decrement: Fish = Fish(age-1)
  def spawn: Seq[Fish] = Seq(Fish(6), Fish(8))
  def checkAge: Boolean = age==0
}

def parseInputs(inputs: List[String]): Seq[Int] = {
  inputs.flatMap(line => line.split(",")).map(_.toInt).toVector
}

def round(fishlist: Seq[Fish]): Seq[Fish] = {
  fishlist.flatMap{fish =>
    if (fish.checkAge) fish.spawn
    else Seq(fish.decrement)
  }
}

def main(fishlist: Seq[Fish], rounds: Int): Seq[Fish] = {
  def iter(pending: Seq[Fish], accum: Int): Seq[Fish] = {
    if (accum == rounds) pending
    else iter(round(pending), accum+1)
  }
  iter(fishlist,0)
}

val fishlist = parseInputs(inputs).map(age => Fish(age))
val result_part1 = main(fishlist, 80).length


//************* Part 2 *******************
case class FishByAge(zero: Long, one: Long, two: Long, three: Long, four: Long, five: Long, six: Long, seven: Long, eight: Long) {
  def getSize: Long = zero+one+two+three+four+five+six+seven+eight
}

val fishmap = parseInputs(inputs).groupBy(identity).map(x => (x._1,x._2.length)).withDefaultValue(0)
val pool = FishByAge(fishmap(0), fishmap(1), fishmap(2),
                     fishmap(3), fishmap(4), fishmap(5),
                     fishmap(6), fishmap(7), fishmap(8))

def round2(pool: FishByAge): FishByAge = {
  pool match {
    case FishByAge(zero, one, two, three, four, five, six, seven, eight) =>
      FishByAge(one, two, three, four, five, six, seven+zero, eight, zero)
    }
}

def main2(pool: FishByAge, rounds: Int): FishByAge = {
  def iter(pending: FishByAge, accum: Int): FishByAge = {
    if (accum == rounds) pending
    else iter(round2(pending), accum+1)
  }
  iter(pool,0)
}

val result_part2 = main2(pool, 256).getSize


