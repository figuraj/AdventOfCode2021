import scala.io.Source

val in = Source.fromFile(getClass.getResource("/inputs/11_input.txt").getFile)
val inputs = in.getLines().toList.map(line => line.map(_.asDigit).toList)

case class Octopus(x: Int, y: Int) {
  def getPointNeighbors(ocean: Map[Octopus, Int]): Set[Octopus] = {
    val candidates = Set((x - 1,y),(x + 1,y),(x,y - 1),(x,y + 1),(x + 1,y - 1),(x + 1,y + 1),(x - 1,y - 1),(x - 1,y + 1))
    candidates
      .filter(p => (0 to 9).contains(p._1) && (0 to 9).contains(p._2))
      .map(x => Octopus(x._1,x._2))
  }
}

val ocean: Map[Octopus, Int] = (for (x <- 0 to 9; y <- 0 to 9) yield Octopus(x,y) -> inputs(x)(y)).toMap

def increment(ocean: Map[Octopus, Int], subset: Set[Octopus] = Set()): Map[Octopus, Int] = {
  if (subset.isEmpty) {
    ocean.map(x => x._1 -> (x._2 + 1))
  } else {
    ocean.map(x => if (subset.contains(x._1)) x._1 -> (x._2 + 1) else x)
  }
}

def iter(toExplode: Set[Octopus], ocean: Map[Octopus, Int], exploded: Set[Octopus], flashes: Int): (Map[Octopus,Int], Int) = {
  if (toExplode.isEmpty) {
    (ocean.map(x => if (x._2 > 9) x._1 -> 0 else x), flashes)
  } else {
    val new_exploded = exploded + toExplode.head
    val affected_by_explosion = toExplode.head.getPointNeighbors(ocean)
    val updated_ocean = increment(ocean, affected_by_explosion)
    val new_to_explode = updated_ocean.filter{_._2 > 9}.keys.toSet -- new_exploded
    iter(new_to_explode, updated_ocean, new_exploded, flashes + 1)
  }
}

def round(accum: Int, ocean: Map[Octopus, Int], flashes: Int, max_rounds: Int): (Map[Octopus,Int], Int) = {
  if (max_rounds == accum) {
    (ocean, flashes)
  } else {
    val updated_ocean = increment(ocean)
    val new_to_explode = updated_ocean.filter{_._2 > 9}.keys.toSet
    val (ocean_after_explosions, flashes_after_explosions) = iter(new_to_explode, updated_ocean, Set(), flashes)
    if (ocean_after_explosions.forall(_._2 == 0)) {
      (ocean, accum)
    } else {
      round(accum + 1, ocean_after_explosions, flashes_after_explosions, max_rounds)
    }
  }
}

val result_part1 = round(0, ocean, 0, 100)._2

val result_part2 = round(1, ocean, 0,100000)._2