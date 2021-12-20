import scala.annotation.tailrec
import scala.io.Source
import scala.math.abs

val in = Source.fromFile(getClass.getResource("/inputs/15_input.txt").getFile)
val inputs = in.getLines().toList

val end = (99,99)

def parseInputs(inputs: List[String]): Map[(Int,Int), Int] = {
  (for { y <- inputs.indices
        x <- inputs(y).indices}
  yield (y,x) -> inputs(x)(y).asDigit).toMap
}

case class Node(coordinates: (Int,Int), cost: Int, previous: (Int,Int)) {
  def getNeighbors(field: Map[(Int,Int), Int], prio_queue: List[Node], visited: List[Node]): List[Node] = {
    List((coordinates._1 + 1, coordinates._2),     (coordinates._1 - 1, coordinates._2),
         (coordinates._1    , coordinates._2 + 1), (coordinates._1    , coordinates._2 - 1))
      .filter(x => (x._1 <= 99) && x._1 >= 0 && x._2 <= 99 && x._2 >= 0)
      .filterNot(x => x == previous || prio_queue.exists(y => x == y.coordinates) || visited.exists(y => x == y.coordinates))
      .map(x => Node(x, cost + field(x), coordinates))
  }

  def isEnd(coordinates: (Int, Int)): Boolean = this.coordinates == coordinates

  def getCostWithHeuristic(end: (Int, Int)): Int = cost + abs(end._1 - coordinates._1) + abs(end._2 - coordinates._2)
}

implicit val NodeOrdering: Ordering[Node] = Ordering.by(_.getCostWithHeuristic(end))

def solve(field: Map[(Int,Int), Int], start: (Int,Int), end: (Int,Int)): Int = {
  @tailrec
  def iter(prio_queue: List[Node], visited: List[Node]): Int = {
    if (prio_queue.head.isEnd(end)) {
      prio_queue.head.cost
    } else {
      val next = prio_queue.head.getNeighbors(field, prio_queue, visited)
      val next_prio = (next ++ prio_queue.tail).sorted
      iter(next_prio, prio_queue.head +: visited)
    }
  }
  iter(List(Node(start, 0, (-1,-1))),List())
}

val field: Map[(Int,Int), Int] = parseInputs(inputs)
val result_part1 = solve(field, (0,0), end)