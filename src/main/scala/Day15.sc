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

case class Node(coordinates: (Int,Int), cost: Int) {
  def getNeighbors(field: Map[(Int,Int), Int], previous: (Int,Int)): Array[Node] = {
    Array((coordinates._1 + 1, coordinates._2),     (coordinates._1 - 1, coordinates._2),
         (coordinates._1    , coordinates._2 + 1), (coordinates._1    , coordinates._2 - 1))
      .filter(x => (x._1 <= 99) && x._1 >= 0 && x._2 <= 99 && x._2 >= 0)
      .filterNot(_ == previous)
      .map(x => Node(x, cost + field(x)))
  }

  def isEnd(coordinates: (Int, Int)): Boolean = this.coordinates == coordinates

  def getCostWithHeuristic(end: (Int, Int)): Int = cost + abs(end._1 - coordinates._1) + abs(end._2 - coordinates._2)
}

implicit val NodeOrdering: Ordering[Node] = Ordering.by(_.getCostWithHeuristic(end))

def insertIntoQueue(prio_queue: Array[Node], next: Array[Node]): Array[Node] = {
  (next ++ prio_queue).sorted
}

def solve(field: Map[(Int,Int), Int], start: (Int,Int), end: (Int,Int)): Int = {
  @tailrec
  def iter(prio_queue: Array[Node], previous: (Int,Int)): Int = {
    if (prio_queue.head.isEnd(end)) {
      prio_queue.head.cost
    } else {
      val next = prio_queue.head.getNeighbors(field, previous)
      val next_filtered = next.filterNot{ x => prio_queue.exists{y =>
        y.coordinates == x.coordinates}  } //|| (previous == x.coordinates)
      iter(insertIntoQueue(prio_queue.tail, next_filtered),prio_queue.head.coordinates)
    }
  }
  iter(Array(Node(start, 0)),(-1,-1))
}

val field: Map[(Int,Int), Int] = parseInputs(inputs)
val result_part1 = solve(field, (0,0), end)