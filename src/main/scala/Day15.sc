import scala.annotation.tailrec
import scala.io.Source
import scala.math.abs

val in = Source.fromFile(getClass.getResource("/inputs/15_input.txt").getFile)
val inputs = in.getLines().toArray

def parseInputs(inputs: Array[String]): Map[(Int,Int), Int] = {
  (for { y <- inputs.indices
        x <- inputs(y).indices}
  yield (y,x) -> inputs(x)(y).asDigit).toMap
}

case class Node(coordinates: (Int,Int), cost: Int) {
  def getNeighbors(field: Map[(Int,Int), Int], max_x: Int, max_y: Int, prio_queue: Array[Node], visited: Array[Node]): Array[Node] = {
    Array((coordinates._1 + 1, coordinates._2),     (coordinates._1 - 1, coordinates._2),
         (coordinates._1    , coordinates._2 + 1), (coordinates._1    , coordinates._2 - 1))
      .filter(x => (x._1 <= max_x) && x._1 >= 0 && x._2 <= max_y && x._2 >= 0)
      .filterNot(x => prio_queue.exists(y => x == y.coordinates) || visited.exists(y => x == y.coordinates))
      .map(x => Node(x, cost + field(x)))
  }
  def isEnd(coordinates: (Int, Int)): Boolean = this.coordinates == coordinates

  def getCostWithHeuristic(end: (Int, Int)): Int = cost + abs(end._1 - coordinates._1) + abs(end._2 - coordinates._2)
}

//implicit val NodeOrdering: Ordering[Node] = Ordering.by(_.getCostWithHeuristic(end))

def solve(field: Map[(Int,Int), Int], start: (Int,Int), end: (Int,Int)): Int = {
  val x_max = field.keys.map(_._1).max
  val y_max = field.keys.map(_._2).max

  @tailrec
  def iter(prio_queue: Array[Node], visited: Array[Node]): Int = {
    val min_node = prio_queue.minBy(_.getCostWithHeuristic(end))
    //val min_node = prio_queue.minBy(_.cost)
    if (min_node.isEnd(end)) {
      min_node.cost
    } else {
      val next = min_node.getNeighbors(field, x_max, y_max , prio_queue, visited)
      val next_prio = (next ++ prio_queue.filterNot(_ == min_node))
      iter(next_prio, prio_queue.head +: visited)
    }
  }
  iter(Array(Node(start, 0)),Array())
}


val field: Map[(Int,Int), Int] = parseInputs(inputs)

def expand(point: ((Int,Int), Int), n: Int): Map[(Int,Int), Int] = {
  val (x,y) = point._1
  val z = point._2

  {for { i <- 0 until n
        j <- 0 until n } yield (x + (100 * i), y + (100 * j)) -> ((z - 1 + i + j) % 9 + 1)}.toMap
}

val result_part1 = solve(field, (0,0), (99,99))

val bigfield = field.flatMap(x => expand(x,2))

val result_part2 = solve(bigfield, (0,0), (199,199))




