import scala.annotation.tailrec
import scala.io.Source
import scala.math.{abs,pow,sqrt}

val in = Source.fromFile(getClass.getResource("/inputs/15_input.txt").getFile)
val inputs = in.getLines().toList

def parseInputs(inputs: List[String]): Map[(Int,Int), Int] = {
  (for { y <- inputs.indices
        x <- inputs(y).indices}
  yield (y,x) -> inputs(x)(y).asDigit).toMap
}

trait Node {
  def getNeighbors(field: Map[(Int,Int), Int]): Array[Node]
  def isEnd(coordinates: (Int,Int)): Boolean
  def getCostWithHeuristic(end: (Int, Int)): Int
  def getCost: Int
}

case class NodeNonEmpty(coordinates: (Int,Int), cost: Int, previous: Node) extends Node {
  override def getNeighbors(field: Map[(Int,Int), Int]): Array[Node] = {
    val previous_coordinates = previous match {
      case NodeNonEmpty(x,_,_) => x
      case _ => (-1,-1)
    }
    (for {i <- List(-1,0,1)
          j <- List(-1,0,1)
          candidate_coordinates = (coordinates._1 + i, coordinates._2 + j)
          if (previous_coordinates != candidate_coordinates) &&
            (abs(i) + abs(j) == 1)
          }
      yield candidate_coordinates -> field.get(candidate_coordinates).toArray)
      .filter(_._2.length == 1)
      .map(x => NodeNonEmpty(x._1, cost + x._2.head, this)).toArray
  }

  override def isEnd(coordinates: (Int, Int)): Boolean = this.coordinates == coordinates

  def getCostWithHeuristic(end: (Int, Int)): Int = cost + abs(end._1 - coordinates._1) + abs(end._2 - coordinates._2)
    //sqrt(pow(end._1 - coordinates._1,2).toInt + pow((end._2 - coordinates._2),2).toInt).toInt

  def getCost: Int = this.cost
}

case class NodeEmpty(cost: Int = 0) extends Node {
  override def getNeighbors(field: Map[(Int, Int), Int]): Array[Node] =
    throw new Error("getNeighbors of NodeEmpty")

  override def isEnd(coordinates: (Int, Int)): Boolean =
    throw new Error("isEnd of NodeEmpty")

  override def getCostWithHeuristic(end: (Int, Int)): Int =
    throw new Error("getCostWithHeuristic of NodeEmpty")

  override def getCost: Int =
    throw new Error("getCost of NodeEmpty")
}

def insertIntoQueue(prio_queue: Array[Node], next: Array[Node], end: (Int, Int)): Array[Node] = {
  //implicit val NodeOrdering: Ordering[NodeNonEmpty] = Ordering.by(_.cost)
  implicit val NodeOrdering: Ordering[Node] = Ordering.by(_.getCostWithHeuristic(end))

//  val prio_and_next =  (next ++ prio_queue)
//  val prio_min = prio_and_next.min
//  prio_min +: prio_and_next.filterNot(_ == prio_min)

//  if (next.isEmpty){
//    prio_queue
//  } else {
//    (next ++ prio_queue).sorted
//  }
  (next ++ prio_queue).sorted
}

def solve(field: Map[(Int,Int), Int], start: (Int,Int), end: (Int,Int)): Int = {
  @tailrec
  def iter(prio_queue: Array[Node]): Int = {
    if (prio_queue.head.isEnd(end)) {
      prio_queue.head.getCost
    } else {
      val next = prio_queue.head.getNeighbors(field)
      val next_filtered = next.filterNot{ case NodeNonEmpty(x,_,_) => prio_queue.exists{ case NodeNonEmpty(y,_,_) => y == x}}
      iter(insertIntoQueue(prio_queue.tail, next_filtered, end))
    }
  }
  iter(Array(NodeNonEmpty(start, 0, NodeEmpty())))
}

val field: Map[(Int,Int), Int] = parseInputs(inputs)
val end = (99,99)
val result_part1 = solve(field, (0,0), end)