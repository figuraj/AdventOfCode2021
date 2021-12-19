import scala.annotation.tailrec
import scala.io.Source
import scala.math.abs

val in = Source.fromFile(getClass.getResource("/inputs/15_input.txt").getFile)
val inputs = in.getLines().toList

def parseInputs(inputs: List[String]): Map[(Int,Int), Int] = {
  (for { y <- inputs.indices
        x <- inputs(y).indices}
  yield (y,x) -> inputs(x)(y).asDigit).toMap
}

trait Node {
  def getNeighbors(field: Map[(Int,Int), Int]): Vector[Node]
  def isEnd(coordinates: (Int,Int)): Boolean
}

case class NodeNonEmpty(coordinates: (Int,Int), cost: Int, previous: Node) extends Node {
  override def getNeighbors(field: Map[(Int,Int), Int]): Vector[NodeNonEmpty] = {
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
      yield candidate_coordinates -> field.get(candidate_coordinates).toVector)
      .filter(_._2.length == 1)
      .map(x => NodeNonEmpty(x._1, cost + x._2.head, this)).toVector
  }

  override def isEnd(coordinates: (Int, Int)): Boolean = this.coordinates == coordinates
}

case class NodeEmpty(cost: Int = 0) extends Node {
  override def getNeighbors(field: Map[(Int, Int), Int]): Vector[NodeNonEmpty] =
    throw new Error("getNeighbors of NodeEmpty")

  override def isEnd(coordinates: (Int, Int)): Boolean =
    throw new Error("isEnd of NodeEmpty")
}

def insertIntoQueue(prio_queue: Vector[NodeNonEmpty], next: Vector[NodeNonEmpty]): Vector[NodeNonEmpty] = {
  implicit val NodeOrdering: Ordering[NodeNonEmpty] = Ordering.by(_.cost)
  def insertElem[T](x: T, lst: Vector[T])(implicit ord: Ordering[T]): Vector[T] = {
    lst match {
      case Vector() => Vector(x)
      case y +: ys =>
            if (ord.lteq(x,y)) {
              x +: lst
            } else {
              y +: insertElem(x, ys)
            }
        }
    }
//  def insertElem[T](x: T, lst: List[T], prefix: List[T])(implicit ord: Ordering[T]): List[T] = {
//    lst match {
//      case List() => prefix ::: List(x)
//      case y :: ys =>
//        if (ord.lteq(x,y)) {
//          prefix ::: x :: lst
//        } else {
//          insertElem(x, ys, List(y))
//        }
//    }
//  }

  if (prio_queue.isEmpty) {
    next.sorted
  } else {
    if (next.isEmpty){
      prio_queue
    } else {
      //next.sorted.foldLeft(prio_queue)((accum, elem) => insertElem(elem, accum))
      (next ++ prio_queue).sorted    }
  }
}


def solve(field: Map[(Int,Int), Int], start: (Int,Int), end: (Int,Int)): Int = {
  @tailrec
  def iter(prio_queue: Vector[NodeNonEmpty]): Int = {
//    println("-----")
//    println(prio_queue.head.isEnd(end))
    if (prio_queue.head.isEnd(end)) {
      prio_queue.head.cost
    } else {
      val next = prio_queue.head.getNeighbors(field)

//      println("head")
//      println(prio_queue.head.coordinates,prio_queue.head.cost)
//      println("Next:")
//      next.foreach(x => println(x.coordinates, x.cost))
//      println("prio_queue: ")
//      prio_queue.tail.foreach(x => println(x.coordinates, x.cost))

      val next_filtered = next.filterNot(x => prio_queue.exists(y => y.coordinates == x.coordinates))
      val new_prio_queue = insertIntoQueue(prio_queue.tail, next_filtered)
//      println("Newprio: ")
//      new_prio_queue.foreach(x => println(x.coordinates, x.cost))
      //println(new_prio_queue.length)
      iter(new_prio_queue)
    }
  }
  iter(Vector(NodeNonEmpty(start, 0, NodeEmpty())))
}

val field: Map[(Int,Int), Int] = parseInputs(inputs)

//val smallfield = field.filter(x => (x._1._1 < 10 && x._1._2 < 10))

//val end = (field.keySet.map(_._1).max, field.keySet.map(_._2).max)
val end = (99,99)
val result_part1 = solve(field, (0,0), end)

//val node1 = NodeNonEmpty((1,1),4, NodeEmpty())
//val node2 = NodeNonEmpty((1,2),10, NodeEmpty())
//val node3 = NodeNonEmpty((1,3),5, NodeEmpty())
//
//val node4 = NodeNonEmpty((1,4),7, NodeEmpty())
//val node5 = NodeNonEmpty((1,5),8, NodeEmpty())
//val node6 = NodeNonEmpty((1,6),9, NodeEmpty())
//
//insertIntoQueue(List(node4,node5,node6), List(node1,node2,node3))
//insertIntoQueue(List(node4,node5,node6), List())
//insertIntoQueue(List(), List(node1,node2,node3))


//val start = NodeNonEmpty((0,0),0, NodeEmpty())
//start.getNeighbors(field)
//
//val middle = NodeNonEmpty((40,10),0, NodeEmpty())
//middle.getNeighbors(field)
//
//val edge1 = NodeNonEmpty((99,99),0, NodeEmpty())
//edge1.getNeighbors(field)
//val edge2 = NodeNonEmpty((99,0),0, NodeEmpty())
//edge2.getNeighbors(field)
//val edge3 = NodeNonEmpty((0,99),0, NodeEmpty())
//edge3.getNeighbors(field)
//
//edge3.isEnd((0,99))
