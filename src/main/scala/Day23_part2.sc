import scala.annotation.tailrec
import scala.io.Source
import scala.math.{abs, min}


val in = Source.fromFile(getClass.getResource("/inputs/23_input.txt").getFile)
val inputs = in.getLines().toList

val costs: Map[Char, Int] = Map('A' -> 1, 'B' -> 10,'C' -> 100, 'D' -> 1000)

case class A(atype: Char, position: (Int,Int)){
  def cost: Int = costs(atype)
}
case class Game(state: Set[A])

implicit class TupleOps(value: (Int,Int)) {
  def x: Int = value._1
  def y: Int = value._2
}

//#############
//#...........#
//###A#D#B#C###
//  #B#C#D#A#
//  #########


//(0,0),(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0),(9,0),(10,0)
//            (2,1)       (4,1)       (6,1)       (8,1)
//            (2,2)       (4,2)       (6,2)       (8,2)

def parseInput(inputs: List[String]): Set[A] = {
  (inputs(2) match {
    case s"###$a1#$a2#$a3#$a4###" =>
      Set(A(a1.head,(2,1)),
        A(a2.head,(4,1)),
        A(a3.head,(6,1)),
        A(a4.head,(8,1)))
  }) ++
    (inputs(3) match {
      case s"  #$a1#$a2#$a3#$a4#" =>
        Set(A(a1.head,(2,2)),
          A(a2.head,(4,2)),
          A(a3.head,(6,2)),
          A(a4.head,(8,2)))
    })
}

//#############
//#...........#
//###A#D#B#C###
//  #D#C#B#A#
//  #D#B#A#C#
//  #B#C#D#A#
//  #########

def parseInputPart2(inputs: List[String]): Set[A] =
  parseInput(inputs).map(x => if (x.position.y == 2) x.copy(position = (x.position.x, 4)) else x) ++
    Set(A('D', (2, 2)), A('C', (4, 2)), A('B', (6, 2)), A('A', (8, 2)),
      A('D', (2, 3)), A('B', (4, 3)), A('A', (6, 3)), A('C', (8, 3)))

val positionsExtended: List[(Int,Int)] = List((0,0),(1,0),(3,0),(5,0),(7,0),(9,0),(10,0),
  (2,1),(2,2),(4,1),(4,2),(6,1),(6,2),(8,1),(8,2),
  (2,3),(2,4),(4,3),(4,4),(6,3),(6,4),(8,3),(8,4))
val movesExtended = positionsExtended.groupBy(x => positionsExtended.filterNot(_ == x)).map(x => x._2.head -> x._1 )


def filterOutMoves(amphipod: A, others: List[A]): List[((Int, Int), Int)] = {
  val position: (Int, Int) = amphipod.position
  val raw_moves = movesExtended(position)

  val temp: List[((Int, Int)) => Boolean] =
    ((x: (Int, Int)) => x.x == position.x) ::
      ((x: (Int, Int)) => List(1,2,3).contains(x.y) && !others.exists(other => other.position.x == x.x && other.position.y > x.y)) ::
      others.foldLeft(List(): List[((Int, Int)) => Boolean])((accum, oth) => {
        val elem = oth.position
        val temp: ((Int, Int)) => Boolean =
          if (position.x < elem.x) {
            if (elem.y == 0) x => x.x >= elem.x
            else x => (x.x == elem.x) && (x.y >= elem.y)
          } else if (position.x > elem.x) {
            if (elem.y == 0) x => x.x <= elem.x
            else x => (x.x == elem.x) && (x.y >= elem.y)
          } else { // position.x == elem.x, this can happen only at the door level of x
            if (position.y > elem.y) x => true // if position is below other amphipod then its blocked to go out
            else x => false // it can go anywhere (except the same x filtered elsewhere)
          }
        if (oth.atype != amphipod.atype && elem.y > 0) ((x: (Int, Int)) => x.x == elem.x) :: temp :: accum else temp :: accum
      })

  raw_moves
    .filterNot(x => temp.exists(y => y(x)))
    .map(x => x -> (abs(position.x - x.x) + (if (position.x == x.x) x.y else position.y + x.y)))
}

def moveSingleAmphipod(amphipod: A, others: List[A], init_cost: Int): List[(Game, Int)] = {
  filterOutMoves(amphipod, others)
    .map(x => Game((A(amphipod.atype,x._1) :: others).toSet) -> (init_cost + x._2*amphipod.cost))
}

def moveAllAmphipods(game: Game, init_score: Int): Map[Game, Int] = {
  game.state.foldLeft(List(): List[(Game,Int)])((accum, elem) => {
    accum ++ moveSingleAmphipod(elem, (game.state - elem).toList, init_score)
  }).toMap
}

def heuristic(here: Game): Int = {
  here.state.foldLeft(0)((accum, elem) =>
    accum + (elem match {
      case A('A', position) => costs('A') * (abs(position.x - 2) + (if (position.x == 2) 0 else 1 + position.y))
      case A('B', position) => costs('B') * (abs(position.x - 4) + (if (position.x == 4) 0 else 1 + position.y))
      case A('C', position) => costs('C') * (abs(position.x - 6) + (if (position.x == 6) 0 else 1 + position.y))
      case A('D', position) => costs('D') * (abs(position.x - 8) + (if (position.x == 8) 0 else 1 + position.y))
    })
  )
}

def solve(game: Game, end: Game): Int = {
  @tailrec
  def iter(prio_queue: Map[Game, Int], visited: Map[Game, Int]): Int = {
    if (visited(end) < 10000000) {
      visited(end)
    } else {
      val min_node = prio_queue.minBy(x => heuristic(x._1) + x._2)
      val next = moveAllAmphipods(min_node._1, min_node._2) // all new games generated from this game
        .filterNot(x => visited(x._1) < x._2) // some of the new games that are of lower cost or completely new
        .filterNot(x => prio_queue.getOrElse(x._1,10000000) < x._2)
      val next_prio = prio_queue.removed(min_node._1) ++ next
      iter(next_prio, visited.updated(min_node._1, min(min_node._2,visited(min_node._1))))
    }
  }
  iter(Map((game,0)).withDefaultValue(100000000),Map().withDefaultValue(10000000))
}

val startExtended = Game(Set(A('A',(2,1)),A('A',(4,4)),A('B',(4,1)),A('B',(2,2)),A('C',(6,1)),A('C',(6,2)),A('D',(5,0)),A('D',(8,2)),
  A('A',(2,3)),A('A',(2,4)),A('B',(4,3)),A('B',(4,2)),A('C',(6,3)),A('C',(6,4)),A('D',(8,3)),A('D',(8,4))))
val endExtended = Game(Set(A('A',(2,1)),A('A',(2,2)),A('B',(4,1)),A('B',(4,2)),A('C',(6,1)),A('C',(6,2)),A('D',(8,1)),A('D',(8,2)),
  A('A',(2,3)),A('A',(2,4)),A('B',(4,3)),A('B',(4,4)),A('C',(6,3)),A('C',(6,4)),A('D',(8,3)),A('D',(8,4))))

//solve(startExtended,endExtended)
//heuristic(startExtended)

val amphipodsExtended: Set[A] = parseInputPart2(inputs)
val result_part2 = solve(Game(amphipodsExtended),endExtended)