
import scala.io.Source
import scala.math.{abs, exp, max, min}

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

//#############
//#0123456789A#  0(A = 10)
//###A#D#B#C###  1
//  #B#C#D#A#    2
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

val positions: List[(Int,Int)] = List((0,0),(1,0),(3,0),(5,0),(7,0),(9,0),(10,0),(2,1),(2,2),(4,1),(4,2),(6,1),(6,2),(8,1),(8,2))
val moves = positions.groupBy(x => positions.filterNot(_ == x)).map(x => x._2.head -> x._1 )

def filterOutMoves(position: (Int,Int), others: List[(Int,Int)]): List[((Int,Int), Int)] = {
  val raw_moves = moves(position)
  //println(s"Position:  $position")
  //println(s"Others: $others")
  val temp = others.foldLeft(raw_moves: List[(Int,Int)])((accum, elem) => {
    //println(accum)
    if (position.x < elem.x) { // 2,1
      if (elem.y == 0) accum.filterNot(x => x.x >= elem.x)
      else accum.filterNot(x => (x.x == elem.x) && (x.y >= elem.y))
    } else if (position.x > elem.x) {
      if (elem.y == 0) accum.filterNot(x => x.x <= elem.x)
      else accum.filterNot(x => (x.x == elem.x) && (x.y >= elem.y))
    } else { // position.x == elem.x, this can happen only at the door level of x
      if (elem.y == 0) accum.filterNot(x => x.x != elem.x) // position is at y > 0 thus blocked to go anywhere on x
      else if (elem.y == 1)  List() // position at y=2 and elem at y=1
      else accum.filterNot(_ == elem) // position at y=1 and elem at y=2
    }
  }).map(x => x -> (abs(position.x-x.x) + (if (position.x == x.x) x.y else position.y + x.y)))
  //println(temp)
  temp
}

def moveSingleAmphipod(amphipod: A, others: List[A], init_cost: Int): List[(Game, Int)] = {
  val temp = filterOutMoves(amphipod.position, others.map(_.position))
   // println(s"Temp1 $temp")
  val temp2 = temp.map(x => A(amphipod.atype,x._1) -> x._2)
  //println(s"Temp2 $temp2")
   val temp3 = temp2.map(x => Game((x._1 :: others).toSet) -> x._2)
  //println(s"Temp3 $temp3")
   val temp4 = temp3.map(x => x._1 -> (init_cost + x._2*amphipod.cost))
  //println(s"Temp4 $temp4")
  temp4
}

def moveAllAmphipods(game: Game, init_score: Int): Map[Game, Int] = {
  game.state.foldLeft(List(): List[(Game,Int)])((accum, elem) => {
    //println(accum)
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
  def iter(prio_queue: Map[Game, Int], visited: Map[Game, Int]): Int = {
    if (prio_queue.isEmpty || visited(end) < 10000000 ) {
      visited(end)
    } else {
      val min_node = prio_queue.minBy(x => heuristic(x._1) + x._2)
      //val min_node = prio_queue.minBy(_._2)
      //println(min_node)
      val next = moveAllAmphipods(min_node._1, min_node._2) // all new games generated from this game
        .filterNot(x => visited(x._1) < x._2) // some of the new games that are of lower cost or completely new
        .filterNot(x => prio_queue.getOrElse(x._1,10000000) < x._2)
      val next_prio = prio_queue.removed(min_node._1) ++ next
      iter(next_prio, visited.updated(min_node._1, min(min_node._2,visited(min_node._1))))
    }
  }
  iter(Map((game,0)).withDefaultValue(100000000),Map().withDefaultValue(10000000))
}

val end = Game(Set(A('A',(2,1)),A('A',(2,2)),A('B',(4,1)),A('B',(4,2)),A('C',(6,1)),A('C',(6,2)),A('D',(8,1)),A('D',(8,2))))
val amphipods: Set[A] = parseInput(inputs)
val result_part1 = solve(Game(amphipods),end)
print(result_part1)

//val start = Game(Set(A('A',(2,1)),A('A',(4,2)),A('B',(4,1)),A('B',(2,2)),A('C',(6,1)),A('C',(6,2)),A('D',(5,0)),A('D',(8,2))))
//println(solve(start,end))
//println(heuristic(start))


