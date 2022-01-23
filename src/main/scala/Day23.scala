
object Day23 extends App{
  import scala.io.Source
  import scala.math.{abs, exp, max, min}

  val in = Source.fromFile(getClass.getResource("/inputs/23_input.txt").getFile)
  val inputs = in.getLines().toList

  val costs: Map[Char, Int] = Map('A' -> 1, 'B' -> 10,'C' -> 100, 'D' -> 1000)

  case class A(atype: Char, position: (Int,Int)){
    def isSameAmphipodTypeAtPosition(pos: (Int, Int), others: List[A]): Boolean = others.find(_.position == pos).get.atype == atype
    def cost: Int = costs(atype)
  }
  case class Game(state: Set[A])

  implicit class TupleOps(value: (Int,Int)) {
    def next: List[(Int,Int)] = {
      val (x,y) = value
      List((x + 1, y).validate,
        (x - 1, y).validate,
        (x, y - 1).validate,
        (x, y + 1).validate)
        .flatten
    }
    def validate: Option[(Int,Int)] = {
      val (x,y) = value
      if ((x >= 0 && x < 11 && y == 0) || ((y == 1 || y == 2) && List(2,4,6,8).contains(x)))
        Some((x,y))
      else None
    }

    def isOccupied(others: List[A]) = others.exists(amphipod => amphipod.position == value)

    def x: Int = value._1
    def y: Int = value._2

    def isAtTheDoorLevel = List(2,4,6,8).contains(value.x)
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

  // costs function not necessary here, simple number as param will suffice
  // parent method with iter
  def moveSingleAmphipod(amphipod: A, others: List[A], init_cost: Int): List[(Game, Int)] = {
    def iter(queued: List[((Int, Int), Int)], explored: List[(Int, Int)], accum: List[(Game, Int)]): List[(Game, Int)] = {
      if (queued.isEmpty) {
        accum.map(x => x._1 -> (init_cost + x._2*amphipod.cost))
      } else {
        val (position, score) = queued.head
        if (position.isOccupied(others)) {
          if (position.y == 2 && amphipod.isSameAmphipodTypeAtPosition(position, others) ) {
            if (amphipod.position != (position.x, 1))
              iter(queued.tail, position :: explored, (Game((amphipod.copy(position = (position.x, 1)) :: others).toSet) -> (score - 1)) :: accum)
              else iter(queued.tail, position :: explored, accum)
          } else {
            iter(queued.tail, position :: explored, accum)
          }
        } else if (position.isAtTheDoorLevel) {
          if (position.y < 2) {
            val new_next = position.next.filterNot(x => explored.contains(x)).map(pos => (pos, score + 1))
            iter(new_next ::: queued.tail, position :: explored, accum)
          } else {
            iter(queued.tail, position :: explored, (Game((amphipod.copy(position = position) :: others).toSet) -> score) :: accum)
          }
        } else {
          val new_next = position.next.filterNot(x => explored.contains(x)).map(x => (x, score + 1))
          iter(new_next ::: queued.tail, position :: explored, (Game((amphipod.copy(position = position) :: others).toSet) -> score) :: accum)
        }
      }
    }
    iter(amphipod.position.next.map(x =>(x,1)), List(amphipod.position), List())
  }

  def moveAllAmphipods(game: Game, init_score: Int): Map[Game, Int] = {
    game.state.foldLeft(List(): List[(Game,Int)])((accum, elem) =>
      accum ++ moveSingleAmphipod(elem, (game.state - elem).toList, init_score)).toMap
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
        visited(end) // change game to set of amphipod
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
//  val result_part1 = solve(Game(amphipods),end)
//  print(result_part1)

  val start = Game(Set(A('A',(2,1)),A('A',(4,2)),A('B',(4,1)),A('B',(2,2)),A('C',(6,1)),A('C',(6,2)),A('D',(5,0)),A('D',(8,2))))
  println(solve(start,end))
  println(heuristic(start))

}
