import scala.annotation.tailrec
import scala.io.Source
import scala.math.{abs, exp, max, min}

object Day23e extends App {

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

  def parseInputPart2(inputs: List[String]): Set[A] =
    parseInput(inputs).map(x => if (x.position.y == 2) x.copy(position = (x.position.x, 4)) else x) ++
    Set(A('D', (2, 2)), A('C', (4, 2)), A('B', (6, 2)), A('A', (8, 2)),
        A('D', (2, 3)), A('B', (4, 3)), A('A', (6, 3)), A('C', (8, 3)))

  val positions: List[(Int,Int)] = List((0,0),(1,0),(3,0),(5,0),(7,0),(9,0),(10,0),(2,1),(2,2),(4,1),(4,2),(6,1),(6,2),(8,1),(8,2))
  val moves = positions.groupBy(x => positions.filterNot(_ == x)).map(x => x._2.head -> x._1 )

  val positionsExtended: List[(Int,Int)] = List((0,0),(1,0),(3,0),(5,0),(7,0),(9,0),(10,0),(2,1),(2,2),(4,1),(4,2),(6,1),(6,2),(8,1),(8,2),
                                                                                           (2,3),(2,4),(4,3),(4,4),(6,3),(6,4),(8,3),(8,4))
  val movesExtended = positionsExtended.groupBy(x => positionsExtended.filterNot(_ == x)).map(x => x._2.head -> x._1 )


  def filterOutMoves(amphipod: A, others: List[A]): List[((Int, Int), Int)] = {
    val position: (Int, Int) = amphipod.position
    val others_pos: List[(Int, Int)] = others.map(_.position)
    val raw_moves = moves(position)
    //println(s"Position:  $position")
    //println(s"Others: $others")

    val temp: List[((Int, Int)) => Boolean] =
      ((x: (Int, Int))  => x.x == position.x) ::
        ((x: (Int, Int)) => x.y == 1 && !others.exists(_.position.x == x.x)) ::
        others.foldLeft(List(): List[((Int, Int)) => Boolean])((accum, oth) => {
          //println(accum)
          val elem = oth.position
          val temp: ((Int, Int)) => Boolean =
            if (position.x < elem.x) {
              if (elem.y == 0) x => x.x >= elem.x
              else x => (x.x == elem.x) && (x.y >= elem.y)
            } else if (position.x > elem.x) {
              if (elem.y == 0) x => x.x <= elem.x
              else x => (x.x == elem.x) && (x.y >= elem.y)
            } else { // position.x == elem.x, this can happen only at the door level of x
              if (elem.y == 0) x => true //accum.filterNot(x => x.x != elem.x) // position is at y > 0 thus blocked to go anywhere on x
              else if (elem.y == 1) x => true // position at y=2 and elem at y=1
              else x => x == elem // position at y=1 and elem at y=2
            }
          if (oth.atype != amphipod.atype && elem.y > 0) ((x: (Int, Int)) => x.x == elem.x) :: temp :: accum else temp :: accum
        })

    raw_moves
      .filterNot(x => temp.exists(y => y(x)))
      //.filterNot(x => !others_pos.exists(_.x == x.x && List(2,4,6,8).contains(x.x) && x.y == 1)) // browsing through candidates, if the candidate happens to be at 2,4,6 or 8 I have to
      // check if there are other amphipods at the same position, if there are no amphipods then I cannot move to position 1
      .map(x => x -> (abs(position.x - x.x) + (if (position.x == x.x) x.y else position.y + x.y)))
  }

//  def filterOutMoves(amphipod: A, others: List[A]): List[((Int, Int), Int)] = {
//    val position: (Int, Int) = amphipod.position
//    val others_pos: List[(Int, Int)] = others.map(_.position)
//    val raw_moves = movesExtended(position)
//    //println(s"Position:  $position")
//    //println(s"Others: $others")
//
//    val temp: List[((Int, Int)) => Boolean] =
//      ((x: (Int, Int)) => x.x == position.x) ::
//        ((x: (Int, Int)) => List(1,2,3).contains(x.y) && !others.exists(other => other.position.x == x.x && other.position.y > x.y)) ::
//          others.foldLeft(List(): List[((Int, Int)) => Boolean])((accum, oth) => {
//          //println(accum)
//          val elem = oth.position
//          val temp: ((Int, Int)) => Boolean =
//            if (position.x < elem.x) {
//              if (elem.y == 0) x => x.x >= elem.x
//              else x => (x.x == elem.x) && (x.y >= elem.y)
//            } else if (position.x > elem.x) {
//              if (elem.y == 0) x => x.x <= elem.x
//              else x => (x.x == elem.x) && (x.y >= elem.y)
//            } else { // position.x == elem.x, this can happen only at the door level of x
//              if (position.y > elem.y) x => true // if position is below other amphipod then its blocked to go out
//              else x => false // it can go anywhere (except the same x filtered elsewhere)
//            }
//          if (oth.atype != amphipod.atype && elem.y > 0) ((x: (Int, Int)) => x.x == elem.x) :: temp :: accum else temp :: accum
//        })
//
//    raw_moves
//      .filterNot(x => temp.exists(y => y(x)))
//      //.filterNot(x => !others_pos.exists(_.x == x.x && List(2,4,6,8).contains(x.x) && x.y == 1)) // browsing through candidates, if the candidate happens to be at 2,4,6 or 8 I have to
//      // check if there are other amphipods at the same position, if there are no amphipods then I cannot move to position 1
//      .map(x => x -> (abs(position.x - x.x) + (if (position.x == x.x) x.y else position.y + x.y)))
//  }

  def moveSingleAmphipod(amphipod: A, others: List[A], init_cost: Int): List[(Game, Int)] = {
    filterOutMoves(amphipod, others)
      //    .map(x => A(amphipod.atype,x._1) -> x._2)
      //    .map(x => Game((x._1 :: others).toSet) -> x._2)
      //    .map(x => x._1 -> (init_cost + x._2*amphipod.cost))
      .map(x => Game((A(amphipod.atype,x._1) :: others).toSet) -> (init_cost + x._2*amphipod.cost))
  }

  def moveAllAmphipods(game: Game, init_score: Int): List[(Game,Int)] = {
    game.state.foldLeft(List(): List[(Game,Int)])((accum, elem) => {
      //println(accum)
      moveSingleAmphipod(elem, (game.state - elem).toList, init_score) ++ accum
    })
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
    def iter(prio_queue: Set[Game], expanded: Map[Game, Int]): Int = {
      //println(prio_queue)
      if (expanded.contains(end)) {
        expanded(end)
      } else {
        val min_node = prio_queue.minBy(expanded)
        //println(min_node, expanded(min_node))
        val next = moveAllAmphipods(min_node, 0)
          .filterNot(x => expanded.contains(x._1))
        val next_prio = (prio_queue - min_node) ++ next.map(_._1)
        //val next_expanded = expanded ++ next
        val next_expanded = next.foldLeft(expanded)((accum, elem) => accum + (elem._1 -> (accum(min_node) + elem._2)))

        iter(next_prio, next_expanded)
      }
    }
    iter(Set(game),Map(game -> 0))
  }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0)/1000000000.0 + "s")
    result
  }

  val end = Game(Set(A('A',(2,1)),A('A',(2,2)),A('B',(4,1)),A('B',(4,2)),A('C',(6,1)),A('C',(6,2)),A('D',(8,1)),A('D',(8,2))))
  val amphipods: Set[A] = parseInput(inputs)

  time {
    val result_part1 = solve(Game(amphipods),end)
    print(result_part1)}

  val start = Game(Set(A('A',(2,1)),A('A',(4,2)),A('B',(4,1)),A('B',(2,2)),A('C',(5,0)),A('C',(6,2)),A('D',(7,0)),A('D',(8,2))))

//    time {println(solve(start,end))
//          println(heuristic(start))}

  val startExtended = Game(Set(A('A',(2,1)),A('A',(4,2)),A('B',(4,1)),A('B',(2,2)),A('C',(6,1)),A('C',(6,2)),A('D',(5,0)),A('D',(8,2)),
                                A('A',(2,3)),A('A',(2,4)),A('B',(4,3)),A('B',(4,4)),A('C',(6,3)),A('C',(6,4)),A('D',(8,3)),A('D',(8,4))))
  val endExtended = Game(Set(A('A',(2,1)),A('A',(2,2)),A('B',(4,1)),A('B',(4,2)),A('C',(6,1)),A('C',(6,2)),A('D',(8,1)),A('D',(8,2)),
                             A('A',(2,3)),A('A',(2,4)),A('B',(4,3)),A('B',(4,4)),A('C',(6,3)),A('C',(6,4)),A('D',(8,3)),A('D',(8,4))))

//  time {println(solve(startExtended,endExtended))
//    println(heuristic(startExtended))}

//  val amphipodsExtended: Set[A] = parseInputPart2(inputs)
//
//  time {
//      val result_part2 = solve(Game(amphipodsExtended),endExtended)
//      print(result_part2)}
}

