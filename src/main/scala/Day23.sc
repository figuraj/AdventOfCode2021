import scala.io.Source
import scala.math.{abs, exp, max, min}

val in = Source.fromFile(getClass.getResource("/inputs/23_input.txt").getFile)
val inputs = in.getLines().toList

val costs: Map[Char, Int] = Map('A' -> 1, 'B' -> 10,'C' -> 100, 'D' -> 1000)

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
    if ((x >= 0 && x < 11 && y == 0) || ((y > 0) && List(2,4,6,8).contains(x)))
      Some((x,y))
    else None
  }
}

case class A(atype: Char, position: (Int,Int))
case class Game(state: Set[A])

//#############
//#...........#
//###A#D#B#C###
//  #B#C#D#A#
//  #########

def parseInput(inputs: List[String]): List[A] = {
  (inputs(2) match {
    case s"###$a1#$a2#$a3#$a4###" => List(A(a1.head,(2,1)),
                                          A(a2.head,(4,1)),
                                          A(a3.head,(6,1)),
                                          A(a4.head,(8,1)))
  }) :::
  (inputs(3) match {
    case s"  #$a1#$a2#$a3#$a4#"   => List(A(a1.head,(2,2)),
                                          A(a2.head,(4,2)),
                                          A(a3.head,(6,2)),
                                          A(a4.head,(8,2)))
  })
}

def moveSingleAmphipod(queued: List[((Int,Int), Int)], explored: List[(Int,Int)], accum: List[(Game, Int)], active: A, others: List[A]): List[(Game, Int)] = {
  if (queued.isEmpty) {
    accum
  } else {
    if (others.exists(x => x.position == queued.head._1)){
      if (queued.head._1._2 == 2 && active.atype == others.filter(_.position == (queued.head._1._1, 2)).head.atype){
        moveSingleAmphipod(queued.tail, queued.head._1 :: explored, (Game((active.copy(position = (queued.head._1._1, 1)) :: others).toSet) -> queued.head._2) :: accum, active, others)
      } else {
        moveSingleAmphipod(queued.tail, queued.head._1 :: explored, accum, active, others)
      }
    } else if (List(2,4,6,8).contains(queued.head._1._1)){
      if (queued.head._1._2 == 0) {
        val new_next = queued.head._1.next.filterNot(x => explored.contains(x)).map(x => (x, queued.head._2 + costs(active.atype)))
        moveSingleAmphipod(new_next ::: queued.tail, queued.head._1 :: explored, accum, active, others)
      } else if (queued.head._1._2 == 1){
        val new_next = queued.head._1.next.filterNot(x => explored.contains(x)).map(x => (x, queued.head._2 + costs(active.atype)))
        moveSingleAmphipod(new_next ::: queued.tail, queued.head._1 :: explored, accum, active, others)
      } else {
        moveSingleAmphipod(queued.tail, queued.head._1 :: explored, (Game((active.copy(position = queued.head._1) :: others).toSet) -> queued.head._2) :: accum, active, others)
      }
    } else {
      val new_next = queued.head._1.next.filterNot(x => explored.contains(x)).map(x => (x, queued.head._2 + costs(active.atype)))
      moveSingleAmphipod(new_next ::: queued.tail, queued.head._1 :: explored, (Game((active.copy(position = queued.head._1) :: others).toSet) -> queued.head._2) :: accum, active, others)
    }
  }
}
//val round = moveSingleAmphipod(amphipods.head.position.next.map(x => (x, 1)), List(amphipods.head.position), List(), amphipods.head)
//
//round.foreach(println)
//
//println(round(1)._1.state.head.position)
//val round2 = moveSingleAmphipod(round(1)._1.state.head.position.next.map(x => (x, 1)), List(round(1)._1.state.head.position), List(), round(1)._1.state.head )
//round2.foreach(println)

//val round1 = moveAllAmphipods(amphipods)
//round1.foreach(println)
//round1.size

def moveAllAmphipods(game: Game, init_score: Int): Map[Game, Int] = {
  game.state.foldLeft(List(): List[(Game,Int)])((accum, elem) =>
    moveSingleAmphipod(elem.position.next.map(x =>
      (x,1)), List(game.state.head.position), List(), elem, (game.state - elem).toList) ::: accum).map(x => x._1 -> (x._2 + init_score)).toMap
}


def solve(game: Game, end: Game): Int = {
  def iter(prio_queue: Map[Game, Int], visited: Map[Game, Int]): Int = {
    if (prio_queue.isEmpty || visited(end) < 10000000 ) {
      visited(end) // change game to set of amphipod
    } else {
      val min_node = prio_queue.minBy(_._2)
      val next = moveAllAmphipods(min_node._1, min_node._2) // all new games generated from this game
                              .filterNot(x => visited(x._1) < x._2) // some of the new games that are of lower cost or completely new
                              .filterNot(x => prio_queue.getOrElse(x._1,10000000) < x._2)
      val next_prio = next ++ prio_queue.filterNot(x => x == min_node)
      iter(next_prio, visited.updated(min_node._1, min(min_node._2,visited(min_node._1))))
    }
  }
  iter(Map((game,0)).withDefaultValue(100000000),Map().withDefaultValue(10000000))
}

//val end = Game(Set(A('A',(2,1)),A('A',(2,2)),A('B',(4,1)),A('B',(4,2)),A('C',(6,1)),A('C',(6,2)),A('D',(8,1)),A('D',(8,2))))


val amphipods: List[A] = parseInput(inputs)
//val others = amphipods.tail
//             List(A(A,(2,1)),  A(D,(4,1)),     A(B,(6,1)), A(C,(8,1)),    A(B,(2,2)),  A(C,(4,2)),  A(D,(6,2)),     A(A,(8,2)))
val end = Game(Set(A('A',(2,1)), A('D',(4,1)), A('B',(5,0)), A('C',(8,1)), A('B',(2,2)), A('C',(4,2)), A('D',(6,2)), A('A',(8,2))))

val result_part1 = solve(Game(amphipods.toSet),end)
