
object Day15 extends App {

  import scala.annotation.tailrec
  import scala.io.Source
  import scala.math.{abs, min}

  val in = Source.fromFile(getClass.getResource("/inputs/15_input.txt").getFile)
  val inputs = in.getLines().toArray

  def parseInputs(inputs: Array[String]): Map[(Int, Int), Int] = {
    (for {y <- inputs.indices
          x <- inputs(y).indices}
    yield (y, x) -> inputs(x)(y).asDigit).toMap
  }

  case class Node(coordinates: (Int, Int), cost: Int) {
    def getCostWithHeuristic(end: (Int, Int)): Int = cost + abs(end._1 - coordinates._1) + abs(end._2 - coordinates._2)
  }

  def getNeighbors(coordinates: (Int, Int), field: Map[(Int, Int), Int], max_x: Int, max_y: Int, expanded: Map[(Int, Int), Int]): Set[(Int, Int)] = {
    Set((coordinates._1 + 1, coordinates._2), (coordinates._1 - 1, coordinates._2),
      (coordinates._1, coordinates._2 + 1), (coordinates._1, coordinates._2 - 1))
      .filter(x => (x._1 <= max_x) && x._1 >= 0 && x._2 <= max_y && x._2 >= 0)
      .filterNot(x => expanded.contains(x))
      //.map(x => Node(x, cost + field(x)))
    //      .collect { case x if visited(x) >= (cost + field(x)) => Node(x,cost + field(x))}
  }

  def solve(field: Map[(Int, Int), Int], start: (Int, Int), end: (Int, Int)): Int = {
    val x_max = field.keys.map(_._1).max
    val y_max = field.keys.map(_._2).max

    @tailrec
    def iter(prio_queue: Set[(Int, Int)], expanded: Map[(Int, Int), Int]): Int = {
      if (expanded.contains(end)) {
        expanded(end)
      } else {
        val min_node = prio_queue.minBy(expanded)
        val next = getNeighbors(min_node, field, x_max, y_max, expanded)
        val next_prio = (prio_queue - min_node) ++ next
        val next_visited = next.foldLeft(expanded)((accum, elem) => accum + (elem -> (accum(min_node) + field(elem))))
        iter(next_prio, next_visited)
      }
    }

    iter(Set(start), Map(start -> 0).withDefaultValue(10000000))
  }


  val field: Map[(Int, Int), Int] = parseInputs(inputs)

  def expand(point: ((Int, Int), Int), n: Int): Map[(Int, Int), Int] = {
    val (x, y) = point._1
    val z = point._2

    {
      for {i <- 0 until n
           j <- 0 until n} yield (x + (100 * i), y + (100 * j)) -> ((z - 1 + i + j) % 9 + 1)
    }.toMap
  }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0)/1000000000.0 + "s")
    result
  }

  time {

    val result_part1 = solve(field, (0, 0), (99, 99))

    val bigfield = field.flatMap(x => expand(x, 5))

    val result_part2 = solve(bigfield, (0, 0), (499, 499))

    println(result_part1)
    println(result_part2)
  }


}
