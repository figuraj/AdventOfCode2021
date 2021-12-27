import scala.annotation.tailrec
import scala.io.Source

val in = Source.fromFile(getClass.getResource("/inputs/17_input.txt").getFile)
val inputs = in.getLines().toList.head

val temp = inputs.split(" ")
val temp2 = temp(2).init.drop(2).split("\\..").map(_.toInt)
val target_area_x_min = temp2.head
val target_area_x_max = temp2.last
val temp3 = temp(3).drop(2).split("\\..").map(_.toInt)
val target_area_y_min = temp3.head
val target_area_y_max = temp3.last

@tailrec
def maxHeight(start: Int, accum: Int = 0): Int = {
  if (start == 0) {
    accum
  } else {
    maxHeight(start-1, accum + start)
  }
}

val result_part1 = maxHeight(-target_area_y_min-1)

def getYsolutions(min_target: Int, max_target: Int): List[(Int,Int)] = {
  @tailrec
  def shoot(velocity: Int, distance: Int = 0, steps: Int = 0, solutions: List[Int] = List()): List[Int] = {
    val new_solutions = if ((min_target to max_target).contains(distance)) steps :: solutions else solutions
    if (velocity < min_target) {
      new_solutions
    } else {
      shoot(velocity-1, distance + velocity, steps + 1, new_solutions)
    }
  }

  (for {i <- min_target until -min_target
        temp = shoot(i)
        if temp.nonEmpty} yield temp.map(x => (i,x))).flatten.toList
}

def getXsolutions(min_target: Int, max_target: Int): List[(Int,Int)] = {
  @tailrec
  def shoot(velocity: Int, distance: Int = 0, steps: Int = 0, solutions: List[Int] = List()): List[Int] = {
    val new_solutions = if ((min_target to max_target).contains(distance)) steps :: solutions else solutions
    if (velocity < 0) {
      if ((min_target to max_target).contains(distance)) {
        Range(steps + 1, steps + 1000, 1).toList ::: new_solutions
      } else {
        new_solutions
      }
    } else {
      shoot(velocity-1, distance + velocity, steps + 1, new_solutions)
    }
  }

  (for {i <- 1 to max_target
       temp = shoot(i)
       if temp.nonEmpty} yield temp.map(x => (i,x))).flatten.toList
}

val y_candidates = getYsolutions(target_area_y_min,target_area_y_max)
val x_candidates = getXsolutions(target_area_x_min,target_area_x_max)

val solutions =
  y_candidates.flatMap(y =>
    x_candidates.flatMap(x =>
      if (x._2 == y._2) List((x._1,y._1, x._2)) else List()))

val unique_solutions = solutions
  .map(x => (x._1,x._2))
  .foldLeft(List(): List[(Int,Int)]) ((accum, elem) => if (accum.contains(elem)) accum else elem :: accum)

val result_part2 = unique_solutions.length