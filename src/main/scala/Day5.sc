import scala.io.Source
import scala.math.{min,max,abs}

val in = Source.fromFile(getClass.getResource("/inputs/05_input.txt").getFile)
val inputs = in.getLines().toList

case class Point(x: Int, y: Int)
case class Line(x1: Int, y1: Int, x2: Int, y2: Int) {
  def getPoints: List[Point] = {
    if (x1==x2) (for (i <- min(y1,y2) to max(y1,y2)) yield Point(x1, i)).toList
    else if (y1==y2) (for (i <- min(x1,x2) to max(x1,x2)) yield Point(i, y1)).toList
    else if (abs(x1-x2)==abs(y1-y2))   {
      if (x2>x1) {
        if (y2>y1) (for (i <- x1 to x2) yield Point(i, y1+(i-x1))).toList
        else (for (i <- x1 to x2) yield Point(i, y1-(i-x1))).toList
      } else {
        if (y2>y1) (for (i <- x2 to x1) yield Point(i, y2-(i-x2))).toList
        else (for (i <- x2 to x1) yield Point(i, y2+(i-x2))).toList
      }
    } else throw new Exception("Neither horizontal, vertical or diagonal line")
  }
}

def parseInputs(inputs: List[String]): List[Line] = {
  inputs.map { line =>
    line.replace(" -> ", ",").split(",").toList match {
      case List(x1: String, y1: String, x2: String, y2: String) => Line(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
    }
  }
}

def updateOceanFloor(ocean_floor: Map[Point, Int], points: List[Point]): Map[Point, Int] = {
  def iter(active_point: Point, remaining_points: List[Point], updated_ocean_floor: Map[Point, Int]): Map[Point, Int] = {
    if (remaining_points.isEmpty) updated_ocean_floor + (active_point -> (updated_ocean_floor(active_point) + 1))
    else iter(remaining_points.head, remaining_points.tail, updated_ocean_floor + (active_point -> (updated_ocean_floor(active_point) + 1) ))
  }
  iter(points.head,points.tail, ocean_floor.withDefaultValue(0))
}

def countOverlap(ocean_floor: Map[Point, Int]): Int = {
  ocean_floor.foldLeft(0){case (accum,(Point(_,_),overlap_count)) => if (overlap_count > 1) accum+1 else accum}

}

def main_part1(inputs: List[String]):Int = {
  val lines = parseInputs(inputs)
  val points = lines.filter { case Line(x1,y1,x2,y2) => (x1==x2) || (y1==y2) }.flatMap(_.getPoints)
  val final_ocean_floor = updateOceanFloor(Map(), points)
  countOverlap(final_ocean_floor)
}

main_part1(inputs)

def main_part2(inputs: List[String]):Int = {
  val lines = parseInputs(inputs)
  val points = lines.filter { case Line(x1,y1,x2,y2) => (x1==x2) || (y1==y2) || (abs(x1-x2)==abs(y1-y2)) }.flatMap(_.getPoints)
  val final_ocean_floor = updateOceanFloor(Map(), points)
  countOverlap(final_ocean_floor)
}

main_part2(inputs)


//// Tests
parseInputs(inputs)

Line(21,268,30,268).getPoints
Line(30,268,21,268).getPoints
Line(21,268,21,275).getPoints
Line(21,275,21,268).getPoints
Line(21,275,21,268).getPoints
Line(705,62,702,65).getPoints
Line(702,65,705,62).getPoints
Line(702,62,705,65).getPoints
Line(705,65,702,62).getPoints

