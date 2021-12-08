import scala.io.Source
import scala.math.{min,max,abs}

val in = Source.fromFile(getClass.getResource("/inputs/05_input.txt").getFile)
val inputs = in.getLines().toList

type point = (Int,Int)

def parseRemoveArrow(str: String): List[String] = {
  str.split(" -> ").toList
}

def parseCoordToPairInt(str: String): (Int,Int) = {
  val coord = str.split(",").toList
  (coord.head.toInt, coord.last.toInt)
}

def parseInputs(inputs: List[String]): List[(point,point)] = {
  inputs.map { line =>
    val temp = parseRemoveArrow(line).map(elem => parseCoordToPairInt(elem))
    (temp.head, temp.last)
  }
}

trait Line
case class HorizontalLine(line: (point,point)) extends Line
case class VerticalLine(line: (point,point)) extends Line
case class DiagonalLine(line: (point,point)) extends Line

def getLines(line_coords: List[(point,point)]): List[Line] = {
  line_coords.map{ line =>
    val (x1,y1) = line._1
    val (x2,y2) = line._2
    if (x1==x2) HorizontalLine(line)
    else if (y1==y2) VerticalLine(line)
    else if (abs(x1-x2)==abs(y1-y2)) DiagonalLine(line)
    else throw new Exception("Neither horizontal, vertical or diagonal line")
  }
}

def getPointsFromLine(line: Line): List[point]= {
  line match {
    case HorizontalLine(((x1,y1),(x2,y2))) =>
      (for (i <- min(y1,y2) to max(y1,y2)) yield (x1, i)).toList
    case VerticalLine(((x1,y1),(x2,y2))) =>
      (for (i <- min(x1,x2) to max(x1,x2)) yield (i, y1)).toList
    case DiagonalLine(((x1,y1),(x2,y2))) =>
      if (x2>x1) {
        if (y2>y1) (for (i <- x1 to x2) yield (i, y1+(i-x1))).toList
        else (for (i <- x1 to x2) yield (i, y1-(i-x1))).toList
      } else {
        if (y2>y1) (for (i <- x2 to x1) yield (i, y2-(i-x2))).toList
        else (for (i <- x2 to x1) yield (i, y2+(i-x2))).toList
      }
  }
}

def updateOceanFloor(ocean_floor: Map[(Int,Int), Int], points: List[point]): Map[(Int,Int), Int] = {
  def iter(active_point: point, remaining_points: List[point], updated_ocean_floor: Map[(Int,Int), Int]): Map[(Int,Int), Int] = {
    if (remaining_points.isEmpty) updated_ocean_floor + (active_point -> (updated_ocean_floor(active_point) + 1))
    else iter(remaining_points.head, remaining_points.tail, updated_ocean_floor + (active_point -> (updated_ocean_floor(active_point) + 1) ))
  }
  iter(points.head,points.tail, ocean_floor.withDefaultValue(0))
}

def countOverlap(ocean_floor: Map[(Int,Int), Int]): Int = {
  ocean_floor.foldLeft(0)((accum,point_overlaps) => if (point_overlaps._2 > 1) accum+1 else accum)
}

def main_part1(inputs: List[String]):Int = {
  val line_coords_input = parseInputs(inputs)
  val points = (for {line <- getLines(line_coords_input)
                     if {line match { case HorizontalLine(_) => true
                                      case VerticalLine(_) => true
                                      case _ => false}}}
                     yield getPointsFromLine(line)).flatten
  val final_ocean_floor = updateOceanFloor(Map(), points)
  countOverlap(final_ocean_floor)
}

main_part1(inputs)

def main_part2(inputs: List[String]): Int = {
  val line_coords_input = parseInputs(inputs)
  val points = (for (line <- getLines(line_coords_input)) yield getPointsFromLine(line)).flatten
  val final_ocean_floor = updateOceanFloor(Map(), points)
  countOverlap(final_ocean_floor)
}

main_part2(inputs)


// Tests
val line_coords_input = parseInputs(inputs)
val horizontal_lines = getLines(line_coords_input).filter{case HorizontalLine(_) => true;  case _ => false}
val vertical_lines = getLines(line_coords_input).filter{case VerticalLine(_) => true;  case _ => false}
val diagonal_lines = getLines(line_coords_input).filter{case DiagonalLine(_) => true;  case _ => false}

getPointsFromLine(VerticalLine((21,268),(30,268)))
getPointsFromLine(VerticalLine((30,268),(21,268)))
getPointsFromLine(HorizontalLine((21,268),(21,275)))
getPointsFromLine(HorizontalLine((21,275),(21,268)))
getPointsFromLine(HorizontalLine((21,275),(21,268)))
getPointsFromLine(DiagonalLine((705,62),(702,65)))
getPointsFromLine(DiagonalLine((702,65),(705,62)))
getPointsFromLine(DiagonalLine((702,62),(705,65)))
getPointsFromLine(DiagonalLine((705,65),(702,62)))

