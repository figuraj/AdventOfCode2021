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


def getHorizontalLines(line_coords: List[(point,point)]): List[HorizontalLine] = {
  line_coords.filter{ line =>
    val (x1,y1) = line._1
    val (x2,y2) = line._2
    x1==x2
  }.map(line => HorizontalLine(line))
}

def getVerticalLines(line_coords: List[(point,point)]): List[VerticalLine] = {
  line_coords.filter{ line =>
    val (x1,y1) = line._1
    val (x2,y2) = line._2
    y1==y2
  }.map(line => VerticalLine(line))
}

def getDiagonalLines(line_coords: List[(point,point)]): List[DiagonalLine] = {
  line_coords.filter{ line =>
    val (x1,y1) = line._1
    val (x2,y2) = line._2
    abs(x1-x2)==abs(y1-y2)
  }.map(line => DiagonalLine(line))
}

val line_coords_input = parseInputs(inputs)
val horizontal_lines = getHorizontalLines(line_coords_input)
val vertical_lines = getVerticalLines(line_coords_input)
//val point_lines = getStraightLines(line_coords_input)((x1,y1,x2,y2) => x1==x2 && y1==y2)
val diagonal_lines = getDiagonalLines(line_coords_input)

//val ocean_floor: Map[Int,Map[Int, Int]] =

def getPointsFromLine(line: Line): List[point]= {
  line match {
    case HorizontalLine(line_coords) =>
      val (x1, y1) = line_coords._1
      val (x2, y2) = line_coords._2
      (for (i <- min(y1,y2) to max(y1,y2)) yield (x1, i)).toList
    case VerticalLine(line_coords) =>
      val (x1, y1) = line_coords._1
      val (x2, y2) = line_coords._2
      (for (i <- min(x1,x2) to max(x1,x2)) yield (i, y1)).toList
    case DiagonalLine(line_coords) =>
      val (x1, y1) = line_coords._1
      val (x2, y2) = line_coords._2
      if (x2>x1) {
        if (y2>y1) (for (i <- x1 to x2) yield (i, y1+(i-x1))).toList
        else (for (i <- x1 to x2) yield (i, y1-(i-x1))).toList
      } else {
        if (y2>y1) (for (i <- x2 to x1) yield (i, y2-(i-x2))).toList
        else (for (i <- x2 to x1) yield (i, y2+(i-x2))).toList
      }
  }
}

getPointsFromLine(VerticalLine((21,268),(30,268)))
getPointsFromLine(VerticalLine((30,268),(21,268)))
getPointsFromLine(HorizontalLine((21,268),(21,275)))
getPointsFromLine(HorizontalLine((21,275),(21,268)))
getPointsFromLine(HorizontalLine((21,275),(21,268)))
getPointsFromLine(DiagonalLine((705,62),(702,65)))
getPointsFromLine(DiagonalLine((702,65),(705,62)))
getPointsFromLine(DiagonalLine((702,62),(705,65)))
getPointsFromLine(DiagonalLine((705,65),(702,62)))

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

def main_part1(inputs: List[String]) = {
  val line_coords_input = parseInputs(inputs)
  val horizontal_lines = getHorizontalLines(line_coords_input)
  val vertical_lines = getVerticalLines(line_coords_input)
  val points_horizontal = (for (line <- horizontal_lines) yield getPointsFromLine(line)).flatten
  val points_vertical = (for (line <- vertical_lines) yield getPointsFromLine(line)).flatten
  val final_ocean_floor = updateOceanFloor(Map(),points_horizontal ::: points_vertical)
  countOverlap(final_ocean_floor)
}

main_part1(inputs)

def main_part2(inputs: List[String]) = {
  val line_coords_input = parseInputs(inputs)
  val horizontal_lines = getHorizontalLines(line_coords_input)
  val vertical_lines = getVerticalLines(line_coords_input)
  val diagonal_lines = getDiagonalLines(line_coords_input)
  val points_horizontal = (for (line <- horizontal_lines) yield getPointsFromLine(line)).flatten
  val points_vertical = (for (line <- vertical_lines) yield getPointsFromLine(line)).flatten
  val points_diagonal = (for (line <- diagonal_lines) yield getPointsFromLine(line)).flatten
  val final_ocean_floor = updateOceanFloor(Map(),points_horizontal ::: points_vertical ::: points_diagonal)
  countOverlap(final_ocean_floor)
}

main_part2(inputs)

