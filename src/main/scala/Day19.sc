import scala.annotation.tailrec
import scala.io.Source
import scala.math.abs

val in = Source.fromFile(getClass.getResource("/inputs/19_input.txt").getFile)
val inputs = in.getLines().toList

case class Scanner(beacons: List[List[Int]])

@tailrec
def parseInputs(inputs: List[String], accum: List[Scanner] = List()): List[Scanner] = {
  val (scanner, rest) = inputs.span(_ != "")
  val new_scanner = Scanner(scanner.drop(1).map(x => x.split(",").toList.map(_.toInt)))
  if (rest.isEmpty) {
    new_scanner :: accum
  } else {
    parseInputs(rest.tail, new_scanner :: accum)
  }
}

@tailrec
def permutationsWithDuplicates(lst: List[Int],
                               permutations: List[List[Int]] = List(0,1,2).permutations.toList,
                               accum: List[List[Int]] = List()): List[List[Int]] = {
  if (permutations.isEmpty) {
    accum
  } else {
    val permutation = permutations.head
    permutationsWithDuplicates(lst, permutations.tail, (lst(permutation.head) :: lst(permutation(1)) :: lst(permutation.last) :: List() ) :: accum)
  }
}

@tailrec
def getAllRotations(coordinates: List[Int],
                    accum: List[List[Int]] = List(),
                    rot: List[List[Int]] = List(List(1,1,1),List(-1,1,1),List(1,-1,1),List(1,1,-1),
                      List(-1,-1,1), List(-1,1,-1), List(1,-1,-1), List(-1,-1,-1))): List[List[Int]] = {
  if (rot.isEmpty) {
    accum
  } else {
    val temp = (coordinates zip rot.head).map(x => x._1 * x._2)
    val new_accum = permutationsWithDuplicates(temp)
    getAllRotations(coordinates, new_accum ::: accum, rot.tail)
  }
}

@tailrec
def matchScanners(beacons: List[List[Int]], rotations: List[List[List[Int]]], accum: Int = 0): (List[Int], Int) = {
  if (rotations.isEmpty) {
    (List(),-1)
  } else {
    val diff =  beacons.flatMap(x => rotations.head.map(y => (x zip y).map(x => x._1 - x._2)))
    val x = diff.groupBy(identity).map(x => x._1 -> x._2.length).maxBy(_._2)
    if (x._2 >= 12) {
      (x._1, accum)
    } else {
      matchScanners(beacons,rotations.tail, accum + 1)
    }
  }
}

def maxManhattanDistance(scanner_locations: List[List[Int]]): Int =
  scanner_locations.combinations(2).toList.map(x => x.head.zip(x.last).map(y => abs(y._1 - y._2)).sum).max

def solve(scanners: List[Scanner]): (Int, Int) = {
  @tailrec
  def iter(remaining: List[Scanner], accum_beacons: List[List[Int]], accum_scanner_locations: List[List[Int]] = List()): (Int, Int) = {
    if (remaining.isEmpty) {
      (accum_beacons.length, maxManhattanDistance(accum_scanner_locations))
    } else {
      val scanner_all_rotations = remaining.head.beacons.map(x => getAllRotations(x)).transpose
      val (shift, rotation_index) = matchScanners(accum_beacons, scanner_all_rotations)
      if (rotation_index == -1) {
        iter(remaining.tail :+ remaining.head, accum_beacons, accum_scanner_locations)
      } else {
        val merged = (scanner_all_rotations(rotation_index).map(_.zip(shift).map(x => x._1 + x._2)) ++ accum_beacons).distinct
        iter(remaining.tail, merged, shift :: accum_scanner_locations)
      }
    }
  }
  iter(scanners.tail, scanners.head.beacons)
}

val scanners = parseInputs(inputs)
val (result_part1, result_part2) = solve(scanners)
