import scala.io.Source

val in = Source.fromFile(getClass.getResource("/inputs/09_input.txt").getFile)
val inputs = in.getLines().toList.map(line => line.map(_.asDigit).toList)//.map(_.asDigit)

case class Point(x: Int, y: Int, height: Int) {
  def getCoordinates: (Int, Int) = (x, y)

  def <(num: Int): Boolean = this.height < num

  def <(that: Point): Boolean = this.height < that.height
}

def getPoint(x: Int, y: Int, field: Set[Point]): Point = {
  field.filter {
    case Point(x1, y1, height) => x == x1 && y == y1
  }.head
}

def getPointNeighbors(point: Point, field: Set[Point]): Set[Point] = {
  val (x, y) = point match {case Point(x,y,_) => (x,y)}
  val candidates = Set((x - 1,y),(x + 1,y),(x,y - 1),(x,y + 1))
  candidates
    .filter(p => (0 to 99).contains(p._1) && (0 to 99).contains(p._2))
    .map(x => getPoint(x._1,x._2, field))
}

val field: Set[Point] = (for (x <- 0 to 99; y <- 0 to 99) yield Point(x,y,inputs(x)(y))).toSet

val low_points = (for {i <- 0 to 99
                       j <- 0 to 99
                       point = getPoint(i,j,field)
                       if getPointNeighbors(point, field).forall(point < _)}
yield point).toList

val result_part1 = low_points.map(_.height + 1).sum

def getBasin(low_point: Point): Set[Point] = {
  def iter(unchecked: Set[Point], discarded: Set[Point], accepted: Set[Point] ): Set[Point] = {
    if (unchecked.isEmpty) {
      accepted
    } else {
      if (unchecked.head < 9) {
        val neighbors = getPointNeighbors(unchecked.head, field)
        iter(unchecked.tail ++ neighbors -- discarded -- accepted, discarded, accepted + unchecked.head)
      } else {
        iter(unchecked.tail, discarded + unchecked.head, accepted)
      }
    }
  }
  iter(Set(low_point),Set(),Set())
}

val result_part2 = low_points.map(getBasin).map(_.size).sorted.takeRight(3).product
