import scala.io.Source

val in = Source.fromFile(getClass.getResource("/inputs/09_input.txt").getFile)
val inputs = in.getLines().toList.map(line => line.map(_.asDigit).toList)//.map(_.asDigit)

case class Point(x: Int, y: Int, height: Int) {
  def getCoordinates: (Int,Int) = (x,y)
  def <(num: Int): Boolean = this.height < num
}

case class Field(points: Set[Point])  {
  def getPoint(x: Int, y: Int): Point = {
    points.filter {
      case Point(x1,y1, height) => x==x1 && y==y1
    }.head
  }

  def getPointNeighbors(x: Int, y: Int): Set[Point] = {
    val candidates = Set((x - 1,y),(x + 1,y),(x,y - 1),(x,y + 1))
    candidates
      .filter(p => p._1 >= 0 && p._1 <=99 && p._2 >=0 && p._2 <= 99)
      .map(x => getPoint(x._1,x._2))
  }
  def getPointNeighbors(point: Point): Set[Point] = {
    getPointNeighbors(point.x,point.y)
  }
}

val field = Field((for (x <- 0 to 99; y <- 0 to 99) yield Point(x,y,inputs(x)(y))).toSet)
//field.getPointNeighbors(1,1)

val low_points = (for {i <- 0 to 99
                       j <- 0 to 99
                       if field.getPointNeighbors(i,j).forall(point => point.height > field.getPoint(i,j).height)}
yield field.getPoint(i,j)).toList

val result_part1 = low_points.map(_.height + 1).sum

def getBasin(low_point: Point): Set[Point] = {
  def iter(unchecked: Set[Point], discarded: Set[Point], accepted: Set[Point] ): Set[Point] = {
    if (unchecked.isEmpty) {
      accepted
    } else {
      if (unchecked.head < 9) {
        val neighbors = field.getPointNeighbors(unchecked.head)
        iter(unchecked.tail ++ neighbors -- discarded -- accepted, discarded, accepted ++ Set(unchecked.head))
      } else {
        iter(unchecked.tail, discarded ++ Set(unchecked.head), accepted)
      }
    }
  }
  iter(Set(low_point),Set(),Set())
}

getBasin(low_points.head).size

low_points.map(getBasin).map(_.size).sorted.takeRight(3).product
