import scala.io.Source
import scala.math.{abs,max,min}

val in = Source.fromFile(getClass.getResource("/inputs/22_input.txt").getFile)
val inputs = in.getLines().toList

case class Command(on: Boolean, x: Int, xl: Int, y: Int, yl: Int, z: Int, zl: Int)

def parseInput(input: List[String]): List[Command] =
  input.map{
    case s"$on x=$x..$xr,y=$y..$yr,z=$z..$zr" =>
      Command(on == "on", x.toInt, xr.toInt - x.toInt + 1, y.toInt, yr.toInt - y.toInt + 1,z.toInt, zr.toInt - z.toInt + 1)
  }

val commands = parseInput(inputs)

val result_part1 = commands.filter(x => abs(x.x) <= 50).foldLeft(Map(): Map[(Int, Int, Int), Boolean]) {
  (accum, elem) =>
    elem match {
      case Command(on, x, xl, y, yl, z, zl) =>
        accum ++ (for {i <- 0 until xl
                       j <- 0 until yl
                       k <- 0 until zl}
        yield (x + i, y + j, z + k) -> on)
    }
}.count(_._2)


case class Cuboid(x: Int, xl: Int, y: Int, yl: Int, z: Int, zl: Int){
  def isOverlapping(other: Cuboid): Boolean = {
    other match {
      case Cuboid(x2, xl2, y2, yl2, z2, zl2) =>
        (x until x + xl).exists(xi => (x2 until x2 + xl2).contains(xi)) &&
          (y until y + yl).exists(yi => (y2 until y2 + yl2).contains(yi)) &&
          (z until z + zl).exists(zi => (z2 until z2 + zl2).contains(zi))
    }
  }

  def validate: Option[Cuboid] =
    if (xl > 0 && yl > 0 && zl > 0) {
      Some(Cuboid(x, xl, y, yl, z, zl))
    } else {
      None
    }

  def cutOutAndSplit(cuboid: Cuboid): List[Cuboid] = {
    cuboid match {
      case Cuboid(x2r, xl2r, y2r, yl2r, z2r, zl2r) =>
        val x2 = max(x,x2r)
        val xl2 = min(xl2r - (x2 - x2r), x + xl - x2)
        val y2 = max(y,y2r)
        val yl2 = min(yl2r - (y2 - y2r), y + yl - y2)
        val z2 = max(z,z2r)
        val zl2 = min(zl2r - (z2 - z2r), z + zl - z2)
        List(Cuboid(x           , x2 - x           , y           , yl               , z           , zl              ).validate,
             Cuboid(x2 + xl2    , x + xl - x2 - xl2, y           , yl               , z           , zl              ).validate,
             Cuboid(x2          , xl2              , y           , yl               , z2 + zl2    , z + zl - z2 -zl2).validate,
             Cuboid(x2          , xl2              , y           , y2 - y           , z           , z2 - z + zl2    ).validate,
             Cuboid(x2          , xl2              , y2 + yl2    , y + yl - y2 - yl2, z           , z2 - z + zl2    ).validate,
             Cuboid(x2          , xl2              , y2          , yl2              , z           , z2 - z          ).validate)
          .flatten
    }
  }
  def getVolume: Long = xl.toLong*yl.toLong*zl.toLong
}

val cuboids = commands.foldLeft(List(): List[Cuboid])((accum, elem) => elem match {
  case Command(on, x, xl, y, yl, z, zl) =>
    val new_cuboid = Cuboid(x, xl, y, yl, z, zl)
    (if (on) List(new_cuboid) else List()) ++
    accum.flatMap(cuboid => if (cuboid.isOverlapping(new_cuboid)) cuboid.cutOutAndSplit(new_cuboid) else List(cuboid))
  }
)
val result_part2 = cuboids.map(_.getVolume).sum