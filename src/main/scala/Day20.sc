import scala.annotation.tailrec
import scala.io.Source

val in = Source.fromFile(getClass.getResource("/inputs/20_input.txt").getFile)
val inputs = in.getLines().toList

def parsePixelString(input: String): List[Int] = {
  input.map {
    case '#' => 1
    case '.' => 0
  }.toList
}

def parseImage(inputs: List[String]): Map[(Int,Int), Int] = {
  inputs
    .map(x => parsePixelString(x).zipWithIndex)
    .zipWithIndex
    .flatMap(y => y._1.map(x => (x._2, y._2) -> x._1))
    .filter(_._2 == 1)
    .toMap
}

def enhanceImage(image: Map[(Int,Int), Int], infinite_light: Boolean)
                (implicit image_enhancement_algorithm: List[Int]): Map[(Int,Int), Int] = {
  val min_x = image.minBy(_._1._1)._1._1 - 2
  val max_x = image.maxBy(_._1._1)._1._1 + 2
  val min_y = image.minBy(_._1._2)._1._2 - 2
  val max_y = image.maxBy(_._1._2)._1._2 + 2

  val image_with_default = if (infinite_light) image.withDefaultValue(1) else image.withDefaultValue(0)

  (for { y <- min_y to max_y
         x <- min_x to max_x}
    yield {
      val binary_str = (for {iy <- -1 to 1
                             jx <- -1 to 1} yield image_with_default(x + jx, y + iy)).mkString
      val index = Integer.parseInt(binary_str, 2)
      (x,y) -> image_enhancement_algorithm(index)
    }).filter(x => (!infinite_light && x._2 == 0) || (infinite_light && x._2 == 1) ).toMap
}

@tailrec
def solve(image: Map[(Int,Int), Int], max_rounds: Int, rounds: Int = 0)
         (implicit image_enhancement_algorithm: List[Int]): Int = {
  if (rounds == max_rounds) {
    image.size
  } else {
    solve(enhanceImage(image, rounds % 2 != 0), max_rounds, rounds + 1)
  }
}

implicit val image_enhancement_algorithm: List[Int] = parsePixelString(inputs.head)
val image = parseImage(inputs.drop(2))
val result_part1 = solve(image, 2)
val result_part2 = solve(image, 50)