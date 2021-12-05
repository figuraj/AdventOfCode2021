import scala.io.Source
//val in = Source.fromURL("https://adventofcode.com/2021/day/1/input")
val in = Source.fromFile(getClass.getResource("/inputs/01_input.txt").getFile)
val inputs = in.getLines().toList.map(_.toInt)

def get_depth_increases(inputs: List[Int]) = {
  def iter(active: Int, remaining: List[Int], accum: Int): Int= {
    if (remaining.isEmpty) accum
    else if (remaining.head > active) iter(remaining.head, remaining.tail,accum+1)
    else iter(remaining.head,remaining.tail,accum)
  }
  iter(inputs.head, inputs.tail, 0)
}

get_depth_increases(inputs)

def get_depth_increases2(inputs: List[Int]) = {
  (for (i <- 1 until inputs.length if inputs(i) > inputs(i-1)) yield 1).sum
}

get_depth_increases2(inputs)

def get_depth_increases3(inputs: List[Int])= {
  inputs.foldLeft((-1,0))((accum,elem) => if (elem > accum._2) (accum._1+1,elem) else (accum._1,elem))._1
}

get_depth_increases3(inputs)

def get_depth_increases4(inputs: List[Int])= {
  inputs.zip(inputs.drop(1)).count(pair => pair._2 > pair._1)

}
get_depth_increases4(inputs)


def get_depth_increases_with_windowing(inputs: List[Int]) = {
  val window = (inputs zip inputs.drop(1)).map(pair => pair._1+pair._2).zip(inputs.drop(2)).map(pair => pair._1+pair._2)
  get_depth_increases4(window)
}

get_depth_increases_with_windowing(inputs)

def get_depth_increases_with_windowing2(inputs: List[Int]) = {
  val window = (for (i <- 3 to inputs.length) yield inputs.slice(i - 3, i).sum).toList
  get_depth_increases4(window)
}

get_depth_increases_with_windowing2(inputs)
