import scala.io.Source

val in = Source.fromFile(getClass.getResource("/inputs/12_input.txt").getFile)
val inputs = in.getLines().toList

def parseInputs(inputs: List[String]): Map[String, List[String]] = {
  val splitted = inputs.map(_.split("-"))
  val left_to_right = splitted.map(x => x.head -> x(1)).groupBy(_._1).map(x => x._1 -> x._2.map(_._2))
  val right_to_left = splitted.map(x => x(1) -> x.head).groupBy(_._1).map(x => x._1 -> x._2.map(_._2))

  (left_to_right.keySet ++ right_to_left.keySet)
    .map{ key => key -> (left_to_right.get(key).toList ::: right_to_left.get(key).toList).flatten}
    .toMap
  }

val pathmap: Map[String, List[String]] = parseInputs(inputs)

def checkPathPart1(path: List[String]): Boolean = {
  path.filter(x => x.toLowerCase() == x).groupBy(identity).map(_._2.length).forall(_ == 1)
}

def checkPathPart2(path: List[String]): Boolean = {
  val node_to_count = path.filter(x => x.toLowerCase() == x).groupBy(identity).map(x => x._1 -> x._2.length).withDefaultValue(0)
  val node_counts = node_to_count.values.toList
  node_counts.count(_ != 1) <= 1 && node_counts.filter(_ > 1).forall(_ <= 2) &&  node_to_count("end") < 2
}

def iter(endpath: List[String])(implicit checkPath: List[String] => Boolean): Int = {
  if (endpath.head == "start") {
    1
  } else {
    val forks = pathmap(endpath.head)
    val new_paths = (for {fork <- forks
                          candidate = fork :: endpath
                          path = if (checkPath(candidate)) candidate else List()} yield path).filterNot(_.isEmpty)
    if (new_paths.isEmpty) {
      0
    } else {
      (for (path <- new_paths) yield iter(path)).sum
    }
  }
}

val result_part1 = iter(List("end"))(checkPathPart1)
val result_part2 = iter(List("end"))(checkPathPart2)

