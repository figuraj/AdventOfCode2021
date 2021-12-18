import scala.io.Source

val in = Source.fromFile(getClass.getResource("/inputs/13_input.txt").getFile)
val inputs = in.getLines().toList

def parseInputsDots(inputs: List[String]): Map[(Int,Int), Boolean] = {
  inputs
    .filterNot(_.contains("fold along"))
    .filterNot(_.equals(""))
    .map { line =>
    val coordinates = line.split(",")
    ((coordinates.head.toInt, coordinates(1).toInt), true)
  }.toMap.withDefaultValue(false)
}

def parseInputsFolds(inputs: List[String]): List[(Char,Int)] = {
  inputs.filter(_.contains("fold along"))
    .map{line =>
       val temp = line.split("=")
      (temp.head.last, temp.last.toInt)
    }
}

def mergeMapsWithDefault[K,V,U](map1: Map[K, V], map2: Map[K,V], default: U)(op: (V, V) => U): Map[K,U] = {
  (map1.keySet ++ map2.keySet).map(key => key -> op(map1(key), map2(key))).toMap.withDefaultValue(default)
}

def foldLeft(paper: Map[(Int,Int), Boolean], foldline: Int): Map[(Int,Int), Boolean] = {
  val to_be_folded = paper.filter(_._1._1 > foldline).filter(x => x._2)
  val folded = to_be_folded.map(x => (2*foldline-x._1._1, x._1._2) -> x._2).withDefaultValue(false)
  mergeMapsWithDefault(paper, folded, false)(_ || _).filter(_._1._1 < foldline)
}

def foldUp(paper: Map[(Int,Int), Boolean], foldline: Int): Map[(Int,Int), Boolean] = {
  val to_be_folded = paper.filter(_._1._2 > foldline).filter(x => x._2)
  val folded = to_be_folded.map(x => (x._1._1, 2*foldline-x._1._2) -> x._2).withDefaultValue(false)
  mergeMapsWithDefault(paper, folded, false)(_ || _).filter(_._1._2 < foldline)
}

def foldPaper(instructions: List[(Int,Int)], paper: Map[(Int,Int), Boolean]): Map[(Int,Int), Boolean] = {
  if (instructions.isEmpty) {
    paper
  } else {
    instructions.head match {
      case (0, foldline) => foldPaper(instructions.tail, foldLeft(paper, foldline))
      case (1, foldline) => foldPaper(instructions.tail, foldUp(paper, foldline))
    }
  }
}

val paper = parseInputsDots(inputs)
val instructions = parseInputsFolds(inputs).map(x => if (x._1 =='x') 0 -> x._2 else 1 -> x._2)

val result_part1 = foldPaper(List(instructions.head), paper).size

// Part 2
val folded = foldPaper(instructions, paper)

// Result Part 2
(for {y <- 0 to folded.keySet.map(_._2).max} yield {
  (for {x <-  0 to folded.keySet.map(_._1).max} yield if (folded((x,y))) "@" else " ").toList.mkString
}).toList.foreach(println)