import scala.io.Source
import scala.math.{round,abs,min,max}
val in = Source.fromFile(getClass.getResource("/inputs/07_input.txt").getFile)
val inputs = in.getLines().next()

def parseInputs(inputs: String): List[Int] = {
  inputs.split(",").map(_.toInt).toList
}

def getMedian(lst: List[Int]): Int = {
  val sorted_lst = lst.sorted
  if (sorted_lst.length % 2 == 1) sorted_lst(sorted_lst.length/2)
  else round((sorted_lst(sorted_lst.length/2)+sorted_lst(sorted_lst.length/2+1))/2)
}

val crab_inputs = parseInputs(inputs)
val results_part1 = crab_inputs.foldLeft(0)((accum, elem) => accum + abs(elem-getMedian(crab_inputs)))

// **** Part 2
implicit def fuel(from: Int, to: Int): Int = {
  def iter(from: Int, to: Int, accum: Int): Int = {
    if (from==to) accum else iter(from+1, to, accum+(to-from))
  }
  iter(min(from,to),max(from,to),0)
}
def cost_function(lst: List[Int], x: Int)(implicit f: (Int,Int) => Int): Int =
  lst.foldLeft(0)((accum, elem) => accum+f(elem,x))

def bruteForceSolver(lst: List[Int])(cost_function: (List[Int], Int) => Int): Int = {
  (for (x <- lst.min to lst.max) yield cost_function(lst,x)).min
}
val results_part2 = bruteForceSolver(crab_inputs)(cost_function)

