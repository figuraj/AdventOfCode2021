import scala.io.Source
import scala.math.{round,abs,min,max}
val in = Source.fromFile(getClass.getResource("/inputs/07_input.txt").getFile)
val inputs = in.getLines().next()

def parseInputs(inputs: String): List[Int] = {
  inputs.split(",").map(_.toInt).toList
}
val crab_inputs = parseInputs(inputs)
//val crab_inputs = List(16,1,2,0,4,2,7,1,2,14)

val sorted_crabs = crab_inputs.sorted
val crabs_count = sorted_crabs.length
val median_index = if (crabs_count % 2 == 0) List(crabs_count/2, crabs_count/2-1) else List(crabs_count/2)

val result_part1 = (for (median_candidate <- median_index) yield
  crab_inputs.foldLeft(0)((accum, crab) => accum + abs(crab-sorted_crabs(median_candidate)))).min


sorted_crabs(5)
crab_inputs.sum/5
def fuel(from: Int, to: Int): Int = {
  def iter(from: Int, to: Int, accum: Int): Int = {
    if (from==to) accum else iter(from+1, to, accum+(to-from))
  }
  iter(min(from,to),max(from,to),0)
}
def cost_function(lst: List[Int], x: Int) = lst.foldLeft(0)((accum, elem) => accum+fuel(elem,x))

(for (x <- crab_inputs.min to crab_inputs.max) yield (cost_function(crab_inputs,x))).min

