import scala.io.Source
val in = Source.fromFile(getClass.getResource("/inputs/03_input.txt").getFile)
val inputs = in.getLines().toList


def calculate_gamma_nth_bit(n: Int, lst: List[String])  = {
  if (lst.map(x => x(n) match {
    case '0' => 0
    case '1' => 1}).sum > lst.length/2) '1'
  else '0'
}

def calculate_gamma(lst: List[String]) = {
  (for (n <- lst.head.indices) yield calculate_gamma_nth_bit(n, lst)).toList.mkString
}

def bitwise_or(binary_str: String) = {
  (for (digit <- binary_str) yield digit match {
    case '0' => '1'
    case '1' => '0'
  }).toList.mkString
}

val gamma = calculate_gamma(inputs)
val epsilon = bitwise_or(gamma)
val power = Integer.parseInt(gamma,2)*Integer.parseInt(epsilon,2)


// part2
def calculate_nth_bit(n: Int, lst: List[String])(implicit op: (Int, Int) => Boolean)  = {
  val ones = lst.map(x => x(n) match {
    case '0' => 0
    case '1' => 1}).sum
  val zeros = lst.length-ones
  if (op(ones,zeros)) '1' else '0'
}

def calculate_metric(lst: List[String])(implicit op: (Int, Int) => Boolean ): String = {
  def iter(active: Int,remaining: List[String]): List[String] = {
    if (remaining.length == 1) remaining
    else iter(active+1, remaining.filter(x => x(active) == calculate_nth_bit(active, remaining)))
  }
  iter(0, lst).head
}

val ogr = calculate_metric(inputs)((ones, zeros) => ones >= zeros)
val csr = calculate_metric(inputs)((ones, zeros) => ones < zeros)
val lsr = Integer.parseInt(ogr,2)*Integer.parseInt(csr,2)








