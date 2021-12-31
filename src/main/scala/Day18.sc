import scala.annotation.tailrec
import scala.io.Source

val in = Source.fromFile(getClass.getResource("/inputs/18_input.txt").getFile)
val inputs = in.getLines().toList

case class SnailNumber(sn: List[(Int,Int)]) {
  def + (other: SnailNumber): SnailNumber = {
    SnailNumber((sn ::: other.sn).map(x => (x._1 + 1, x._2)))
  }

  @tailrec
  final def explode: SnailNumber = {
    val (left, rest) = sn.span(_._1 < 5)
    if (rest.isEmpty) {
      this.split
    } else {
      val (exploding_pair, right) = rest match { case x :: y :: xs => (List(x,y), xs)}
      val (explode_left, explode_right) = (exploding_pair.head._2, exploding_pair.last._2)

      val new_left = if (left.isEmpty) {
        left
      } else if (left.length == 1) {
        List((left.head._1, left.head._2 + explode_left))
      } else {
        val last = left.last
        left.init :+ (last._1, last._2 + explode_left)
      }

      val new_right = if (right.isEmpty) {
        right
      } else {
        (right.head._1, right.head._2 + explode_right) :: (if (right.length > 1) right.tail else List())
      }

      SnailNumber(new_left ::: ((exploding_pair.head._1 - 1, 0) :: new_right)).explode
    }

  }

  def split: SnailNumber = {
    val (left, rest) = sn.span(_._2 < 10)
    if (rest.isEmpty) {
      this
    } else {
      val (number_to_split, right) = rest match { case x :: xs => (x, xs)}
      val depth = number_to_split._1 + 1
      val left_value = number_to_split._2 / 2
      val right_value = number_to_split._2 - left_value
      SnailNumber(left ::: (depth, left_value) :: (depth, right_value) :: right).explode
    }
  }

  @tailrec
  final def magnitude(n: Int = 4): Int = {
    if (n == 0) {
      sn.map(_._2).sum
    } else {
      val (left, rest) = sn.span(_._1 < n)
      if (rest.isEmpty) {
        this.magnitude(n-1)
      } else {
        val (pair_left, pair_right, right) = rest match {
          case x :: y :: xs => (x, y, xs)
        }
        val new_left = left :+ (pair_left._1 - 1, 3 * pair_left._2 + 2 * pair_right._2)
        if (right.isEmpty) {
          SnailNumber(new_left).magnitude(n - 1)
        } else {
          SnailNumber(new_left ::: right).magnitude(n)
        }
      }
    }
  }

}

def parseSnailNumber(str: String): SnailNumber = {
  @tailrec
  def iter(depth: Int, remaining: List[Char], accum: List[(Int,Int)]): SnailNumber = {
    if (remaining.isEmpty) {
      SnailNumber(accum)
    } else {
      val char = remaining.head
      char match {
        case '[' => iter(depth + 1, remaining.tail, accum)
        case ']' => iter(depth - 1, remaining.tail, accum)
        case x: Char => iter(depth, remaining.tail, accum :+ (depth, x.asDigit))
      }
    }
  }
  iter(0,str.replace(",","").toList, List())
}

def parseInputs(inputs: List[String]): List[SnailNumber] = {
  inputs.map(x => parseSnailNumber(x))
}

val snailNumbers = parseInputs(inputs)

val sum_of_all = snailNumbers.tail.foldLeft(snailNumbers.head)((accum, elem) => (accum + elem).explode)
val result_part1 = sum_of_all.magnitude()

val combinations = snailNumbers.combinations(2).toList
val combinations_both_directions = combinations ::: combinations.map(_.reverse)
val result_part2 = combinations_both_directions.map(x => (x.head + x.last).explode).map(_.magnitude()).max