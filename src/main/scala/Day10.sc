import scala.io.Source

val in = Source.fromFile(getClass.getResource("/inputs/10_input.txt").getFile)
val inputs = in.getLines().toList

val closing: Map[Char, Char] = Map('{' -> '}', '(' -> ')', '[' -> ']', '<' -> '>')

val cost_corrupted: Map[Char, Int] = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)

val cost_incomplete: Map[Char, Int] = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)

def solve(remaining_text: String, pending_chunks: List[Char] = List()): (Long, Long) = {
  if (remaining_text.isEmpty) {
    (0L, pending_chunks.map(closing).foldLeft(0L)((accum,elem) => accum * 5 + cost_incomplete(elem)))
  } else {
    if (List('{', '(', '[', '<').contains(remaining_text.head)) {
      solve(remaining_text.tail, remaining_text.head :: pending_chunks)
    } else {
      if (remaining_text.head == closing(pending_chunks.head)) {
        solve(remaining_text.tail, pending_chunks.tail)
      } else {
        (cost_corrupted(remaining_text.head), 0L)
      }
    }
  }
}

val result_part1 = inputs.map(solve(_)).map(_._1).sum

val temp = inputs.map(solve(_)).map(_._2).filter(_ > 0).sorted
val result_part2 = temp(temp.length/2)