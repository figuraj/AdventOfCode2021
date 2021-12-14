import scala.io.Source

val in = Source.fromFile(getClass.getResource("/inputs/10_input.txt").getFile)
val inputs = in.getLines().toList

def closing(char: Char): Char = {
  char match {
    case '{' => '}'
    case '(' => ')'
    case '[' => ']'
    case '<' => '>'
  }
}

def cost(char: Char): Int = {
  char match {
    case ')' => 3
    case ']' => 57
    case '}' => 1197
    case '>' => 25137
    case _ => 0
  }
}

def iter(remaining_text: String, pending_chunks: List[Char]): Int = {
  if (remaining_text.isEmpty) {
    0
  } else {
    if (List('{', '(', '[', '<').contains(remaining_text.head)) {
      iter(remaining_text.tail, remaining_text.head :: pending_chunks)
    } else {
      if (remaining_text.head == closing(pending_chunks.head)) {
        iter(remaining_text.tail, pending_chunks.tail)
      } else {
        cost(remaining_text.head)
      }
    }
  }
}

inputs.map(line => iter(line,List())).sum

def cost2(char: Char): Int = {
  char match {
    case ')' => 1
    case ']' => 2
    case '}' => 3
    case '>' => 4
    case _ => 0
  }
}

def iter2(remaining_text: String, pending_chunks: List[Char]): Long = {
  if (remaining_text.isEmpty) {
    pending_chunks.map(closing).foldLeft(0L)((accum,elem) => accum * 5 + cost2(elem))
  } else {
    if (List('{', '(', '[', '<').contains(remaining_text.head)) {
      iter2(remaining_text.tail, remaining_text.head :: pending_chunks)
    } else {
      if (remaining_text.head == closing(pending_chunks.head)) {
        iter2(remaining_text.tail, pending_chunks.tail)
      } else {
        0
      }
    }
  }
}

val temp = inputs.map(line => iter2(line,List())).filter(_ > 0).sorted
val result_part2 = temp(temp.length/2)