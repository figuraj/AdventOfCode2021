import scala.io.Source
val in = Source.fromFile(getClass.getResource("/inputs/04_input.txt").getFile)
val inputs = in.getLines().toList

type Row = List[Int]
type Board = List[Row]

def parse_inputs(inputs: List[String]): List[String] = {
  inputs.drop(1).map(_.trim)
    .map(_.replace("  "," "))
    .filterNot(_.equals(""))
}

def get_line_of_int(line: String): Row = {
  line.split(' ').toList.map(_.toInt)
}

def get_board(input: List[String]): Board = {
  (for (i <- 0 until 5) yield get_line_of_int(input(i))).toList
}

def get_all_boards(input: List[String]): List[Board] = {
  if (input.isEmpty) List()
  else get_board(input) :: get_all_boards(input drop 5)
}

val input_rounds = inputs.head.split(",").toList
val input_boards: List[Board] = get_all_boards(parse_inputs(inputs))

