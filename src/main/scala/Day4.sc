import scala.io.Source
val in = Source.fromFile(getClass.getResource("/inputs/04_input.txt").getFile)
val inputs = in.getLines().toList

type Row = List[(Int,Boolean)]
type Column = Row
type Board = List[Row]

def parse_inputs(inputs: List[String]): List[String] = {
  inputs.drop(1).map(_.trim)
    .map(_.replace("  "," "))
    .filterNot(_.equals(""))
}

def get_line_of_int(line: String): Row = {
  line.split(' ').toList.map(x => (x.toInt,false))
}

def get_board(input: List[String]): Board = {
  (for (i <- 0 until 5) yield get_line_of_int(input(i))).toList
}

def get_all_boards(input: List[String]): List[Board] = {
  if (input.isEmpty) List()
  else get_board(input) :: get_all_boards(input drop 5)
}

val input_draws = inputs.head.split(",").toList.map(_.toInt)
val input_boards: List[Board] = get_all_boards(parse_inputs(inputs))

def get_nth_row(n: Int, board: Board) = {
  board(n)
}
def get_nth_column(n: Int, board: Board): Column = {
  board.map(row => row(n))
}

def check_row_or_column(row_or_column: List[(Int,Boolean)]): Boolean = {
  row_or_column.forall(_._2 == true)
}

def check_board(board: Board): Boolean = {
  val row_solution = board.exists(row => check_row_or_column(row))
  val column_solution = (for (n <- 0 until 5) yield (check_row_or_column(get_nth_column(n, board)))).contains(true)
  row_solution || column_solution
}

def mark_board(draw: Int, board: Board): Board = {
  (for (n <- 0 until 5) yield get_nth_row(n, board).map {
    case (num, status) => if (draw == num) (num, true) else (num, status)
  }).toList.reverse
}

def sum_unmarked(board: Board): Int = {
  (for(row <- board;
      elem <- row;
      if !elem._2) yield elem._1).sum
}

val test_board = mark_board(37,input_boards.head)
sum_unmarked(input_boards.head)
sum_unmarked(test_board)
check_board(test_board)

//37 35 76 92 56
val test_board2 = mark_board(56,mark_board(92,mark_board(76,mark_board(35,test_board))))
check_board(test_board2)

//12 55 37 72 91
val test_board3 = mark_board(91,mark_board(72,mark_board(55,mark_board(12,test_board))))
check_board(test_board3)


def bingo(draws: List[Int], boards: List[Board]) = {
  def rounds(draw: Int, remaining_draws: List[Int], pending_boards: List[Board]): (List[Board], Int) = {

    def iter(active: Board, remaining: List[Board], accum: List[Board]): (List[Board], Int) = {
      val marked_board = mark_board(draw, active)
      if (check_board(marked_board)) (marked_board :: accum, draw * sum_unmarked(marked_board))
      else if (remaining.isEmpty) (marked_board :: accum, 0)
      else iter(remaining.head, remaining.tail, marked_board :: accum)
    }

    val (new_boards, solution) = iter(pending_boards.head, pending_boards.tail, List())
    if (solution > 0) (new_boards, solution)
    else if (remaining_draws.isEmpty) (new_boards, 0) else rounds(remaining_draws.head, remaining_draws.tail, new_boards)
  }

  rounds(draws.head, draws.tail, boards)
}

val (result_boards, solution) = bingo(input_draws, input_boards)
solution


def bingo_part2(draws: List[Int], boards: List[Board]): Int = {
  def rounds(draw: Int, remaining_draws: List[Int], pending_boards: List[Board]): (List[Board], Int) = {

    def iter(active: Board, remaining: List[Board], accum: List[Board]): (List[Board], Int) = {
      val marked_board = mark_board(draw, active)
      if (remaining.isEmpty) {
        if (check_board(marked_board)) (accum, draw * sum_unmarked(marked_board))
        else (marked_board :: accum, 0)
      } else if (check_board(marked_board)) iter(remaining.head, remaining.tail, accum)
        else iter(remaining.head, remaining.tail, marked_board :: accum)
    }

    val (new_boards, solution) = iter(pending_boards.head, pending_boards.tail, List())
    if (new_boards.isEmpty) (new_boards, solution)
    else if (remaining_draws.isEmpty) (new_boards, 0) else rounds(remaining_draws.head, remaining_draws.tail, new_boards)
  }

  val (_, solution) = rounds(draws.head, draws.tail, boards)
  solution
}

val solution =  bingo_part2(input_draws, input_boards)

