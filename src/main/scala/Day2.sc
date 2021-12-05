import scala.io.Source
val in = Source.fromFile(getClass.getResource("/inputs/02_input.txt").getFile)
val inputs = in.getLines().toList


def prepare_inputs(inputs: List[String]) = {
  inputs.map(instruction => instruction.split(" ").toList)
        .map(instruction => (instruction.head,instruction(1).toInt))
}

def run_instruction(instruction: (String, Int)) = {
  instruction match {
    case ("forward", num: Int) => (num, 0)
    case ("up", num: Int) => (0, -num)
    case ("down", num: Int) => (0, num)
  }
}

def follow_instructions(instructions: List[(String,Int)]) = {
  val results = for (instruction <- instructions) yield run_instruction(instruction)
  val horizontal = results.foldLeft(0)((accum, elem) => accum + elem._1)
  val vertical = results.foldLeft(0)((accum, elem) => accum + elem._2)
  (horizontal,vertical)
}

def main(inputs: List[String]) = {
  follow_instructions(prepare_inputs(inputs))
}

val position = main(inputs)
position._1*position._2

// Part 2
def run_instruction_part2(instruction: (String, Int)) = {
  instruction match { // horizontal, aim
    case ("forward", num: Int) => (num, 0)
    case ("up", num: Int) => (0,-num)
    case ("down", num: Int) => (0, num)
  }
}

def follow_instructions_part2(instructions: List[(String,Int)]) = {
  val results = for (instruction <- instructions) yield run_instruction_part2(instruction)
  val horizontal = results.foldLeft(0)((accum, elem) => accum + elem._1)
  //accum = vertical, aim
  val (vertical, aim) = results.foldLeft((0,0))((accum: (Int, Int), elem) => (accum._1+elem._1*(accum._2+elem._2),
                                                                            accum._2+elem._2))
  (horizontal,vertical, aim)
}

def main_part2(inputs: List[String]) = {
  follow_instructions_part2(prepare_inputs(inputs))
}

val position_part2 = main_part2(inputs)
position_part2._1*position_part2._2