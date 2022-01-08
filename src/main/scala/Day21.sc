import scala.annotation.tailrec
import scala.math.max

case class Dice(value: Int, used: Int = 0){
  def roll: Dice = Dice(value % 100 + 1, used + 1)
}

case class Player(position: Int, score: Int = 0) {
  def play(dice: Dice): (Player, Dice) = {
    @tailrec
    def iter(dice: Dice, rolls: Int = 0, turn_score: Int = 0): (Player, Dice) = {
      if (rolls == 3) {
        (this.update(turn_score),  dice)
      } else {
        val new_dice = dice.roll
        iter(new_dice, rolls + 1, turn_score + new_dice.value)
      }
    }
    iter(dice)
  }

  def update(turn_score: Int): Player = {
    val new_position = (position + turn_score - 1) % 10 + 1
    Player(new_position, score + new_position)
  }
  def isWinner(limit: Int): Boolean = score >= limit
}

@tailrec
def game(player1: Player, player2: Player, dice: Dice, limit: Int): Int = {
  val (new_player1, new_dice) = player1.play(dice)
  if (new_player1.isWinner(limit)) {
    player2.score*new_dice.used
  } else {
    val (new_player2, new_dice2) = player2.play(new_dice)
    if (new_player2.isWinner(limit)) {
      player1.score*new_dice2.used
    } else {
      game(new_player1, new_player2, new_dice2, limit)
    }
  }
}

val result_part1 = game(Player(7), Player(9), Dice(0), 1000)


def p1turn(game: ((Int,Int,Int,Int), Long))(implicit turn: Map[Int, Int]): Map[(Int,Int,Int,Int), Long] = {
  turn.map { x =>
    val new_position = (game._1._1 + x._1 - 1) % 10 + 1
    (new_position, game._1._2 + new_position, game._1._3, game._1._4) -> game._2*x._2
  }
}

def p2turn(game: ((Int,Int,Int,Int), Long))(implicit turn: Map[Int, Int]) : Map[(Int,Int,Int,Int), Long] = {
  turn.map { x =>
    val new_position = (game._1._3 + x._1 - 1) % 10 + 1
    (game._1._1, game._1._2,new_position, game._1._4 + new_position) -> game._2*x._2
  }
}

def collectWins(games: Map[(Int,Int,Int,Int), Long])
               (f: (((Int,Int,Int,Int), Long)) => Boolean): (Long, Map[(Int,Int,Int,Int), Long]) = {
  val (finished, open) = games.partition(x => f(x))
  (finished.values.sum, open)
}

def mergeMaps(map1: Map[(Int,Int,Int,Int), Long], map2: Map[(Int,Int,Int,Int), Long]): Map[(Int,Int,Int,Int), Long] = {
  map1.foldLeft(map2)((accum, elem) => accum.updated(elem._1, accum(elem._1) + elem._2))
}

@tailrec
def solve(games: Map[(Int,Int,Int,Int), Long], p1wins: Long, p2wins: Long, limit: Int)
         (implicit turn: Map[Int, Int]) : Long = {
  if (games.isEmpty){
    max(p1wins, p2wins)
  } else {
    val selected = games.minBy(x => x._1._2 + x._1._4)
    val other = games.removed(selected._1).withDefaultValue(0L)
    val (p1wins_update, open_games) = collectWins(p1turn(selected))(x => x._1._2 >= limit)
    val (p2wins_update, open_games2) = collectWins(open_games.flatMap(x => p2turn(x)))(x => x._1._4 >= limit)
    solve(mergeMaps(other, open_games2.withDefaultValue(0L)),
          p1wins + p1wins_update,
          p2wins + p2wins_update, limit)
  }
}

implicit val turn: Map[Int, Int] = Map(3 -> 1, 4 -> 3, 5 -> 6, 6 -> 7, 7 -> 6, 8 -> 3, 9 -> 1)
val start: Map[(Int,Int,Int,Int), Long] = Map((7,0,9,0) -> 1)
solve(start.withDefaultValue(0L),0,0,21)