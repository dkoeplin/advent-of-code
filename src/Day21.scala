import scala.collection.mutable

object Day21 extends App {
  case class Player(var square: Int, var score: Int = 0)

  val PlayerStart = "Player . starting position: ([0-9]+)".r
  val file = io.Source.fromFile("./data/21")
  val lines = file.getLines()
  val starts = lines.map{case PlayerStart(v) => v.toInt - 1 }.toArray

  var rolls = 0
  var turn = 0
  var dice = 1
  def roll(): Int = { val roll = dice; dice = if (dice == 100) 1 else dice + 1; rolls += 1; roll }

  val players = starts.map{square => Player(square) }
  while (!players.exists(_.score >= 1000)) {
    val current = players(turn)
    val total = roll() + roll() + roll()
    val next = (current.square + total) % 10
    current.square = next
    current.score += (next + 1)
    turn = (turn + 1) % players.length
  }

  val loser = players.minBy(_.score).score
  println(s"Part 1: Loser Score: $loser, Rolls: $rolls, Total: ${loser * rolls}")

  // Part 2
  // Total score >= 21
  // Total dice roll is 3 - 9 (1,1,1) => (3,3,3)
  // Max number of turns is ~7
  // Winning = have at least 21 first
  // Number of ways to finish with N rolls = number of ways to reach >= 21 in exactly N
  // score[N, P] = N + sum((startP + roll1) % 10 + (start + roll1 + roll2) % 10 ... (start + roll1 + roll2 + ... + rollN) % 10)
  // done[N, P] = #roll combinations such that score[N, P] >= 21
  //            = |{rolls : score[rolls] >= 21 }|
  // notDone[N, P] = #roll combinations such that score[N, P] < 21

  // Player wins by reaching 21 first:
  //   wins[N, P=1] = done[N, 1] * notDone[N - 1, 2] // Number of universes where P1 has finished in N and P2 was not done in N-1
  //   wins[N, P=2] = done[N, 2] * notDone[N, 1]     // Number of universes where P2 has finished in N and P1 was not done in N (since P1 goes first)
  // wins[P] = sum(N = 1 to Max)(wins[N, P])

  // Order matters for score, e.g.:
  //   start 5: rolls {5, 7}: score = 10 + 7 = 17
  //   start 5: rolls {7, 5}: score = 2 + 7 = 9

  val combinations = Map[Int,Long](
    3 -> 1L, // 1,1,1
    4 -> 3L,  // 1,1,2, 1,2,1, 2,1,1
    5 -> 6L,  // 1,2,2, 2,1,2, 2,2,1, 3,1,1, 1,3,1, 1,1,3
    6 -> 7L,  // 2,2,2, 1,2,3, 3,2,1, 2,3,1, 2,1,3, 3,1,2, 1,3,2
    7 -> 6L,  // 3,2,2, 2,2,3, 2,3,2, 1,3,3, 3,3,1, 3,1,3
    8 -> 3L,  // 3,2,3, 2,3,3, 3,3,2
    9 -> 1L   // 3,3,3
  )

  case class PlayerRecords(
    dones: mutable.Map[Int, mutable.Buffer[Seq[Int]]] = mutable.Map.empty, // Number of rolls => #ways >= 21
    notDones: mutable.Map[Int, mutable.Buffer[Seq[Int]]] = mutable.Map.empty // #rolls => roll => score
  ) {
    def wins(n: Int): Long = dones.getOrElse(n, Nil).map{rolls => rolls.map{r => combinations(r)}.product }.sum
    def losses(n: Int): Long = notDones.getOrElse(n, Nil).map{rolls => rolls.map{r => combinations(r)}.product }.sum
  }
  def countWins(player1: PlayerRecords, player2: PlayerRecords): (Long, Long) = {
    val max = Math.max(player1.dones.keys.max, player2.dones.keys.max)
    val p1Wins = (1 to max).map{n => player1.wins(n) * player2.losses(n - 1) }.sum
    val p2Wins = (1 to max).map{n => player2.wins(n) * player1.losses(n) }.sum
    (p1Wins, p2Wins)
  }

  def bruteforce(start: Int): (PlayerRecords, Int) = {
    val records = PlayerRecords()
    records.notDones(0) = mutable.Buffer.empty
    records.notDones(0) += Nil
    var n = 1
    while (records.notDones(n - 1).nonEmpty) {
      records.dones(n) = mutable.Buffer.empty
      records.notDones(n) = mutable.Buffer.empty
      records.notDones(n - 1).foreach{prevRoll =>
        (3 to 9).foreach{roll =>
          val rolls = prevRoll :+ roll
          val score = n + rolls.indices.map{i => (start + rolls.take(i + 1).sum) % 10 }.sum
          if (score >= 21) records.dones(n) += rolls else records.notDones(n) += rolls
        }
      }
      n += 1
    }
    (records, n - 1)
  }

  val (player1, _) = bruteforce(starts(0))
  val (player2, _) = bruteforce(starts(1))
  val (p1Wins, p2Wins) = countWins(player1, player2)
  println(s"Player 1 wins: $p1Wins")
  println(s"Player 2 wins: $p2Wins")
}
