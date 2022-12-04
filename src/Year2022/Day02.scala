package Year2022

object Day02 extends App {
  val Lose: Int = 0
  val Draw: Int = 3
  val Win: Int = 6
  val Rock: Int = 1
  val Paper: Int = 2
  val Scissors: Int = 3
  def decode_part1(x: Char): Int = x match {
    case 'A' | 'X' => Rock
    case 'B' | 'Y' => Paper
    case 'C' | 'Z' => Scissors
  }
  def decode_result(x: Char): Int = x match {
    case 'X' => Lose
    case 'Y' => Draw
    case 'Z' => Win
  }
  object GamePart1 {
    def unapply(x: String): Option[(Int, Int)] =
      Some((decode_part1(x(0)), decode_part1(x(2))))
  }
  def score(a: Int, b: Int): Int = (a, b) match {
    case (a, b) if a == b  => Draw
    case (Rock, Paper)     => Win
    case (Rock, Scissors)  => Lose
    case (Paper, Rock)     => Lose
    case (Paper, Scissors) => Win
    case (Scissors, Rock)  => Win
    case (Scissors, Paper) => Lose
  }
  def reverse_score(a: Int, b: Int) = (a, b) match {
    case (a, Draw) => a
    case (Rock, Lose) => Scissors
    case (Rock, Win)  => Paper
    case (Paper, Lose) => Rock
    case (Paper, Win) => Scissors
    case (Scissors, Lose) => Paper
    case (Scissors, Win) => Rock
  }
  object GamePart2 {
    def unapply(x: String): Option[(Int, Int)] = {
      val other = decode_part1(x(0))
      val result = decode_result(x(2))
      val move = reverse_score(other, result)
      Some((other, move))
    }
  }

  val file = scala.io.Source.fromFile("data/full/02")
  val lines = file.getLines()
  val part1 = lines.map{case GamePart1(a, b) => score(a, b) + b }.sum
  println(s"Total: $part1")
  val part2 = lines.map{case GamePart2(a, b) => score(a, b) + b }.sum
  println(s"Total: $part2")
}
