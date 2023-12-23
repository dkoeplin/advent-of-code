package Year2023

object Day07 extends Year2023(7) {
  trait Rules extends Ordering[Hand] {
    protected type HandPattern = Map[Char, Int] => Boolean
    protected val cardRanking: List[Char]
    private val handRanking: List[HandPattern] = List[HandPattern](
      {_.size == 1},         // Five of a kind
      {_.exists(_._2 == 4)}, // Four of a kind
      {_.size == 2},         // Full house (exactly two)
      {_.exists(_._2 == 3)}, // Three of a kind
      {_.size == 3},         // Two pair
      {_.size == 4},         // One pair
      { _ => true }          // High card
    )
    def rank(card: Char): Int = cardRanking.size - cardRanking.indexOf(card)
    def rank(counts: Map[Char, Int]): Int = handRanking.size - handRanking.indexWhere(_.apply(counts))
    def rank(hand: Hand): Int = rank(counts(hand.cards))
    def counts(hand: Array[Char]): Map[Char, Int]
    def compare(a: Hand, b: Hand): Int = {
      implicit val rules: Rules = this
      if (a == b) 0 else if (a < b) -1 else 1
    }
  }
  object Part1 extends Rules {
    protected val cardRanking: List[Char] = "AKQJT98765432".toList
    def counts(cards: Array[Char]): Map[Char, Int] =
      cards.groupMapReduce(identity)(_ => 1)(_+_) // ooh fancy
  }
  object Part2 extends Rules {
    protected val cardRanking: List[Char] = "AKQT98765432J".toList
    def counts(cards: Array[Char]): Map[Char, Int] = {
      val part1 = Part1.counts(cards)
      val jokers = part1.getOrElse('J',0)
      if (jokers == 5) Map('2' -> 5)
      else part1.removed('J').map{case (c,n) => (c, n + jokers) }
    }
  }

  case class Hand(cards: Array[Char], bid: Long) {
    def <(rhs: Hand)(implicit rules: Rules): Boolean =
      cards.iterator.zip(rhs.cards)
           .map{case (a, b) => (rules.rank(a), rules.rank(b)) }
           .find{case (a, b) => a != b }
           .exists{case (a, b) => a < b }
  }
  object Hand {
    def parse(x: String): Hand = Hand(x.take(5).toArray, x.drop(5).trim.toLong)
  }
  def winnings(hands: Array[Hand])(implicit rules: Rules): Long =
    hands.groupBy(rules.rank)
         .toArray.sortBy(_._1).iterator
         .flatMap(_._2.sorted)
         .zipWithIndex
         .map{case (hand, rank) => hand.bid * (rank + 1) }
         .sum

  val hands = data.getLines().map(Hand.parse).toArray
  val part1 = winnings(hands)(Part1)
  println(s"Part 1: $part1")

  val part2 = winnings(hands)(Part2)
  println(s"Part 2: $part2")
}
