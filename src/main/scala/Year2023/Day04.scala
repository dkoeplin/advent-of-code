package Year2023

object Day04 extends Year2023(4) {
  case class Card(win: Set[Int], has: Set[Int]) {
    val matching: Int = has.count(win.contains)
    val points: Long = if (matching > 0) 1L << (matching - 1) else 0L
  }
  val cards = data.getLines().map{line =>
    val Array(win, has) = line.split(':').last
                              .split('|').map( _.split(' ').iterator.map(_.trim).filter(_.nonEmpty).map(_.toInt).toSet)
    Card(win, has)
  }.toArray

  val part1 = cards.iterator.map(_.points).sum
  println(s"Part 1: $part1")

  // Just keep an array for the next N copies, dropping the first each time we reduce it
  val part2 = cards.foldLeft((Array.empty[Int], 0)){case ((repeats, n), card) =>
    val copies = if (repeats.isEmpty) 1 else repeats.head
    // Not super elegant, but this is just previously known copies + new copies
    val repeats2 = Array.tabulate(math.max(repeats.length - 1, card.matching)){j =>
      repeats.applyOrElse(j + 1, {_: Int => 1}) + (if (j < card.matching) copies else 0)
    }
    (repeats2, n + copies)
  }
  println(s"Part 2: ${part2._2}")
}
