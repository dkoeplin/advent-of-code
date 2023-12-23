package Year2023

object Day06 extends Year2023(6) {
  val time = "Time: "
  val dist = "Distance: "
  val lines = data.getLines()
  val line0 = lines.next().drop(time.length)
  val line1 = lines.next().drop(dist.length)

  def combos(t: Long, d: Long): Long = {
    // Constant speed, so distance is just (where h is hold time):
    //   d = (t - h)*h
    // So if we want to beat a distance of d:
    //   -h^2 + h*t - d > 0
    // Solving for h:
    //   (t +/- sqrt(t^2 - 4d))/2
    // So number of combinations is just integer values between these two zeros (non-inclusive)
    val x = t * t - 4 * d
    if (x < 0) 0 else {
      val z0 = math.floor((t - math.sqrt(x)) / 2).toLong + 1
      val z1 = math.ceil((t + math.sqrt(x)) / 2).toLong - 1
      if (z1 >= z0) z1 - z0 + 1 else 0
    }
  }

  val times = line0.split(' ').iterator.map(_.trim).filter(_.nonEmpty).map(_.toLong)
  val dists = line1.split(' ').iterator.map(_.trim).filter(_.nonEmpty).map(_.toLong)
  val part1 = times.zip(dists).map{case (t, d) => combos(t, d) }.product
  println(s"Part 1: $part1")

  val t = line0.filterNot(_ == ' ').toLong
  val d = line1.filterNot(_ == ' ').toLong
  val part2 = combos(t, d)
  println(s"Part 2: $part2")
}
