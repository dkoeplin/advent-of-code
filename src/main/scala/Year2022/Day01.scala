package Year2022

object Day01 extends common.AoC(1, 2022) {
  val max = data.getLines().foldLeft((Array(0, 0, 0), 0)){
    case (accum, "") => ((accum._1 :+ accum._2).sortBy(x => -x).take(3), 0)
    case (accum, x) => (accum._1, accum._2 + x.toInt)
  }
  println(s"Max: ${max._1.sum}")
}
