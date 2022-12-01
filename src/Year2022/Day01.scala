package Year2022

object IntString {
  def unapply(x: String): Option[Int] = if (x.nonEmpty && x.forall(_.isDigit)) Some(x.toInt) else None
}
object Day01 extends App {
  val file = scala.io.Source.fromFile("data/2022/01")
  val max = file.getLines().foldLeft((Array(0, 0, 0), 0)){
    case (accum, IntString(x)) => (accum._1, accum._2 + x)
    case (accum, _) => ((accum._1 :+ accum._2).sortBy(x => -x).take(3), 0)
  }
  println(s"Max: ${max._1.sum}")
}
