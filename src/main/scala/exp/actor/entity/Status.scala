package exp.actor.entity

object Status {
  case object None extends Status(0)
  case object Wake extends Status(1)
  case object Dead extends Status(2)
}
sealed abstract class Status(val n: Int) {
  def |(rhs: Status): Status = if (rhs.n > n) rhs else this
}
