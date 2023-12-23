package common.implicits

object OptionOps {
  private def join(a: Option[Int], b: Option[Int])(func: (Int, Int) => Int): Option[Int] = (a, b) match {
    case (Some(a), Some(b)) => Some(func(a, b))
    case (Some(a), None) => Some(a)
    case (None, Some(b)) => Some(b)
    case _ => None
  }

  implicit class OptionIntOps(a: Option[Int]) {
    def max(b: Option[Int]): Option[Int] = join(a, b)(_ max _)
    def min(b: Option[Int]): Option[Int] = join(a, b)(_ min _)
    def max(b: Int): Option[Int] = max(Some(b))
    def min(b: Int): Option[Int] = min(Some(b))
  }
}
