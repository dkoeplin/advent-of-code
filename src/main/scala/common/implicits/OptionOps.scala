package common.implicits

object OptionOps {
  private def join[A:Ordering](a: Option[A], b: Option[A])(func: (A, A) => A): Option[A] = (a, b) match {
    case (Some(a), Some(b)) => Some(func(a, b))
    case (Some(a), None) => Some(a)
    case (None, Some(b)) => Some(b)
    case _ => None
  }

  implicit class OptionNumericOps[A](a: Option[A])(implicit order: Ordering[A]) {
    import order._
    def max(b: Option[A]): Option[A] = join(a, b)(_ max _)
    def min(b: Option[A]): Option[A] = join(a, b)(_ min _)
    def max(b: A): Option[A] = max(Some(b))
    def min(b: A): Option[A] = min(Some(b))
  }
}
