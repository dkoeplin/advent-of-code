package common.implicits

import common.immutable.Pos.Idx

object IteratorOps {
  implicit class IteratorOps[T](x: Iterator[T]) {
    /**
     * Returns an iterator over each list of contiguous elements in the original iterator.
     * The results will include an empty iterator for _every_ occurrence of the split element.
     * NOTE that this is different than String.split, which drops the first and last empty iterators
     */
    def split(t: T): Iterator[Iterator[T]] = {
      var current: Iterator[T] = x
      var result: Iterator[Iterator[T]] = Iterator.empty
      while(current.hasNext) {
        val (prefix, suffix) = current.span(_ != t)
        result = result ++ Iterator(prefix)
        if (suffix.hasNext) {
          if (prefix.hasNext) result ++= Iterator(Iterator.empty)
          suffix.next()
        }
        current = suffix
      }
      result
    }
    /**
     * Returns the total number of elements that match vs. don't match the given predicate.
     * The first entry (x) in the Pos is the number of elements that don't match.
     * The second entry (y) is the number of elements that do match.
     */
    def total(pred: T => Boolean): Idx = x.foldLeft(Idx(0, 0)){(m, t) =>
      import common.implicits.BooleanOps._
      val matches = pred(t)
      Idx(m.x + (!matches).toInt, m.y + matches.toInt)
    }
  }

  def zipped[T](iters: Iterator[T]*): Iterator[Seq[T]] = LazyList.iterate((Seq.empty[T], iters, true)){case (_,iters,_) =>
    val keep = iters.head.hasNext
    if (keep) (iters.map{i => i.next() }, iters, keep) else (Nil, iters, false)
  }.takeWhile(_._3).iterator.map(_._1).drop(1)
}
