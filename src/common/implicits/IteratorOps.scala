package common.implicits

object IteratorOps {
  implicit class IteratorOps[T](x: Iterator[T]) {
    /**
     * Returns an iterator over each list of contiguous elements in the original iterator.
     * The results will include an empty iterator for _every_ occurrence of the split element.
     * NOTE that this is different than String.split, which drops the first and last empty iterators
     */
    def split(t: T): Iterator[Iterator[T]] = LazyList.iterate((Iterator.empty[T],x ++ Iterator[T](t))){i =>
      (i._2.takeWhile{x => x != t}, i._2)
    }.takeWhile(_._2.hasNext).drop(1).map(_._1).iterator

    def total(pred: T => Boolean): (Int, Int) = x.foldLeft((0, 0)){ case ((yes, no), t) =>
      val matches = pred(t)
      (yes + (if (matches) 1 else 0), no + (if (matches) 0 else 1))
    }
  }

  implicit class NestedIteratorOps[T](x: Iterator[Iterator[T]]) {
    def join(t: T): Iterator[T] = x.next ++ x.flatMap(_ ++ Iterator(t))
  }
}
