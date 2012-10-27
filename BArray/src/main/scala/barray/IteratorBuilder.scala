package barray

import scala.collection.mutable.Builder

final class IteratorBuilder[A]() extends Builder[A, Iterator[A]] {

  private[this] var arr: Array[A] = mkArr

  private[this] var size = 0;

  def isEmpty = size == 0
  def nonEmpty = !isEmpty

  override def sizeHint(size: Int) {
    if (size > this.size) arr = resize(size)
  }

  override def +=(a: A): this.type = {
    ensureCapacity()
    arr(size) = a
    size += 1
    this
  }
  override def ++=(xs: TraversableOnce[A]): this.type =
    super.++=(xs)

  override def clear() { arr = mkArr; size = 0 }

  override def result: Iterator[A] = new Iterator[A] {
    private[this] val a = arr
    private[this] var i = 0
    override final val size = IteratorBuilder.this.size
    @inline override final def hasNext = i < size
    override final def next =
      if (hasNext) { val e = a(i); i += 1; e }
      else Iterator.empty.next
  }

  private[this] def ensureCapacity() {
    val minSize = size + 1
    if (minSize > arr.length) {
      val len = arr.length;
      var newLen = len + (len >> 1);
      if (newLen - minSize < 0) newLen = minSize;
      arr = resize(newLen)
    }
  }

  @inline private[this] def mkArr: Array[A] =
    new Array[AnyRef](10).asInstanceOf[Array[A]]

  @inline private[this] def resize(n: Int): Array[A] =
    java.util.Arrays.copyOf(arr.asInstanceOf[Array[AnyRef]], n).asInstanceOf[Array[A]]

}
