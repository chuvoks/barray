/*
 * Copyright (c) 2012 Juha Heljoranta
 */
package barray

import barray.{ RedBlackRank => RB }
import scala.annotation.unchecked.uncheckedVariance
import compat.Platform
import scala.collection.generic._
import scala.collection.mutable.Builder
import scala.collection.mutable.ArrayBuffer
import scala.collection.IndexedSeqLike
import scala.collection.GenTraversableOnce
import scala.collection.GenSeq
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuilder

/* Most code in here is adapted from Scala 2.10 classes Vector, TreeSet and TreeMap classes. */

/**
 * Companion object to the BArray class
 */
object BArray extends SeqFactory[BArray] {

  private[barray] class BArrayReusableCBF extends GenericCanBuildFrom[Nothing] {
    override def apply() = newBuilder[Nothing]
  }

  private val BArrayReusableCBF: GenericCanBuildFrom[Nothing] = new BArrayReusableCBF

  def newBuilder[A]: Builder[A, BArray[A]] = new BArrayBuilder[A]()

  @inline implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, BArray[A]] =
    BArrayReusableCBF.asInstanceOf[CanBuildFrom[Coll, A, BArray[A]]]

  private[barray] val NIL = new BArray[Nothing]()
  @inline override def empty[A]: BArray[A] = NIL

  @inline def apply[A](a: A) = new BArray(RB.apply(a))

}

/**
 * IndexedSeq implementation backed by a balanced tree.
 *
 * Provides O(log(n)) element insert, update and removal.
 */
final class BArray[+A] private[barray] (private[barray] val tree: RB.Tree[A])
  extends IndexedSeq[A]
  with GenericTraversableTemplate[A, BArray]
  with IndexedSeqLike[A, BArray[A]]
  with Serializable { self =>

  def this() = this(null)

  override def companion: GenericCompanion[BArray] = BArray

  override def length = RB.count(tree)

  override def isEmpty = length == 0

  override def iterator: Iterator[A] = RB.iterator(tree)

  override def toIterator = iterator // avoid stream iterator

  override def reverseIterator: Iterator[A] = RB.reverseIterator(tree)

  override def lengthCompare(len: Int): Int = length - len

  override def apply(index: Int): A = RB.nth(tree, index).value

  override def foreach[U](f: A => U) = RB.foreach(tree, f)

  override def map[B, That](f: A => B)(implicit bf: CanBuildFrom[BArray[A], B, That]): That = bf match {
    case _: BArray.BArrayReusableCBF => new BArray(RB.map(tree, f)).asInstanceOf[That]
    case _ => super.map(f)
  }

  override def reverseMap[B, That](f: A => B)(implicit bf: CanBuildFrom[BArray[A], B, That]): That = bf match {
    case _: BArray.BArrayReusableCBF => new BArray(RB.reverseMap(tree, f)).asInstanceOf[That]
    case _ => super.reverseMap(f)
  }

  override def reverse = new BArray(RB.reverse(tree))

  override def head = {
    if (isEmpty) throw new UnsupportedOperationException("empty.head")
    RB.smallest(tree).value
  }

  override def last = {
    if (isEmpty) throw new UnsupportedOperationException("empty.last")
    RB.greatest(tree).value
  }

  override def tail = {
    if (isEmpty) throw new UnsupportedOperationException("empty.tail")
    new BArray(RB.deleteNth(tree, 0))
  }

  override def init = {
    if (isEmpty) throw new UnsupportedOperationException("empty.init")
    new BArray(RB.deleteNth(tree, size - 1))
  }

  override def take(n: Int): BArray[A] =
    if (n <= 0) BArray.empty
    else if (n < length) new BArray(RB.take(tree, n))
    else this

  override def drop(n: Int): BArray[A] =
    if (n <= 0) this
    else if (n < length) new BArray(RB.drop(tree, n))
    else BArray.empty

  override def takeRight(n: Int): BArray[A] = drop(length - n)

  override def dropRight(n: Int): BArray[A] = take(length - n)

  override def splitAt(n: Int) = (take(n), drop(n))

  override def slice(from: Int, until: Int) = {
    if (until <= from) BArray.empty
    else if (from <= 0) take(until)
    else if (until >= size) drop(from)
    else new BArray(RB.slice(tree, from, until))
  }

  private[this] def countWhile(p: A => Boolean): Int = {
    var n = 0
    RB.forwardWhile(tree, { a: A => if (p(a)) { n += 1; true } else false })
    n
  }

  override def takeWhile(p: A => Boolean) = take(countWhile(p))
  override def dropWhile(p: A => Boolean) = drop(countWhile(p))
  override def span(p: A => Boolean) = splitAt(countWhile(p))

  override def ++[B >: A, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[BArray[A], B, That]): That = {
    that match {
      case b: BArray[B] => new BArray(RB.concat(tree, b.tree)).asInstanceOf[That]
      case _ => super.++(that)
    }
  }

  @inline override def +:[B >: A, That](elem: B)(implicit bf: CanBuildFrom[BArray[A], B, That]): That = bf match {
    case _: BArray.BArrayReusableCBF => new BArray(RB.insertNth(tree, 0, elem)).asInstanceOf[That]
    case _ => super.+:(elem)(bf)
  }

  @inline override def :+[B >: A, That](elem: B)(implicit bf: CanBuildFrom[BArray[A], B, That]): That = bf match {
    case _: BArray.BArrayReusableCBF => new BArray(RB.insertNth(tree, size, elem)).asInstanceOf[That]
    case _ => super.:+(elem)(bf)
  }

  @inline override def updated[B >: A, That](index: Int, elem: B)(implicit bf: CanBuildFrom[BArray[A], B, That]): That = bf match {
    case _: BArray.BArrayReusableCBF => new BArray(RB.updateNth(tree, index, elem, true)).asInstanceOf[That]
    case _ => super.updated(index, elem)(bf)
  }

  /** Same as patch(index, Repr(elem), 0) but faster */
  @inline def inserted[B >: A, That](index: Int, elem: B)(implicit bf: CanBuildFrom[BArray[A], B, That]): That = bf match {
    case _: BArray.BArrayReusableCBF => {
      val idx = if (index < 0) 0 else if (index > length) length else index
      new BArray(RB.insertNth(tree, idx, elem)).asInstanceOf[That]
    }
    case _ => super.patch(index, BArray.apply(elem), 0)
  }

  /** Same as patch(index, Repr.empty, 1) but faster */
  @inline def removed[B >: A, That](index: Int)(implicit bf: CanBuildFrom[BArray[A], B, That]): That = bf match {
    case _: BArray.BArrayReusableCBF => {
      if (index >= length) this.asInstanceOf[That]
      else new BArray(RB.deleteNth(tree, if (index < 0) 0 else index)).asInstanceOf[That]
    }
    case _ => super.patch(index, BArray.empty, 1)
  }

  // based on RRBVector patch
  override def patch[B >: A, That](from: Int, patch: GenSeq[B], replaced: Int)(implicit bf: CanBuildFrom[BArray[A], B, That]): That = bf match {
    case _: BArray.BArrayReusableCBF => {
      val insert = patch.nonEmpty
      val delete = replaced > 0
      if (insert || delete) {
        val insertOne = patch.seq.lengthCompare(1) == 0
        val deleteOne = replaced == 1
        if (insertOne && deleteOne) updated(from, patch.head)
        else if (insertOne && !delete) inserted(from, patch.head)
        else if (!insert && deleteOne) removed(from)
        else {
          val prefix = take(from)
          val rest = drop(from + replaced)
          ((prefix ++ patch).asInstanceOf[BArray[B]] ++ rest).asInstanceOf[That]
        }
      } else this.asInstanceOf[That]
    }
    case _ => super.patch(from, patch, replaced)
  }

  /**
   * On sorted sequence: update element if found.
   * If sequence is unsorted the result is undefined.
   */
  def bupdated[B >: A, That](elem: B)(implicit bf: CanBuildFrom[BArray[A], B, That], ord: Ordering[B]): That = {
    search(elem)(ord) match {
      case Found(index) => updated(index, elem)(bf)
      case _ => this.asInstanceOf[That]
    }
  }

  /**
   * On sorted sequence: insert element if found.
   * If sequence is unsorted the result is undefined.
   */
  def binserted[B >: A, That](elem: B)(implicit bf: CanBuildFrom[BArray[A], B, That], ord: Ordering[B]): That = {
    inserted(search(elem)(ord).insertionPoint + 1, elem)(bf)
  }

  /**
   * On sorted sequence: remove element if found.
   * If sequence is unsorted the result is undefined.
   */
  def bremoved[B >: A, That](elem: B)(implicit bf: CanBuildFrom[BArray[A], B, That], ord: Ordering[B]): That = {
    search(elem)(ord) match {
      case Found(index) => removed(index)(bf)
      case _ => this.asInstanceOf[That]
    }
  }

  // binary search adapted from Scala 2.11

  private[this] sealed abstract class SearchResult {
    def insertionPoint: Int
  }
  private[this] case class Found(foundIndex: Int) extends SearchResult {
    override def insertionPoint = foundIndex
  }
  private[this] case class InsertionPoint(insertionPoint: Int) extends SearchResult

  private[this] def search[B >: A](elem: B)(implicit ord: Ordering[B]): SearchResult =
    binarySearch(elem, -1, length)(ord)

  @tailrec
  private[this] def binarySearch[B >: A](elem: B, from: Int, to: Int)(implicit ord: Ordering[B]): SearchResult = {
    if ((to - from) == 1) InsertionPoint(from) else {
      val idx = from + (to - from) / 2
      math.signum(ord.compare(elem, this(idx))) match {
        case -1 => binarySearch(elem, from, idx)(ord)
        case 1 => binarySearch(elem, idx, to)(ord)
        case _ => Found(idx)
      }
    }
  }

}

final class BArrayBuilder[A]() extends Builder[A, BArray[A]] {

  private[this] var acc = RB.empty[A]

  private[this] var ib = new IteratorBuilder[A]

  //override def sizeHint(size: Int) = ib.sizeHint(size)

  def +=(elem: A): this.type = { ib += elem; this }

  private[this] def commitIb() {
    if (ib.nonEmpty) {
      val i = ib.result
      acc = RB.concat(acc, RB.blindMap(RB.allocate(i.size), i.next()))
      ib = new IteratorBuilder[A]
    }
  }

  override def ++=(xs: TraversableOnce[A]): this.type = {
    xs match {
      case b: BArray[A] => {
        commitIb()
        acc = RB.concat(acc, b.tree)
        this
      }
      case v: IndexedSeqLike[A, _] => {
        commitIb()
        val i = v.iterator
        acc = RB.concat(acc, RB.blindMap(RB.allocate(v.size), i.next()))
        this
      }
      case _ => { xs.seq foreach ib.+=; this }
    }
  }

  def result: BArray[A] = { commitIb(); new BArray(acc) }

  def clear(): Unit = {
    ib = new IteratorBuilder[A]
    acc = RB.empty[A]
  }

}
