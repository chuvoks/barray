/*
 * Copyright (c) 2012 Juha Heljoranta
 */
package scala.collection.immutable
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import scala.util.Random
import java.util.NoSuchElementException

@RunWith(classOf[JUnitRunner])
class BArrayTest extends FunSuite {

  private[this] val rnd = new Random
  private[this] val N = rnd.nextInt(32)

  private[this] val vec = Vector.empty ++ rnd.shuffle((1 to N).toList)
  private[this] val ba = BArray.empty ++ vec

  private[this] def invariants[A](t: BArray[A]) = RedBlackRank.invariants(t.tree)

  test("foreach/iterator consistency") {
    val it = ba.iterator
    var consistent = true
    ba.foreach { element =>
      consistent &&= it.hasNext && element == it.next
    }
    assert(consistent)
  }

  test("map/iterator consistency") {
    val it = ba.iterator
    var consistent = true
    ba.map { element =>
      consistent &&= it.hasNext && element == it.next
    }
    assert(consistent)
  }

  test("reverseMap/reverseIterator consistency") {
    val it = ba.reverseIterator
    var consistent = true
    ba.reverseMap { element =>
      consistent &&= it.hasNext && element == it.next
    }
    assert(consistent)
  }

  test("++") {
    val l = vec.toList
    assert((ba ++ ba) === (vec ++ vec))
    assert((ba ++ vec) === (vec ++ ba))
    assert((ba ++ l) === (vec ++ l))
    invariants(ba ++ ba)
  }

  test("apply") {
    for (i <- 0 until vec.length)
      assert(ba(i) === vec(i))
  }

  test("length") {
    assert(ba.length === vec.length)
  }

  test("head") {
    if (N > 0)
      assert(ba.head === vec.head)
  }

  test("last") {
    if (N > 0)
      assert(ba.last === vec.last)
  }

  test("tail") {
    if (N > 0)
      assert(ba.tail === vec.tail)
  }

  test("init") {
    if (N > 0)
      assert(ba.init === vec.init)
  }

  test(":+") {
    var b = BArray.empty[Int]
    vec.foreach(a => b :+= a)
    assert(vec === b)
  }

  test("+:") {
    var b = BArray.empty[Int]
    vec.foreach(a => b +:= a)
    assert(vec.reverse === b)
  }

  test("drop") {
    val i = rnd.nextInt(N + 1)
    assert(ba.drop(i) === vec.drop(i))
    invariants(ba.drop(i))
  }

  test("take") {
    val i = rnd.nextInt(N + 1)
    assert(ba.take(i) === vec.take(i))
    invariants(ba.take(i))

  }

  test("map") {
    assert(ba.map(_.toString) === vec.map(_.toString))
    invariants(ba.map(identity))
  }

  test("reverseMap") {
    assert(ba.reverseMap(_.toString) === vec.reverseMap(_.toString))
    invariants(ba.reverseMap(identity))
  }

  test("iterator") {
    assert(ba.iterator.toSeq === vec.iterator.toSeq)
  }

  test("reverse") {
    assert(ba.reverse === vec.reverse)
    invariants(ba.reverse)
  }

  test("reverseIterator") {
    assert(ba.reverseIterator.toSeq === vec.reverseIterator.toSeq)
    if (N > 0) {
      assert(ba.reverseIterator.next === vec.reverseIterator.next)
    }
  }

  test("startsWith/take") {
    ba startsWith (ba take rnd.nextInt(N + 1))
  }

  test("endsWith/takeRight") {
    ba endsWith (ba takeRight rnd.nextInt(N + 1))
  }

  test("slice/take/drop") {
    val from = rnd.nextInt(N + 1)
    val until = rnd.nextInt(N - from + 1) + from
    assert(ba.slice(from, until) === ba.take(until).drop(from))
  }

  test("slice") {
    val from = rnd.nextInt(N + 1)
    val until = rnd.nextInt(N - from + 1) + from
    assert(ba.slice(from, until) === vec.slice(from, until))
    invariants(ba.slice(from, until))
  }

  test("splitAt") {
    val i = rnd.nextInt(N + 1)
    assert(ba.splitAt(i) === vec.splitAt(i))
  }

  test("takeRight") {
    val i = rnd.nextInt(N + 1)
    assert(ba.takeRight(i) === vec.takeRight(i))
  }

  test("dropRight") {
    val i = rnd.nextInt(N + 1)
    assert(ba.dropRight(i) === vec.dropRight(i))
  }

  test("takeWhile") {
    assert(ba.takeWhile(_ > N / 2) === vec.takeWhile(_ > N / 2))
    assert(ba.takeWhile(_ => true) === vec)
  }

  test("inserted") {
    val i = rnd.nextInt(N + 1)
    assert(ba.inserted(i, i + 1) === vec.patch(i, Vector(i + 1), 0))
    invariants(ba.inserted(i, i + 1))
  }

  test("removed") {
    val i = rnd.nextInt(N + 1)
    assert(ba.removed(i) === vec.patch(i, Vector.empty, 1))
    invariants(ba.removed(i))
  }

  test("patch") {
    val i = rnd.nextInt(N + 1)
    // removed
    assert(ba.patch(i, BArray.empty, 1) === vec.patch(i, Vector.empty, 1))
    // inserted
    assert(ba.patch(i, BArray(i + 1), 0) === vec.patch(i, Vector(i + 1), 0))
    // updated
    assert(ba.patch(i, BArray(i + 1), 1) === vec.patch(i, Vector(i + 1), 1))

    // non-special cases
    assert(ba.patch(i, BArray(0, 1, 2), 0) === vec.patch(i, Vector(0, 1, 2), 0))
    assert(ba.patch(i, BArray(0, 1, 2), 2) === vec.patch(i, Vector(0, 1, 2), 2))
    assert(ba.patch(i, BArray.empty, 2) === vec.patch(i, Vector.empty, 2))
  }

  test("binserted") {
    var a = Vector.empty[Int]
    var b = BArray.empty[Int]
    for (u <- vec) {
      a = (a :+ u).sorted
      b = b.binserted(u)
      assert(a === b)
    }
    invariants(b)
  }

  test("bremoved") {
    var a = vec.sorted
    var b = ba.sorted
    for (u <- vec) {
      a = a.patch(a.indexOf(u), Vector.empty, 1)
      b = b.bremoved(u)
      assert(a === b)
    }
    invariants(b)
    a = vec.sorted
    b = ba.sorted
    b = b.bremoved(-10)
    assert(a === b)
    b = b.bremoved(N + 10)
    assert(a === b)

  }

  test("bupdated") {
    var a = vec.sorted
    var b = ba.sorted
    for (u <- vec) {
      b = b.bupdated(u)
      assert(a === b)
    }
    invariants(b)
    b = b.bupdated(-10)
    assert(a === b)
    b = b.bupdated(N + 10)
    assert(a === b)
  }

}
