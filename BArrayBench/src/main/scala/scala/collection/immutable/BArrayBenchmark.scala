package scala.collection.immutable

import com.google.caliper.SimpleBenchmark
import com.google.caliper.Runner
import com.google.caliper.Param
import scala.util.Random
import java.lang.Class
import scala.collection.mutable.WrappedArray

class BArrayBenchmark extends SimpleBenchmark {

  // 1, 3, 7, 18, 47, 123, 322, 843, 2207, 5778, 15127, 39603, 103682, 271443, 710647, 1860498, 4870847
  @Param(Array("3", "7", "18", "47", "123", "322", "843", "2207", "5778", "15127"))
  var size: Int = _

  var tset: TreeSet[Int] = _
  var vec: Vector[Int] = _
  var ba: BArray[Int] = _
  var idxs: Array[Int] = _ // relative positions at 0.25, 0.5 and 0.75
  var list: List[Int] = _
  var rndList: List[Int] = _

  override protected def setUp() {
    idxs = if (size > 0) Array(size / 4, size / 2, size * 3 / 4) else Array()
    vec = Vector.empty ++ (0 until size)
    ba = BArray.empty ++ (0 until size)
    tset = TreeSet.empty[Int] ++ vec
    list = List.empty ++ (size to (size * 2))
    rndList = List.empty ++ Random.shuffle((0 to size))
  }

  def timeAppendSizeVector(reps: Int): Any = {
    var i = 0
    var v = vec
    while (i < reps) {
      var j = 0
      v = Vector()
      while (j < size) {
        v = v :+ j
        j += 1
      }
      i += 1
    }
    v
  }

  def timeAppendSizeBArray(reps: Int): Any = {
    var i = 0
    var v = ba
    while (i < reps) {
      var j = 0
      v = BArray()
      while (j < size) {
        v = v :+ j
        j += 1
      }
      i += 1
    }
    v
  }

  def timeAppendSizeTreeSet(reps: Int): Any = {
    var i = 0
    var v = tset
    while (i < reps) {
      var j = 0
      v = TreeSet()
      while (j < size) {
        v = v + j
        j += 1
      }
      i += 1
    }
    v
  }

  def timePrependSizeVector(reps: Int): Any = {
    var i = 0
    var a = vec
    while (i < reps) {
      var j = 0
      a = Vector()
      while (j < size) {
        a = j +: a
        j += 1
      }
      i += 1
    }
    a
  }

  def timePrependSizeBArray(reps: Int): Any = {
    var i = 0
    var a = ba
    while (i < reps) {
      var j = 0
      a = BArray()
      while (j < size) {
        a = j +: a
        j += 1
      }
      i += 1
    }
    a
  }

  def timeMapVector(reps: Int): Any = {
    var i = 0
    var a = vec
    while (i < reps) {
      a = vec.map(x => x + 1)
      i += 1
    }
    a
  }

  def timeMapBArray(reps: Int): Any = {
    var i = 0
    var a = ba
    while (i < reps) {
      a = ba.map(x => x + 1)
      i += 1
    }
    a
  }

  def timeMapTreeSet(reps: Int): Any = {
    var i = 0
    var a = tset
    while (i < reps) {
      a = tset.map(x => x + 1)
      i += 1
    }
    a
  }

  def timeForeachVector(reps: Int): Any = {
    var i = 0
    var a = 0
    while (i < reps) {
      vec.foreach(x => a = x)
      i += 1
    }
    a
  }

  def timeForeachBArray(reps: Int): Any = {
    var i = 0
    var a = 0
    while (i < reps) {
      ba.foreach(x => a = x)
      i += 1
    }
    a
  }

  def timeForeachTreeSet(reps: Int): Any = {
    var i = 0
    var a = 0
    while (i < reps) {
      tset.foreach(x => a = x)
      i += 1
    }
    a
  }

  def timeSelfCatVector(reps: Int): Any = {
    var i = 0
    var a = vec
    while (i < reps) {
      a = vec ++ vec
      i += 1
    }
    a
  }

  def timeSelfCatBArray(reps: Int): Any = {
    var i = 0
    var a = ba
    while (i < reps) {
      a = ba ++ ba
      i += 1
    }
    a
  }

  def timeSelfCatTreeSet(reps: Int): Any = {
    var i = 0
    var a = tset
    while (i < reps) {
      a = tset ++ tset
      i += 1
    }
    a
  }

  def timeCatListVector(reps: Int): Any = {
    var i = 0
    var a = vec
    while (i < reps) {
      a = vec ++ list
      i += 1
    }
    a
  }

  def timeCatListBArray(reps: Int): Any = {
    var i = 0
    var a = ba
    while (i < reps) {
      a = ba ++ list
      i += 1
    }
    a
  }

  def timeCatListTreeSet(reps: Int): Any = {
    var i = 0
    var a = ba
    while (i < reps) {
      a = ba ++ list
      i += 1
    }
    a
  }

  def timeHeadVector(reps: Int): Any = {
    var i = 0
    var a = 0
    while (i < reps) {
      a = vec.head
      i += 1
    }
    a
  }

  def timeHeadBArray(reps: Int): Any = {
    var i = 0
    var a = 0
    while (i < reps) {
      a = ba.head
      i += 1
    }
    a
  }

  def timeLastVector(reps: Int): Any = {
    var i = 0
    var a = 0
    while (i < reps) {
      a = vec.last
      i += 1
    }
    a
  }

  def timeLastBArray(reps: Int): Any = {
    var i = 0
    var a = 0
    while (i < reps) {
      a = ba.last
      i += 1
    }
    a
  }

  def timeTailVector(reps: Int): Any = {
    var i = 0
    var a = vec
    while (i < reps) {
      a = vec.tail
      i += 1
    }
    a
  }

  def timeTailBArray(reps: Int): Any = {
    var i = 0
    var a = ba
    while (i < reps) {
      a = ba.tail
      i += 1
    }
    a
  }

  def timeInitVector(reps: Int): Any = {
    var i = 0
    var a = vec
    while (i < reps) {
      a = vec.init
      i += 1
    }
    a
  }

  def timeInitBArray(reps: Int): Any = {
    var i = 0
    var a = ba
    while (i < reps) {
      a = ba.init
      i += 1
    }
    a
  }

  def timeGet3Vector(reps: Int): Any = {
    var i = 0
    var a = 0
    while (i < reps) {
      for { j <- idxs }
        a = vec(j)
      i += 1
    }
    a
  }

  def timeGet3BArray(reps: Int): Any = {
    var i = 0
    var a = 0
    while (i < reps) {
      for { j <- idxs }
        a = ba(j)
      i += 1
    }
    a
  }

  def timeFilterTrueVector(reps: Int): Any = {
    var i = 0
    var a = vec
    while (i < reps) {
      a = vec.filter(x => x >= 0)
      i += 1
    }
    a
  }

  def timeFilterTrueBArray(reps: Int): Any = {
    var i = 0
    var a = ba
    while (i < reps) {
      a = ba.filter(x => x >= 0)
      i += 1
    }
    a
  }

  def timeFilterTrueTreeSet(reps: Int): Any = {
    var i = 0
    var a = tset
    while (i < reps) {
      a = tset.filter(x => x >= 0)
      i += 1
    }
    a
  }

  def timeTakeWhileTrueVector(reps: Int): Any = {
    var i = 0
    var a = vec
    while (i < reps) {
      a = vec.takeWhile(x => x >= 0)
      i += 1
    }
    a
  }

  def timeTakeWhileTrueBArray(reps: Int): Any = {
    var i = 0
    var a = ba
    while (i < reps) {
      a = ba.takeWhile(x => x >= 0)
      i += 1
    }
    a
  }

  def timeUpdated3Vector(reps: Int): Any = {
    var i = 0
    var a = vec
    while (i < reps) {
      for { j <- idxs }
        a = vec.updated(j, j)
      i += 1
    }
    a
  }

  def timeUpdated3BArray(reps: Int): Any = {
    var i = 0
    var a = ba
    while (i < reps) {
      for { j <- idxs }
        a = ba.updated(j, j)
      i += 1
    }
    a
  }

  def timeInserted3Vector(reps: Int): Any = {
    var i = 0
    var a = vec
    while (i < reps) {
      for { j <- idxs }
        a = vec.patch(j, Vector(j), 0)
      i += 1
    }
    a
  }

  def timeInserted3BArray(reps: Int): Any = {
    var i = 0
    var a = ba
    while (i < reps) {
      for { j <- idxs }
        a = ba.inserted(j, j)
      i += 1
    }
    a
  }

  def timeRemoved3Vector(reps: Int): Any = {
    var i = 0
    var a = vec
    while (i < reps) {
      for { j <- idxs }
        a = vec.patch(j, Vector.empty, 1)
      i += 1
    }
    a
  }

  def timeRemoved3BArray(reps: Int): Any = {
    var i = 0
    var a = ba
    while (i < reps) {
      for { j <- idxs }
        a = ba.removed(j)
      i += 1
    }
    a
  }

  def timeSplitAt3Vector(reps: Int): Any = {
    var i = 0
    var a = (vec, vec)
    while (i < reps) {
      for { j <- idxs }
        a = vec.splitAt(j)
      i += 1
    }
    a
  }

  def timeSplitAt3BArray(reps: Int): Any = {
    var i = 0
    var a = (ba, ba)
    while (i < reps) {
      for { j <- idxs }
        a = ba.splitAt(j)
      i += 1
    }
    a
  }

  def timeSplitAt3TreeSet(reps: Int): Any = {
    var i = 0
    var a = (tset, tset)
    while (i < reps) {
      for { j <- idxs }
        a = tset.splitAt(j)
      i += 1
    }
    a
  }

  def timeTake3Vector(reps: Int): Any = {
    var i = 0
    var a = vec
    while (i < reps) {
      for { j <- idxs }
        a = vec.take(j)
      i += 1
    }
    a
  }

  def timeTake3BArray(reps: Int): Any = {
    var i = 0
    var a = ba
    while (i < reps) {
      for { j <- idxs }
        a = ba.take(j)
      i += 1
    }
    a
  }

  def timeTake3TreeSet(reps: Int): Any = {
    var i = 0
    var a = tset
    while (i < reps) {
      for { j <- idxs }
        a = tset.take(j)
      i += 1
    }
    a
  }

  def timeItrSizeVector(reps: Int): Any = {
    var i = 0
    var a = 0
    while (i < reps) {
      a = vec.iterator.size
      i += 1
    }
    a
  }

  def timeItrSizeBArray(reps: Int): Any = {
    var i = 0
    var a = 0
    while (i < reps) {
      a = ba.iterator.size
      i += 1
    }
    a
  }

  def timeItrSizeTreeSet(reps: Int): Any = {
    var i = 0
    var a = 0
    while (i < reps) {
      a = tset.iterator.size
      i += 1
    }
    a
  }

  def timeReverseVector(reps: Int): Any = {
    var i = 0
    var a = vec
    while (i < reps) {
      a = vec.reverse
      i += 1
    }
    a
  }

  def timeReverseBArray(reps: Int): Any = {
    var i = 0
    var a = ba
    while (i < reps) {
      a = ba.reverse
      i += 1
    }
    a
  }

  def timeReverseMapVector(reps: Int): Any = {
    var i = 0
    var a = vec
    while (i < reps) {
      a = vec.reverseMap(_ + 1)
      i += 1
    }
    a
  }

  def timeReverseMapBArray(reps: Int): Any = {
    var i = 0
    var a = ba
    while (i < reps) {
      a = ba.reverseMap(_ + 1)
      i += 1
    }
    a
  }

  def timeBinsertedBArray(reps: Int): Any = {
    var i = 0
    var a = BArray.empty[Int]
    while (i < reps) {
      a = BArray.empty[Int]
      for (x <- rndList) {
        a = a.binserted(x)
      }
      i += 1
    }
    a
  }

  def timeBinsertedVector(reps: Int): Any = {
    var i = 0
    var a = Vector.empty[Int]
    while (i < reps) {
      a = Vector.empty[Int]
      for (x <- rndList) {
        a = (a :+ x).sorted //TODO do proper binary search
      }
      i += 1
    }
    a
  }

}

object BArrayBenchmark {

  def main(args: Array[String]) {
    Runner.main(classOf[BArrayBenchmark], args)
  }

}
