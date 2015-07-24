package level2.heap

import scala.collection.mutable.ArrayBuffer

/**
 * Not generified - Only works for Ints.
 * Created by sai on 23/07/2015.
 */
class BinaryHeap(capacity: Int = 100) {


  private[this] val arr = new Array[Int](capacity)
  private var end = 1

  arr(0) = Int.MinValue


  def insert(data: Int) = {
    arr(end) = data
    applyHeapInvariants(end, _bubbleupExchange)
    end += 1
  }

  def insertAll(data: Int*) = {
    data.foreach(insert)
  }

  def removeMin() = {
    arr(1) = arr(end - 1)
    end -= 1
    applyHeapInvariants(1, _bubbledownExchange)

  }

  private[this] def applyHeapInvariants(start: Int, f: Int => Unit) = {
    f(start)
  }

  private[this] def _bubbleupExchange(currIndex: Int): Unit = {
    val parentIndex = currIndex / 2
    if (parentIndex != 0) {
      val currValue = arr(currIndex)
      val parentValue = arr(parentIndex)
      if (parentValue > currValue) {
        arr(parentIndex) = currValue
        arr(currIndex) = parentValue
      }
      _bubbleupExchange(parentIndex)
    }
  }

  private[this] def _bubbledownExchange(currIndex: Int): Unit = {
    val leftChildIndex = 2 * currIndex
    val rightChildIndex = (2 * currIndex) + 1
    val smallestChild = List(leftChildIndex, rightChildIndex).filter(_ <= end).map(index => (leftChildIndex, arr(leftChildIndex))).sortBy(_._2).toList
    if (!smallestChild.isEmpty) {
      val smallestElement = smallestChild(0)._2
      val currElement = arr(currIndex)
      arr(currIndex) = smallestElement
      arr(smallestChild(0)._1) = currElement
      if (currIndex != smallestChild(0)._1) {
        _bubbledownExchange(smallestChild(0)._1)
      }
    }
  }


  def asString(function: Int => String): String = {

    def _asString(currElement: Int, currIndex: Int, acc: String): String = {
      if (currIndex == end) acc
      else _asString(arr(currIndex), currIndex + 1, acc + function(arr(currIndex)))
    }
    if (end == 1) "" else _asString(arr(1), 1, "")
  }

}
