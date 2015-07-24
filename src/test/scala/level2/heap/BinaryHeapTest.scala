package level2.heap

import org.scalatest.{ShouldMatchers, FlatSpec}

import scala.collection.immutable.TreeSet

/**
 * Created by sai on 23/07/2015.
 */
class BinaryHeapTest extends FlatSpec with ShouldMatchers {

  "insert" should "insert an item into the min heap as a root given it's empty" in {
    val heap = new BinaryHeap
    heap.insert(1)
    heap.asString(_ + "") should be("1")
  }

  "insert" should "insert an item into the min heap as a root given it has one element already" in {
    val heap = new BinaryHeap
    heap.insert(10)
    heap.insert(7)
    heap.insert(13)
    heap.insert(11)
    heap.insert(12)
    heap.asString(_ + "|") should be("7|10|13|11|12|")
  }

  "insertAll" should "insert all items into the min heap for an empty heap" in {
    val heap = new BinaryHeap
    heap.insertAll(10, 7, 13, 11, 12, 5, 4)

    heap.asString(_ + "|") should be("4|10|5|11|12|13|7|")
  }

  "removeMin" should "remove the min element in a heap" in {
    val heap = new BinaryHeap
    heap.insertAll(10, 11, 22, 14, 15, 23, 30)
    heap.removeMin()
    heap.asString(_ + "|") should be("11|14|22|30|15|23|")
  }
}





