package level1.queue

import org.scalatest.{FlatSpec, ShouldMatchers}

import scala.collection.mutable.ListBuffer

/**
 * Created by sai on 18/07/2015.
 */
class QueueTest extends FlatSpec with ShouldMatchers {

  "enqueue " should "insert the element in the queue for an empty queue" in {
    val queue = new Queue[Int]
    queue.enqueue(1)
    queue.size should be(1)
  }

  it should "retrieve the elements in FIFO order" in {
    val queue = new Queue[Int]
    queue.enqueue(1)
    queue.enqueue(2)
    queue.enqueue(3)
    queue.size should be(3)
    queue.dequeue() should be(1)
    queue.dequeue() should be(2)
    queue.dequeue() should be(3)
  }

  "each" should "iterate each of the elements in FIFO order" in {
    val queue = new Queue[Int]
    queue.enqueue(1)
    queue.enqueue(2)
    queue.enqueue(3)
    val buffer = new ListBuffer[Int]
    queue.each(buffer += _)
    buffer.mkString(",") should be("1,2,3")
  }

  "searchFor" should "search for the first element matching the specified predicate" in {
    val queue = new Queue[Int]
    queue.enqueue(1)
    queue.enqueue(2)
    queue.enqueue(3)
    queue.enqueue(14)
    queue.searchFor(_ % 2 == 0) should be(Option(2))
  }

}
