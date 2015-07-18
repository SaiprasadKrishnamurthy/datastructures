package level1.queue

import java.util.NoSuchElementException

/**
 * Created by sai on 18/07/2015.
 */
class Queue[T: Manifest](capacity: Int = 1000) {

  private[this] var head: Int = -1
  private[this] var tail: Int = -1
  private[this] var _size: Int = _
  private[this] val arr = new Array[T](capacity)

  def enqueue(data: T) = {
    if (head < 0) {
      _size += 1
      head += 1
      tail += 1
      arr(head) = data
    } else {
      tail += 1
      arr(tail) = data
      _size += 1
    }
  }

  def dequeue() = {
    if (head > tail) throw new NoSuchElementException("Queue empty")
    else {
      val data = arr(head)
      head += 1
      _size -= 1
      data
    }
  }

  def size = _size

  def each(f: T => Unit) = {
    while(head <= tail) f(dequeue())
  }

  def searchFor(function: T => Boolean): Option[T] = {
    def _search(currHead: Int): Option[T] = {
      if (currHead <= tail && function(arr(currHead))) Some(arr(currHead))
      else if (currHead <= tail && !function(arr(currHead))) _search(currHead + 1)
      else None
    }
    _search(head)
  }

}
