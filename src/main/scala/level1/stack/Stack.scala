package level1.stack

/**
 * Created by sai on 18/07/2015.
 */
class Stack[T: Manifest](capacity: Int = 1000) {
  private[this] var head: Int = -1
  private[this] var tail: Int = -1
  private[this] var _size: Int = _
  private[this] val arr = new Array[T](capacity)

  def push(data: T) = {
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

  def pop() = {
    if (tail < head) throw new NoSuchElementException("Stack empty")
    else {
      val data = arr(tail)
      tail -= 1
      _size -= 1
      data
    }
  }

  def size = _size

  def each(f: T => Unit) = {
    while (head <= tail) f(pop())
  }

  def searchFor(function: T => Boolean): Option[T] = {
    def _search(currTail: Int): Option[T] = {
      if (currTail >= head && function(arr(currTail))) Some(arr(currTail))
      else if (currTail >= head && !function(arr(currTail))) _search(currTail - 1)
      else None
    }
    _search(tail)
  }

}
