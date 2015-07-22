package level1.linkedlist

import level2.dictionary.Dictionary


class Node[T](var data: T, var next: Node[T], var prev: Node[T])

/**
 * A Simple Doubly Linked List
 * Created by sai on 18/07/2015.
 */
class LinkedList[T] {
  private var _head: Node[T] = _


  private var _tail: Node[T] = _

  var size: Int = 0

  def insertFirst(data: T) = {
    if (_head == null) {
      _head = new Node(data, null, null)
      size = 1
      _tail = _head
    } else {
      val curr = new Node(data, _head, null)
      _head.prev = curr
      _head = curr
      size += 1
    }
  }

  def insertLast(data: T) = {
    if (_tail == null) {
      insertFirst(data)
    } else {
      val curr = new Node(data, null, _tail)
      _tail.next = curr
      _tail = curr
      size += 1
    }
  }

  def head = if (_head == null) throw new NoSuchElementException else _head.data

  def tail = if (_tail == null) throw new NoSuchElementException else _tail.data

  def asString(f: T => String) = {
    def stringConvert(currNode: Node[T], acc: String): String = {
      if (currNode == null) acc
      else stringConvert(currNode.next, acc + f(currNode.data))
    }
    stringConvert(_head, "")
  }

  def searchFor(function: T => Boolean): Option[T] = {
    def _search(currNode: Node[T]): Option[T] = {
      if (currNode != null && function(currNode.data)) Some(currNode.data)
      else if (currNode != null && !function(currNode.data)) _search(currNode.next)
      else None
    }
    _search(_head)
  }

  def each(function: T => Unit): Unit = {
    _each(function, _head, node => node.next)
  }

  def eachReverse(function: T => Unit): Unit = {
    _each(function, _tail, node => node.prev)
  }

  private[this] def _each(function: T => Unit, currNode: Node[T], nodeTraversalFunction: Node[T] => Node[T]): Unit = {
    if (currNode != null) {
      function(currNode.data)
      _each(function, nodeTraversalFunction(currNode), nodeTraversalFunction)
    }
  }

  def delete(predicate: T => Boolean) = {

    def _delete(parent: Node[T], current: Node[T], deleted: Boolean): Boolean = {
      if (current == null || deleted) deleted
      else if (!deleted && predicate(current.data)) {
        parent.next = current.next
        if (current.next != null) current.next.prev = parent
        current.next = null
        current.prev = null
        size -= 1
        _delete(current, current.next, true)
      }
      else _delete(current, current.next, false)
    }

    if (_head.next != null) {
      _delete(_head, _head, false)
    } else {
      if (predicate(_head.data)) {
        _head = null
        size -= 1
        true
      } else {
        false
      }
    }
  }
}
