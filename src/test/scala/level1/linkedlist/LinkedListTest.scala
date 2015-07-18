package level1.linkedlist

import org.scalatest.{FlatSpec, ShouldMatchers}

import scala.collection.mutable.ListBuffer

/**
 * Created by sai on 18/07/2015.
 */
class LinkedListTest extends FlatSpec with ShouldMatchers {

  "insertFirst" should "insert the element at the head of the empty List" in {
    val linkedList = new level1.linkedlist.LinkedList[Int]
    linkedList.insertFirst(1)
    linkedList.head should be(1)
    linkedList.size should be(1)
  }

  it should "insert the element at the head of the non empty List" in {
    val linkedList = new level1.linkedlist.LinkedList[Int]

    linkedList.insertFirst(1)
    linkedList.insertFirst(2)
    linkedList.head should be(2)
    linkedList.size should be(2)
    linkedList.asString(_.toString) should be("21")
  }

  "insertLast" should "insert the element at the tail of the empty List" in {
    val linkedList = new level1.linkedlist.LinkedList[Int]
    linkedList.insertLast(1)
    linkedList.tail should be(1)
    linkedList.size should be(1)
  }

  it should "insert the element at the tail of the non empty List" in {
    val linkedList = new level1.linkedlist.LinkedList[Int]

    linkedList.insertLast(2)
    linkedList.insertLast(1)
    linkedList.head should be(2)
    linkedList.tail should be(1)
    linkedList.size should be(2)
    linkedList.asString(_.toString) should be("21")
  }

  it should "iterate through all the elements from head to tail" in {
    val linkedList = new level1.linkedlist.LinkedList[Int]

    linkedList.insertFirst(2)
    linkedList.insertFirst(1)
    linkedList.insertFirst(0)
    linkedList.asString(_.toString) should be("012")
    val buffer = new ListBuffer[Int]
    linkedList.each(buffer += _)
    buffer.mkString("") should be("012")
  }

  it should "iterate through all the elements from tail to head" in {
    val linkedList = new level1.linkedlist.LinkedList[Int]
    linkedList.insertFirst(2)
    linkedList.insertFirst(1)
    linkedList.insertFirst(0)
    linkedList.asString(_.toString) should be("012")
    val buffer = new ListBuffer[Int]
    linkedList.eachReverse(buffer += _)
    buffer.mkString("") should be("210")
  }
}
