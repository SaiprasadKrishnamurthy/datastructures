package level1.stack

import org.scalatest.{FlatSpec, ShouldMatchers}

import scala.collection.mutable.ListBuffer

/**
 * Created by sai on 18/07/2015.
 */
class StackTest extends FlatSpec with ShouldMatchers {

  "push " should "insert the element in the stack for an empty stack" in {
    val stack = new Stack[Int]
    stack.push(1)
    stack.size should be(1)
  }

  it should "retrieve the elements in LIFO order" in {
    val stack = new Stack[Int]
    stack.push(1)
    stack.push(2)
    stack.push(3)
    stack.size should be(3)
    stack.pop() should be(3)
    stack.pop() should be(2)
    stack.pop() should be(1)
  }

  "each" should "iterate each of the elements in LIFO order" in {
    val stack = new Stack[Int]
    stack.push(1)
    stack.push(2)
    stack.push(3)
    val buffer = new ListBuffer[Int]
    stack.each(buffer += _)
    buffer.mkString(",") should be("3,2,1")
  }

  "searchFor" should "search for the first element matching the specified predicate" in {
    val stack = new Stack[Int]
    stack.push(1)
    stack.push(2)
    stack.push(3)
    stack.push(14)
    stack.searchFor(_ % 2 == 0) should be(Option(14))
  }

}
