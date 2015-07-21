package level1.bst

import org.scalatest.{ShouldMatchers, FlatSpec}

import scala.collection.immutable.TreeMap
import scala.collection.mutable.ListBuffer

/**
 * Created by sai on 20/07/2015.
 */
class BSTTest extends FlatSpec with ShouldMatchers {

  "find" should ("find an existing element from a binary search tree") in {

    /*
       11
      / \
     9  12
    /\
   8 10
   */

    val node_10 = BSTNode(10, None, None)
    val node_8 = BSTNode(8, None, None)
    val node_9 = BSTNode(9, Some(node_8), Some(node_10))
    val node_12 = BSTNode(12, None, None)
    val node_11 = BSTNode(11, Some(node_9), Some(node_12))
    val bst = new BST[Int](node_11)
    bst.find(10) should be(Option(10))
  }

  it should ("return None for a non existent element ") in {

    /*
       11
      / \
     9  12
    /\
   8 10
   */

    val node_10 = BSTNode(10, None, None)
    val node_8 = BSTNode(8, None, None)
    val node_9 = BSTNode(9, Some(node_8), Some(node_10))
    val node_12 = BSTNode(12, None, None)
    val node_11 = BSTNode(11, Some(node_9), Some(node_12))
    val bst = new BST[Int](node_11)
    bst.find(20) should be(None)
  }

  "inorderTraversal" should "traverse the BST in in-order (Left->Root->Right) order" in {

    /*
      40
     / \
    25  78
   /\
  10 32
  */
    val node_32 = BSTNode(32, None, None)
    val node_10 = BSTNode(10, None, None)
    val node_25 = BSTNode(25, Some(node_10), Some(node_32))
    val node_78 = BSTNode(78, None, None)
    val node_40 = BSTNode(40, Some(node_25), Some(node_78))
    val bst = new BST[Int](node_40)
    val buffer = new ListBuffer[Int]
    bst.inorderTraversal(buffer += _)
    buffer.mkString("") should be("1025324078")
  }

  "collectInorder" should "collect all the elements of the BST in in-order (Left->Root->Right) order" in {

    /*
      40
     / \
    25  78
   /\
  10 32
  */
    val node_32 = BSTNode(32, None, None)
    val node_10 = BSTNode(10, None, None)
    val node_25 = BSTNode(25, Some(node_10), Some(node_32))
    val node_78 = BSTNode(78, None, None)
    val node_40 = BSTNode(40, Some(node_25), Some(node_78))
    val bst = new BST[Int](node_40)
    bst.collectInorder should be(List(10, 25, 32, 40, 78))
  }

  "preorderTraversal" should "traverse the BST in pre-order (Root->Left->Right) order" in {

    /*
      40
     / \
    25  78
   /\
  10 32
  */
    val node_32 = BSTNode(32, None, None)
    val node_10 = BSTNode(10, None, None)
    val node_25 = BSTNode(25, Some(node_10), Some(node_32))
    val node_78 = BSTNode(78, None, None)
    val node_40 = BSTNode(40, Some(node_25), Some(node_78))
    val bst = new BST[Int](node_40)
    val buffer = new ListBuffer[Int]
    bst.preorderTraversal(buffer += _)
    buffer.mkString("") should be("4025103278")
  }

  "collectPreorder" should "collect all the elements of the BST in pre-order (Root->Left->Right) order" in {

    /*
      40
     / \
    25  78
   /\
  10 32
  */
    val node_32 = BSTNode(32, None, None)
    val node_10 = BSTNode(10, None, None)
    val node_25 = BSTNode(25, Some(node_10), Some(node_32))
    val node_78 = BSTNode(78, None, None)
    val node_40 = BSTNode(40, Some(node_25), Some(node_78))
    val bst = new BST[Int](node_40)
    bst.collectPreorder should be(List(40, 25, 10, 32, 78))
  }

  "postorderTraversal" should "traverse the BST in pre-order (Left->Right->Root) order" in {

    /*
      40
     / \
    25  78
   /\
  10 32
  */
    val node_32 = BSTNode(32, None, None)
    val node_10 = BSTNode(10, None, None)
    val node_25 = BSTNode(25, Some(node_10), Some(node_32))
    val node_78 = BSTNode(78, None, None)
    val node_40 = BSTNode(40, Some(node_25), Some(node_78))
    val bst = new BST[Int](node_40)
    val buffer = new ListBuffer[Int]
    bst.postorderTraversal(buffer += _)
    buffer.mkString("") should be("1032257840")
  }

  "collectPostorder" should "collect all the elements of the BST in post-order (Left->Right->Root) order" in {

    /*
      40
     / \
    25  78
   /\
  10 32
  */
    val node_32 = BSTNode(32, None, None)
    val node_10 = BSTNode(10, None, None)
    val node_25 = BSTNode(25, Some(node_10), Some(node_32))
    val node_78 = BSTNode(78, None, None)
    val node_40 = BSTNode(40, Some(node_25), Some(node_78))
    val bst = new BST[Int](node_40)
    bst.collectPostorder should be(List(10, 32, 25, 78, 40))
  }


}
