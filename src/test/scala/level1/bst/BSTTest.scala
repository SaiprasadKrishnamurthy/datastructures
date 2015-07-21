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
    bst.find(10) should be(Some(10))
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

  "insert" should "insert a single element as root for an empty BST" in {

    val bst = new BST[Int]

    /*
      40
    */
    bst.insert(40)
    bst.collectInorder should be(List(40))
  }

  "insertAll" should "insert all the elements in the right locations in a BST" in {

    val bst = new BST[Int]

    /*
       40
      / \
     25  78
    /\
   10 32
      /\
    30 34
       /
      33
   */
    bst.insertAll(List(78, 25, 40, 10, 32, 30, 34, 33))
    bst.collectInorder should be(List(10, 25, 30, 32, 33, 34, 40, 78))
  }

  "delete" should "delete an existing element found in the leaf node" in {

    /*
              78
             /
            25
          /   \
         10   40
              /
             32
            / \
          30  34
              /
             33
     */
    val bst = new BST[Int]
    bst.insertAll(List(78, 25, 40, 10, 32, 30, 34, 33))
    bst.delete(33) should be(true)
    bst.collectInorder should be(List(10, 25, 30, 32, 34, 40, 78))
  }

  "delete" should "NOT delete a non-existing element in the BST" in {

    /*
              78
             /
            25
          /   \
         10   40
              /
             32
            / \
          30  34
              /
             33
     */
    val bst = new BST[Int]
    bst.insertAll(List(78, 25, 40, 10, 32, 30, 34, 33))
    bst.delete(133) should be(false)
    bst.collectInorder should be(List(10, 25, 30, 32, 33, 34, 40, 78))
  }

  "delete" should "delete an existing element found in the node with one child" in {

    /*
              78
             /
            25
          /   \
         10   40
              /
             32
            / \
          30  34
              /
             33
     */
    val bst = new BST[Int]
    bst.insertAll(List(78, 25, 40, 10, 32, 30, 34, 33))
    bst.delete(34) should be(true)
    bst.collectInorder should be(List(10, 25, 30, 32, 33, 40, 78))
  }

  "delete" should "delete an existing element found in the node with both left and right subtree" in {

    /*
              78                                      78
             /                                        /
            25                On delete(32)          25
          /   \            ------------>            /  \
         10   40                                   10  40
              /                                       /
             32                                      33
            / \                                     / \
          30  34                                   30 34
              /
             33






     */
    val bst = new BST[Int]
    bst.insertAll(List(78, 25, 40, 10, 32, 30, 34, 33))
    bst.delete(32) should be(true)
    bst.collectInorder should be(List(10, 25, 30, 33, 34, 40, 78))

  }

  "delete" should "delete an existing element found in the node with both left and right subtree for a more dense/deep BST" in {

    /*
             Before Delete
             -------------
              78
             /
            25
          /   \
         10   40¡
              /
             32
            /  \
          30   34
             /   \
            33   38
                /  \
               37  39
              /
            36
           /
          35

          After Delete
          --------------
             78
             /
            30
          /   \
         10   40
              /
             32
               \
               34
             /   \
            33   38
                /  \
               37  39
              /
            36
           /
          35
     */
    val bst = new BST[Int]
    bst.insertAll(List(78, 25, 40, 10, 32, 30, 34, 33, 38, 37, 39, 36, 35))
    bst.delete(25) should be(true)
    bst.collectInorder should be(List(10, 30, 32, 33, 34, 35, 36, 37, 38, 39, 40, 78))

  }


}
