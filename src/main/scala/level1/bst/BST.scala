package level1.bst

import scala.collection.mutable.ListBuffer

/**
 * Created by sai on 20/07/2015.
 */

case class BSTNode[T <: Int](var data: T, var leftChild: Option[BSTNode[T]], var rightChild: Option[BSTNode[T]])

class BST[T <: Int](var root: BSTNode[T] = null) {


  def inorderTraversal(f: T => Unit): Unit = {

    def inorder(currNode: Option[BSTNode[T]]): Unit = {
      if (currNode.isDefined) {
        inorder(currNode.get.leftChild)
        f(currNode.get.data)
        inorder(currNode.get.rightChild)
      }
    }
    inorder(Some(root))
  }

  def preorderTraversal(f: T => Unit): Unit = {
    def preorder(currNode: Option[BSTNode[T]]): Unit = {
      if (currNode.isDefined) {
        f(currNode.get.data)
        preorder(currNode.get.leftChild)
        preorder(currNode.get.rightChild)
      }
    }
    preorder(Some(root))
  }

  def postorderTraversal(f: T => Unit): Unit = {

    def postorder(currNode: Option[BSTNode[T]]): Unit = {
      if (currNode.isDefined) {
        postorder(currNode.get.leftChild)
        postorder(currNode.get.rightChild)
        f(currNode.get.data)
      }
    }
    postorder(Some(root))
  }


  def collectInorder = collect(inorderTraversal)

  def collectPreorder = collect(preorderTraversal)

  def collectPostorder = collect(postorderTraversal)

  private[this] def collect(traversalFunction: (T => Unit) => Unit): List[T] = {
    val buffer = new ListBuffer[T]()
    traversalFunction(buffer += _)
    buffer.toList
  }

  def find(n: T) = {
    def _find(currNode: Option[BSTNode[T]]): Option[T] = {
      if (currNode.isDefined && currNode.get.data.compareTo(n) == 0) Some(currNode.get.data)
      else if (currNode.isDefined && currNode.get.data.compareTo(n) < 0) _find(currNode.get.rightChild)
      else if (currNode.isDefined && currNode.get.data.compareTo(n) > 0) _find(currNode.get.leftChild)
      else None
    }
    _find(Some(root))
  }

  def insert(element: T) = {
    if (root == null) root = new BSTNode(element, None, None)
    else {
      def _insert(parentNode: Option[BSTNode[T]], currNode: Option[BSTNode[T]], isLeft: Boolean): Unit = {
        if (currNode.isDefined && element < currNode.get.data) _insert(currNode, currNode.get.leftChild, true)
        if (!currNode.isDefined && isLeft) parentNode.get.leftChild = Some(BSTNode(element, None, None))
        if (currNode.isDefined && element > currNode.get.data) _insert(currNode, currNode.get.rightChild, false)
        if (!currNode.isDefined && !isLeft) parentNode.get.rightChild = Some(BSTNode(element, None, None))
      }
      _insert(Some(root), Some(root), false)
    }
  }

  def insertAll(elements: List[T]) = elements foreach insert

  override def toString = {
    root.toString
  }
}



