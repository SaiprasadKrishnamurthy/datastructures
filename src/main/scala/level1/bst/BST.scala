package level1.bst

import scala.collection.mutable.ListBuffer

/**
 * Created by sai on 20/07/2015.
 */

case class BSTNode[T <: Int](data: T, leftChild: Option[BSTNode[T]], rightChild: Option[BSTNode[T]])

class BST[T <: Int](root: BSTNode[T]) {


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

  override def toString = {
    root.toString
  }
}
