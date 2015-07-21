package level2.bst

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

  private[this] def _find(n: T, currNode: Option[BSTNode[T]]): Option[BSTNode[T]] = {
    if (currNode.isDefined && currNode.get.data.compareTo(n) == 0) currNode
    else if (currNode.isDefined && currNode.get.data.compareTo(n) < 0) _find(n, currNode.get.rightChild)
    else if (currNode.isDefined && currNode.get.data.compareTo(n) > 0) _find(n, currNode.get.leftChild)
    else None
  }

  def find(n: T) =
    _find(n, Some(root)) match {
      case Some(bstNode) => Some(bstNode.data)
      case _ => None
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

  def delete(element: T) = {
    def parentAndChild(parent: Option[BSTNode[T]], currNode: Option[BSTNode[T]], isLeftChild: Boolean): (Option[BSTNode[T]], Option[BSTNode[T]], Boolean) = {
      if (currNode.isDefined) {
        if (element < currNode.get.data) parentAndChild(currNode, currNode.get.leftChild, true)
        else if (element > currNode.get.data) parentAndChild(currNode, currNode.get.rightChild, false)
        else (parent, currNode, isLeftChild)
      }
      else (None, None, false)
    }
    val parentAndChildNode = parentAndChild(Some(root), Some(root), false)

    val parent = parentAndChildNode._1
    val nodeToBeDeleted = parentAndChildNode._2
    val leftChildOf = parentAndChildNode._3

    // Non existent
    if (nodeToBeDeleted == None) false
    else if (isLeaf(nodeToBeDeleted) && leftChildOf) {
      parent.get.leftChild = None;
      true;
    }
    // Leaf
    else if (isLeaf(nodeToBeDeleted) && !leftChildOf) {
      parent.get.rightChild = None;
      true;
    }
    // Node with a single child
    else if (!(nodeToBeDeleted.get.leftChild != None && nodeToBeDeleted.get.rightChild != None)) {
      val successor = List(nodeToBeDeleted.get.leftChild, nodeToBeDeleted.get.rightChild).filter(_.isDefined)(0)
      if (leftChildOf) parent.get.leftChild = successor else parent.get.rightChild = successor
      true;
    }
    // Node with two children
    else {
      val immediateRight = nodeToBeDeleted.get.rightChild
      val successorsCurrentParent = parentOfLeftMostLeaf(immediateRight)
      val successor = successorsCurrentParent.leftChild
      successorsCurrentParent.leftChild = None
      if (leftChildOf) parent.get.leftChild = successor else parent.get.rightChild = successor
      successor.get.rightChild = nodeToBeDeleted.get.rightChild
      successor.get.leftChild = nodeToBeDeleted.get.leftChild
      true;
    }
  }

  def parentOfLeftMostLeaf(currNode: Option[BSTNode[T]]): BSTNode[T] = {
    if (currNode.isDefined && currNode.get.leftChild.isDefined && !currNode.get.leftChild.get.leftChild.isDefined) currNode.get
    else parentOfLeftMostLeaf(currNode.get.leftChild)
  }

  private[this] def isLeaf(nodeToBeDeleted: Option[BSTNode[T]]): Boolean = {
    nodeToBeDeleted.get.leftChild == None && nodeToBeDeleted.get.rightChild == None
  }

  override def toString = {
    root.toString
  }
}



