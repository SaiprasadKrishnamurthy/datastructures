package level2.dictionary

import java.security.Key

import level1.linkedlist.LinkedList

/**
 * Created by sai on 22/07/2015.
 */
class Dictionary[K, V](capacity: Int = 100) {
  // Array of Linked Lists containing tuples.
  val buckets = new Array[LinkedList[(K, V)]](capacity)


  def put(key: K, value: V) = {
    val index = hash(key)
    buckets(index) match {
      case existingList if (existingList != null) => {
        // Traverse through the linkedlist and replace the entry.
        existingList.delete(_._1.equals(key))
        existingList.insertFirst((key, value))
        true
      }
      case _ => {
        val list = new LinkedList[(K, V)]
        list.insertFirst((key, value))
        buckets(index) = list
        true
      }

    }
  }

  def get(key: K) = {
    buckets(hash(key)) match {
      case existingList if (existingList != null) => {
        existingList.searchFor(_._1.equals(key)) match {
          case Some(tuple) => Some(tuple._2)
          case _ => None
        }
      }
      case _ => None
    }
  }

  def remove(key: K) = {
    val index = hash(key)
    buckets(index) match {
      case existingList if (existingList != null) => {
        val deleted = existingList.delete(_._1.equals(key))
        deleted
      }
      case _ => false
    }
  }

  def hash(key: K) = key.hashCode % capacity
}
