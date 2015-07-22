package level2.dictionary

import org.scalatest.{ShouldMatchers, FlatSpec}

/**
 * Created by sai on 22/07/2015.
 */
class DictionaryTest extends FlatSpec with ShouldMatchers {

  "put" should "put the key value pair into the empty dictionary " in {
    val dictionary = new Dictionary[Int, String]()
    dictionary.put(1, "One") should be(true)
  }

  "get" should "get the value for an existing key in the dictionary " in {
    val dictionary = new Dictionary[Int, String]()
    (1 to 50).foreach(i => dictionary.put(i, i.toString))
    dictionary.get(16) should be(Some("16"))
  }

  "put" should "replace the value for an existing key in the dictionary " in {
    val dictionary = new Dictionary[Int, String]()
    (1 to 50).foreach(i => dictionary.put(i, i.toString))
    dictionary.put(34, "Thirty four")
    dictionary.get(34) should be(Some("Thirty four"))
  }

  "remove" should "remove the an existing entry in the dictionary " in {
    val dictionary = new Dictionary[Int, String]()
    (1 to 50).foreach(i => dictionary.put(i, i.toString))
    dictionary.remove(34) should be(true)
    dictionary.get(34) should be(None)
  }
}
