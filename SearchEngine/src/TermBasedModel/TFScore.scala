package TermBasedModel

import collection.mutable.LinkedHashMap
import scala.collection.mutable

class TFScore {

  def score(tfs: mutable.LinkedHashMap[String, Map[String, Int]]): LinkedHashMap[String, Int] = {
    return tfs.map{ case (q, m) => (q, m.foldLeft(0)(_+_._2)) }
  }
  
}
