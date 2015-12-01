package TermBasedModel

import main._

import collection.mutable.LinkedHashMap
import scala.collection.mutable

object TFScore {

  def score(tfs: mutable.LinkedHashMap[String, Map[String, Int]], doc_id: String) {
    val scores = tfs.map{ case (q, m) => (q, m.foldLeft(0)(_+_._2)) }
    
    var counter = 0
    for ((q, score) <- scores) {
      // update the min heap
      val heap_size = main.minHeapsTerm(counter).size
      if (heap_size < 100) {
        main.minHeapsTerm(counter) += Tuple2(score, doc_id)
      } else {
        val testScore = main.minHeapsTerm(counter).head._1
        if (score > testScore) {
          main.minHeapsTerm(counter) += Tuple2(score,doc_id)
          main.minHeapsTerm(counter).dequeue()
        }
      }
      counter += 1
    }
    
  }
  
}
