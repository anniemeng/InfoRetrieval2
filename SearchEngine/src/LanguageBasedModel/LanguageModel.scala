package LanguageBasedModel

import scala.collection.mutable
import main._

class LanguageModel {
  def smoothing(qmap : mutable.LinkedHashMap[String, Map[String, Int]], doc_id : String, wordmap : mutable.LinkedHashMap[String, Int], doc_word_count : Int, coll_word_count : Int ): Unit = {
    
    var lambda = 0.5//Tune it
    var counter = 0
    for((query,occurence) <- qmap){
      println(query)
      var Score : Double = 1
      for((term,freq) <- occurence){
        Score = Score * ((1-lambda)*(freq.toDouble/doc_word_count) + (lambda)*(wordmap(term).toFloat/coll_word_count))
      }
      println("Document "+doc_id+" scored "+Score+" with query "+query)
      main.maxHeap(counter) += Tuple2(Score,doc_id)
      var heap_size = main.maxHeap(counter).size;
      if(heap_size > 100){
        main.maxHeap(counter) = main.maxHeap(counter).dropRight(100)
      }
      counter += 1
    }
   
    Evaluation.evaluate(main.maxHeap)
    
  }
}