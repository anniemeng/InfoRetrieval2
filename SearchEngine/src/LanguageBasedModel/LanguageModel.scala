package LanguageBasedModel

import scala.collection.mutable
import main._
import java.io._

object LanguageModel {
  def smoothing(qmap : mutable.LinkedHashMap[String, mutable.Map[String, Double]],
                doc_id : String,
                wordmap : mutable.LinkedHashMap[String, Double],
                doc_word_count : Int,
                coll_word_count : Int ): Unit = {
    var JMlambda = 0.1 //Jelinek-Mercer - not used for final topics
    var counter = 0
      
    for((query, occurence) <- qmap){
      // score each document
      var Score : Double = 0.0
      for ((term, freq) <- occurence){
        /*Score = Score + main.log2(1 + (1-JMlambda) * (freq.toDouble/doc_word_count) + 
            (JMlambda) * (wordmap.getOrElse(term, 0).toDouble/coll_word_count))*/
        val DirConst = 500
        val DirichletDenom : Double = doc_word_count + DirConst
        val DirichletNum : Double = freq + (DirConst * (wordmap.getOrElse(term, 0.0)/coll_word_count))
        Score = Score + main.log2(1 + (DirichletNum.toDouble/DirichletDenom))
      }

      // update the min heap
      val heap_size = main.minHeapsLang(counter).size
      if (heap_size < 100) {
        main.minHeapsLang(counter) += Tuple2(Score, doc_id)
      } else {
        val testScore = main.minHeapsLang(counter).head._1
        if (Score > testScore) {
          main.minHeapsLang(counter) += Tuple2(Score,doc_id)
          main.minHeapsLang(counter).dequeue()
        }
      }
      counter += 1
    }
  }
}