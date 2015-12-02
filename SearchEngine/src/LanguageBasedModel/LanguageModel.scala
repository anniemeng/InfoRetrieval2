package LanguageBasedModel

import scala.collection.mutable
import main._
import java.io._

object LanguageModel {
  def smoothing(qmap : mutable.LinkedHashMap[String, mutable.Map[String, Int]],
                doc_id : String,
                wordmap : mutable.LinkedHashMap[String, Int],
                doc_word_count : Int,
                coll_word_count : Int ): Unit = {
    var lambda = 0.5 //TODO:Tune it
    var counter = 0
    //var writer : FileWriter  = new FileWriter("QueryInsight.txt",true)
    //writer.write("Coll_word: "+coll_word_count+"\n");
    //writer.write("doc_word: "+doc_word_count+"\n");
      
    for((query, occurence) <- qmap){
      //writer.write(query+"\n")

      // score each document
      var Score : Double = 0.0
      //writer.write("Size :"+occurence.size+"\n");
      for((term, freq) <- occurence){
        //writer.write("Term: "+term+" \n");
        //writer.write("Freq: "+freq+" \n");
        //writer.write("Word freq in coll: "+wordmap.getOrElse(term,0)+"\n")
        Score = Score + main.log2(1 + (1-lambda) * (freq.toDouble/doc_word_count) + 
            lambda * (wordmap.getOrElse(term, 0).toDouble/coll_word_count))
      }
      //writer.write("Document "+doc_id+" scored "+Score+" with query "+query)

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
    //writer.close()
    
  }
}