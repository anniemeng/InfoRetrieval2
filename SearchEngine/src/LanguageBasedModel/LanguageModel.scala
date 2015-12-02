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
    var JMlambda = 0.1 //Jelinek-Mercer
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
        /*Score = Score + main.log2(1 + (1-JMlambda) * (freq.toDouble/doc_word_count) + 
            (JMlambda) * (wordmap.getOrElse(term, 0).toDouble/coll_word_count))*/
        var DirConst = 2000  
        var DirichletDenom : Double = doc_word_count+DirConst
        var DirichletNum : Double = freq+((DirConst)*(wordmap.getOrElse(term, 0.0)/coll_word_count))
        Score = Score + main.log2(1+ (DirichletNum.toDouble/DirichletDenom))
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