package LanguageBasedModel

import scala.collection.mutable
import java.io._
import scala.io.Source
import scala.util.Sorting
object Evaluation {
  def evaluate(heaps: mutable.ArrayBuffer[mutable.PriorityQueue[(Double,String)]],
               filename: String) : Unit = {
    var counter : Int = 0
    var Precision : Double = 0.0
    var AvgPrecision : Double = 0.0
    var Recall : Double = 0.0
    var AvgRecall : Double = 0.0
    var F1 : Double = 0.0
    var AverageF1 : Double = 0.0
    var avgPrecisionAtRank : Double = 0.0
    var MAP : Double = 0.0
    var Alpha : Double = 0.5
    var Beta : Double = 1 - Alpha
    var RelevantDocs = 0
    var RetrievedRelevant = 0
    var query_id : String = ""
    var docs_retrieved = new mutable.ArrayBuffer[mutable.ArrayBuffer[String]]()
    var docs_scanned : Int = 0
    //var writer : FileWriter  = new FileWriter("EvaluateInsight.txt",true)
    
    for(heap <- heaps){
      val heap_list : List[(Double,String)] = heap.toList.sortWith(_._1 > _._1)
      //writer.write("Docs "+heap_list+"\n")
      var buf = heap_list.map( x => x._2).to[mutable.ArrayBuffer]
      docs_retrieved += buf
    }
    
    //writer.write("Docs: "+docs_retrieved+"\n");
    
      
    val buffRead = new BufferedReader(new FileReader("Tipster/qrels"))
    var line : String = buffRead.readLine()
    var qrels = line.split(" ")
    
    while (line != null){
      var qrel_docs = new mutable.ArrayBuffer[String]()
      query_id = qrels(0)
      RelevantDocs = 0
      RetrievedRelevant = 0
      avgPrecisionAtRank = 0.0
      docs_scanned = 0
      while(line  != null && query_id == qrels(0)){
        val doc : String = qrels(2).filter(_.isLetterOrDigit)
        //writer.write("Doc: "+doc+"\n")
        if(qrels(3) == "1"){
          RelevantDocs += 1 //(TP+FN)
          qrel_docs += doc
        }
        line = buffRead.readLine()
        if(line != null)
          qrels = line.split(" ")
      }
      
      for(doc <- docs_retrieved(counter)){
        //writer.write("Checking with doc :"+doc+"\n")
        docs_scanned += 1
        if(qrel_docs.contains(doc)){
          RetrievedRelevant += 1 //(TP)
          avgPrecisionAtRank += (RetrievedRelevant.toDouble/docs_scanned)
          //writer.write("Present!! "+RetrievedRelevant+"\n")
        }
      }
      if(RetrievedRelevant != 0)
        avgPrecisionAtRank = avgPrecisionAtRank/RetrievedRelevant
      MAP += avgPrecisionAtRank
      counter += 1
      Precision = RetrievedRelevant.toDouble/docs_scanned
      Recall = RetrievedRelevant.toDouble/RelevantDocs
      AvgPrecision += Precision
      AvgRecall += Recall 
      if(Precision == 0 && Recall == 0)
        F1 = 0
      else
        F1 = ((Beta*Beta+1)*Precision*Recall)/(Beta*Beta*Precision+Recall)
      AverageF1 += F1
      val fw = new FileWriter("Evaluation"+ filename+".txt",true)
      fw.write("Query: "+query_id+"\n")
      fw.write("Precision: "+Precision+"\n")
      fw.write("Recall: "+Recall+"\n")
      fw.write("F1: "+F1+"\n")
      fw.close()
    }
    //writer.close()                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
    MAP = MAP.toDouble/docs_retrieved.size
    println("MAP: "+MAP)
    println("AvgPrecision: "+AvgPrecision);
    println("AvgRecall: "+AvgRecall);
    println("AvgF1: "+AverageF1);
    
  }
}
