package LanguageBasedModel

import scala.collection.mutable
import java.io._;
import scala.io.Source

object Evaluation {
  def evaluate(heaps: mutable.ArrayBuffer[mutable.PriorityQueue[(Double,String)]]) : Unit = {
    println("Hi");
    var counter : Int = 0
    var Precision : Double = 0.0
    var Recall : Double = 0.0
    var F1 : Double = 0.0
    var AP : Double = 0.0
    var MAP : Double = 0.0
    var Alpha : Double = 0.0
    var Beta : Double = 1 - Alpha
    var RelevantDocs = 0
    var RetrievedRelevant = 0
    var query_id : String = ""
    var docs_retrieved = List[List[String]]()
    for(heap <- heaps)
      docs_retrieved ::= heap.toList.flatMap{case (a,b) => List(b)}
         
    val buffRead = new BufferedReader(new FileReader("qrels2")) 
    var line : String = buffRead.readLine()
    var qrels = line.split(" ")
    while (line != null){
      qrels = line.split(" ")
      var qrel_docs = new mutable.ArrayBuffer[String]()
      var query_id = qrels(0)
      RelevantDocs = 0
      RetrievedRelevant = 0
      while(line  != null && query_id == qrels(0)){
        if(qrels(3) == "1"){
          RelevantDocs += 1 //(TP+FN)
          qrel_docs += qrels(2)
        }
        line = buffRead.readLine()
        if(line != null)
          qrels = line.split(" ")
      }
      var docs_scanned : Int = 0
      val iterator = docs_retrieved(counter).reverseIterator
      var avgPrecision : Double = 0.0
      while(iterator.hasNext){
        docs_scanned += 1
        if(qrel_docs.contains(iterator.next)){
          RetrievedRelevant += 1 //(TP)
          avgPrecision += (RetrievedRelevant.toDouble/docs_scanned)
        }
      }
      avgPrecision = avgPrecision/RetrievedRelevant
      MAP += avgPrecision
      counter += 1
      Precision = (RetrievedRelevant.toDouble/docs_scanned)
      Recall = (RetrievedRelevant.toDouble/RelevantDocs)
      F1 = ((Beta*Beta+1)*Precision*Recall)/(Beta*Beta*Precision+Recall)
      val fw = new FileWriter("Evaluation.txt",true);
      fw.write("Query: "+query_id+"\n");
      fw.write("Precision: "+Precision+"\n");
      fw.write("Recall: "+Recall+"\n");
      fw.write("F1: "+F1+"\n");
      fw.close()
    }
    MAP = MAP.toDouble/docs_retrieved.size
    println("MAP: "+MAP);
  }
}
