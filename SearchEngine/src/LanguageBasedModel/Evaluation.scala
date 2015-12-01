
import scala.collection.mutable
import java.io._;
import scala.io.Source

class Evaluation {
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
    var RetrievedDocs = 100//Doubt?? Should set it to heap size
    var query_id : String = ""
    var docs_retrieved = List[List[String]]()
    for(heap <- heaps)
      docs_retrieved ::= heap.toList.flatMap{case (a,b) => List(b)}
         
    for(line <- Source.fromFile("qrels").getLines()){
      var qrels = line.split(" ").toList
      if(query_id == "")/*For first time*/
        query_id = qrels(0)
      if(!(query_id == qrels(0))){  
        Precision = (RetrievedRelevant.toDouble/RetrievedDocs)
        Recall = (RetrievedRelevant.toDouble/RelevantDocs)
        F1 = ((Beta*Beta+1)*Precision*Recall)/(Beta*Beta*Precision+Recall)
        val fw = new FileWriter("Evaluation.txt",true);
        fw.write("Query: "+query_id+"\n");
        fw.write("Precision: "+Precision+"\n");
        fw.write("Recall: "+Recall+"\n");
        fw.write("F1: "+F1+"\n");
        fw.close()
        query_id = qrels(0)
        RelevantDocs = 0
        RetrievedRelevant = 0
        RetrievedDocs = 100
      }
      if(qrels(3) == "1"){
        RelevantDocs += 1 //(TP+FN)
        if(docs_retrieved(counter).contains(qrels(2))){//Relevant Doc Retrieved
          RetrievedRelevant += 1 //(TP)
        }
      }
    }
  }
}