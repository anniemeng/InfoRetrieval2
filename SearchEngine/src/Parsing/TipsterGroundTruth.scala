package Parsing

import scala.io.Source
import scala.collection.immutable.Map

class TipsterGroundTruth(path:String) {
  val judgements: Map[String, Array[String]] =
    Source.fromFile(path).getLines()
      .filter(l => !l.endsWith("0"))
      .map(l => l.split(" "))
      .map(e => (e(0),e(2).replaceAll("-", "")))
      .toArray
      .groupBy(_._1)
      .mapValues(_.map(_._2))

}

object TipsterGroundTruth {

  def main (args:Array[String]){
    val t = new TipsterGroundTruth("/Users/thofmann/Data/TREC/qrels.51-100.disk1.disk2")
    t.judgements.foreach(j => println("Topic "+j._1 +": "+j._2.size+" judgements found."))
  }
}
