package main

import java.io.{FileInputStream, BufferedInputStream}
import java.io._

import Parsing._
import LanguageBasedModel._
import TermBasedModel._
import scala.util.control.Breaks._

import collection.immutable.Map
import collection.mutable.{Map=>MutMap}
import collection.mutable.LinkedHashMap
import collection.mutable.MutableList
import scala.collection.mutable
import scala.collection.mutable.PriorityQueue
import LanguageBasedModel._

object main {
  var minHeapsLang = new mutable.ArrayBuffer[mutable.PriorityQueue[(Double,String)]]()
  var minHeapsTerm = new mutable.ArrayBuffer[mutable.PriorityQueue[(Double,String)]]()
  var stopWords = Set("in", "from", "to", "the", "for", "of", "by", "on", "what", "does")

  def main(args: Array[String]) {

    /******************************
     * 1) GATHERING QUERIES
     ********************************/
    val topicStream = DocStream.getStream("Tipster/topics-final")
    val topics = scala.io.Source.fromInputStream(topicStream).mkString.split("</top>")

    // topic number to query
    val topicNumToTitle = mutable.LinkedHashMap[String, String]() // preserve order

    // all terms of all queries
    var queryTerms = Set[String]()
    for (topicBody <- topics) {
      if (topicNumToTitle.size == 40) {
        break
      }
      val numParts = topicBody.split("Number:")
      val topicParts = topicBody.split("Topic:")

      if (numParts.length > 1 && topicParts.length > 1) {
        // get topic number and query
        val num = numParts(1).split("\n")(0).trim().toInt.toString // strip the 0
        val title = topicParts(1).split("\n\n")(0).trim()
        val newTitle = title.replaceAll("[^A-Za-z0-9 \n.'\\/-]", "").replaceAll("[\\/\n-]", " ").replaceAll("  ", " ").replaceAll("  ", " ").replaceAll("    ", " ").toLowerCase

        // TODO: STOP WORDS
        val filtered = newTitle.split(" ").filterNot(stopWords).mkString(" ")
        topicNumToTitle(num) = filtered

        // TODO: comment out if using STOP WORDS
        //topicNumToTitle(num) = newTitle

        // collect query terms (individual words in each query) for general access later on
        val terms = newTitle.split(" ")
        for (term <- terms) {
          queryTerms += term
        }
      }
    }

     /******************************
      * 2) INITIALIZE MIN HEAP FOR EACH QUERY
      *******************************/
    val query_count = topicNumToTitle.size
    for (i <- 1 to query_count){
      var heapLang = mutable.PriorityQueue.empty[(Double, String)](
                  implicitly[Ordering[(Double, String)]].reverse
                 )
      var heapTerm = mutable.PriorityQueue.empty[(Double, String)](
                  implicitly[Ordering[(Double, String)]].reverse
                 )
      this.minHeapsLang += heapLang
      this.minHeapsTerm += heapTerm
    }

    /******************************
     * 3) PROCESS DOCUMENT STREAM - FIRST PASS
     ******************************/
    /*
     * 1) Get total number of words in general
     * 2) Get total number of documents in collection
     * 3) Get query term frequency for all documents in the collection
     * 4) Get number of documents that each query term appears in for the whole collection
     */

    // total number of words in collection - LANG MODEL
    var numWordsCollection = 0

    // number of documents in collection - TERM MODEL
    var numDocuments = 0

    // query terms freq for WHOLE document collection - LANG MODEL
    val queryTermsFreqTotal = mutable.LinkedHashMap[String, Double]()
    queryTerms.foreach { t => queryTermsFreqTotal(t) = 0.0 }

    // number of documents that have each query term for WHOLE document - TERM MODEL
    val queryTermsNumDocuments = mutable.LinkedHashMap[String, mutable.LinkedHashMap[String, Double]]()

    val sw = new StopWatch
    sw.start

    var tipster = new TipsterCorpusIterator("Tipster/zips")
    while (tipster.hasNext) {
      val doc = tipster.next()
      numDocuments += 1
      numWordsCollection += doc.tokens.length

      // query term frequency
      // frequency of all tokens in document
      val tfs : Map[String,Int]= doc.tokens.groupBy(identity).mapValues(l => l.length)

      // filter map of all tokens to only those in query
      val qtfs = tfs.filterKeys(queryTerms)

      // add to query term frequency for whole doc collection
      for ((queryTerm, freq) <- qtfs) {
        val count = queryTermsFreqTotal.getOrElse(queryTerm, 0.0)
        queryTermsFreqTotal(queryTerm) = count + freq
      }

      // add to num doc count of each term in query
      for ((_, topic) <- topicNumToTitle) {
        val qterms = topic.split(" ")
        val map = queryTermsNumDocuments.getOrElse(topic, mutable.LinkedHashMap[String, Double]())
        for (term <- qterms) {
          // set num doc to 0 if query term not in
          val curr = map.getOrElse(term, 0.0)
          val additional = if (qtfs.getOrElse(term, 0) > 0) 1.0 else 0.0
          map(term) = curr + additional
        }
        queryTermsNumDocuments(topic) = map
      }
    }

    sw.stop
    println("Stopped time = " + sw.stopped)

     /******************************
      * 4) PROCESS DOCUMENT STREAM - SECOND PASS
      ******************************/
    /*
     * 1) Find word count of document
     * 2) Get frequency of each query term in the document for all queries
     * 3) Call models
     */
    val sw2 = new StopWatch
    sw2.start

    tipster = new TipsterCorpusIterator("Tipster/zips")
    while (tipster.hasNext) {
      val doc = tipster.next()

      // get word count of document
      val numWordsDoc = doc.tokens.length
      val docId = doc.name

      // get frequency of each term for query terms
      // for each query, get a word in the query -> frequency
      val queryTermsToFreq = mutable.LinkedHashMap[String, MutMap[String, Double]]()

      // frequency of all tokens in document
      val tfs : Map[String,Int]= doc.tokens.groupBy(identity).mapValues(l => l.length)

      // filter map of all tokens to only those in query
      for ((_, topic) <- topicNumToTitle) {
        val qterms = topic.split(" ")
        val qtfs = MutMap[String, Double]()
        qterms.foreach { q => qtfs(q) = 0.0 }
        tfs.filterKeys(qterms.toSet).foreach{case (k,v) => qtfs(k) = v}
        queryTermsToFreq(topic) = qtfs
      }

      // PASS TO MODEL
      LanguageModel.smoothing(queryTermsToFreq, docId, queryTermsFreqTotal, numWordsDoc, numWordsCollection)
      //TFScore.score(queryTermsToFreq, tfs.values.sum.toDouble, docId)
      TFIDFScore.score(queryTermsToFreq, tfs.values.sum.toDouble, queryTermsNumDocuments, numDocuments, docId)
    }

    sw2.stop
    println("Stopped time = " + sw2.stopped)

     /******************************
      * 5) PRINT OUT TOP 100 OF EACH QUERY
      ******************************/

    // term
    var heapNumTerm = 0
    val heapTermFile = new File("ranking-t-12.run")
    val htbw = new BufferedWriter(new FileWriter(heapTermFile))

    // lang
    var heapNumLang = 0
    val heapLangFile = new File("ranking-l-12.run")
    val hlbw = new BufferedWriter(new FileWriter(heapLangFile))

    for ((num, topics) <- topicNumToTitle) {
      // term
      val minHeapTerm = minHeapsTerm(heapNumTerm)
      var rankT = 1
      val reverseHeapTerm = minHeapTerm.takeRight(100).reverse
      while (reverseHeapTerm.nonEmpty) {
        val doc = reverseHeapTerm.dequeue()
        htbw.write(num + " " + rankT.toString + " " + doc._2 + "\n")
        rankT += 1
      }

       // lang
       val minHeapLang = minHeapsLang(heapNumLang)
       val reverseHeapLang = minHeapTerm.takeRight(100).reverse
       var rankL = 1
       while(reverseHeapLang.nonEmpty) {
         val doc = reverseHeapLang.dequeue()
         hlbw.write(num + " " + rankL.toString + " " + doc._2 + "\n")
         rankL += 1
       }

       heapNumTerm += 1
       heapNumLang += 1
    }
    htbw.close()
    hlbw.close()

    Evaluation.evaluate(minHeapsLang, "lang")
    Evaluation.evaluate(minHeapsTerm, "term")
  }
  def log2 (x: Double) = scala.math.log10(x) / scala.math.log10(2.0)
}
