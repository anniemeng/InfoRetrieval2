package main

import java.io.{FileInputStream, BufferedInputStream}
import java.io._

import Parsing._
import LanguageBasedModel._
import TermBasedModel._

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

  // binary relevance judgement
  /*
  def qrels(topicNumToTitle: mutable.LinkedHashMap[String, String]): Unit = {
    val qrelStream = DocStream.getStream("Tipster/qrels")
    val qrels = scala.io.Source.fromInputStream(qrelStream).getLines()

    val topicToRelevant = mutable.LinkedHashMap[String, mutable.MutableList[String]]()
    val topicToIrrelevant = mutable.LinkedHashMap[String, mutable.MutableList[String]]()
    for (qrel <- qrels) {
      val values = qrel.split(" ")
      val topic = values(0)
      val docId = values(2)
      val relevant = values(3)

      if (relevant == "1") {
        var existing = topicToRelevant.getOrElse(topic, mutable.MutableList[String]())
        existing += docId
        topicToRelevant(topic) = existing
      } else {
        var existing = topicToIrrelevant.getOrElse(topic, mutable.MutableList[String]())
        existing += docId
        topicToIrrelevant(topic) = existing
      }
    }

    /************* TESTING QRELS *****************/
    for ((topicNum, topicTitle) <- topicNumToTitle) {
      println("topic num: " + topicNum)
      println("topic: " + topicTitle)
      println("relevant: ")
      for (relevant <- topicToRelevant.getOrElse(topicNum, mutable.MutableList[String]())) {
        println(relevant)
      }

      println("irrelevant")
      for (irrelevant <- topicToIrrelevant.getOrElse(topicNum, mutable.MutableList[String]())) {
        println(irrelevant)
      }
      println("")
    }
  }
  */

  def main(args: Array[String]) {

    // TODO: let user specify how many n best queries they want

    /************* TESTING - WRITE OUTPUT TO FILE  *****************/
    //val file = new File("output.txt")
    //val bw = new BufferedWriter(new FileWriter(file))

    // 1) GATHERING QUERIES
    val topicStream = DocStream.getStream("Tipster/topics")
    val topics = scala.io.Source.fromInputStream(topicStream).mkString.split("</top>")

    val topicNumToTitle = mutable.LinkedHashMap[String, String]() // preserve order
    var queryTerms = Set[String]()
    for (topicBody <- topics) {
      val numParts = topicBody.split("Number:")
      val topicParts = topicBody.split("Topic:")

      if (numParts.length > 1 && topicParts.length > 1) {
        // get topic number and query
        val num = numParts(1).split("\n")(0).trim().substring(1)
        val title = topicParts(1).split("\n")(0).trim()
        topicNumToTitle(num) = title

        // collect query terms (individual words in each query) for general access later on
        val terms = title.split(" ")
        for (term <- terms) {
          queryTerms += term.replaceAll("[^A-Za-z0-9]", "")
        }
      }
    }

    // TODO: replace query terms non-alphanumeric terms

    // Initialize min heaps for each query
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

    /************* TESTING - QRELS  *****************/
    /*
    qrels(topicNumToTitle)
    return
    */

    /************* TESTING 1) *****************/
    /*
    println("topic num to title map:")
    for ((num, topic) <- topicNumToTitle) {
      println(num)
      println(topic)
    }

    println("num query terms: " + queryTerms.size)
    println("all query terms:")
    for (term <- queryTerms) {
      println(term)
    }
    return
    */

    // 2) PROCESS DOCUMENT STREAM

    /* FIRST PASS
     * 1) Get total number of words in general
     * 2) Get query term frequency for all documents in the collection
     */
    var numWordsCollection = 0

    // query terms freq for WHOLE document collection - LANG MODEL
    val queryTermsFreqTotal = mutable.LinkedHashMap[String, Int]()

    // number of documents that have each query term for WHOLE document - TERM MODEL
    val queryTermsNumDocuments = mutable.LinkedHashMap[String, mutable.LinkedHashMap[String, Int]]()

    val sw = new StopWatch
    sw.start

    var tipster = new TipsterCorpusIterator("Tipster/zips")
    var numDocuments = 0
    println("has next: " + tipster.hasNext)
    while (tipster.hasNext) {
      val doc = tipster.next()
      numDocuments += 1
      // total number of words in collection
      numWordsCollection += doc.tokens.length

      // query term frequency
      // frequency of all tokens in document
      val tfs : Map[String,Int]= doc.tokens.groupBy(identity).mapValues(l => l.length)
      // log frequency of all tokens in document
      // TODO: consider other transformations like square root
      //val sum = tfs.values.sum.toDouble
      //val logtf : Map[String,Double] = tfs.mapValues( v => log2( (v.toDouble+1.0) / sum) )

      // filter map of all tokens to only those in query
      val qtfs = tfs.filterKeys(queryTerms)

      // add to query term frequency for whole doc collection
      for ((queryTerm, freq) <- qtfs) {
        val count = queryTermsFreqTotal.getOrElse(queryTerm, 0)
        queryTermsFreqTotal(queryTerm) = count + freq
      }

      // add to num doc count of each term in query
      for ((_, topic) <- topicNumToTitle) {
        val qterms = topic.split(" ")
        val map = queryTermsNumDocuments.getOrElse(topic, mutable.LinkedHashMap[String, Int]())
        for (term <- qterms) {
          val termNew = term.replaceAll("[^A-Za-z0-9]", "")
          if (qtfs.getOrElse(termNew, 0) > 0) {
            val curr = map.getOrElse(termNew, 0)
            map(termNew) = curr + 1
          }
        }
        queryTermsNumDocuments(topic) = map
      }
    }

    sw.stop
    println("num documents: " + numDocuments)
    println("Stopped time = " + sw.stopped)

    /************* TESTING FIRST PASS *****************/
    /*
    println("total num words:" + numWordsCollection)
    for ((term, freq) <- queryTermsFreqTotal) {
      println(term + ": " + freq)
    }
    */
  /*
    for ((query, map) <- queryTermsNumDocuments) {
      println("query: " + query)
      for ((term, numDoc) <- map) {
        println("term: " + term + " " + numDoc)
      }
      println("")
    }
    return*/




    /* SECOND PASS
     * 1) Find word count of document
     * 2) Get frequency of each query term in the document for all queries
     * 3) Call models
     */
    val sw2 = new StopWatch
    sw2.start

    /************* TESTING SECOND PASS *****************/
    // check if it matches total counts
    //val testQueryTerms = mutable.LinkedHashMap[String, Int]()

    tipster = new TipsterCorpusIterator("Tipster/zips")
    println("has next: " + tipster.hasNext)
    while (tipster.hasNext) {
      val doc = tipster.next()
      // get word count of document
      val numWordsDoc = doc.tokens.length

      // get frequency of each term for query terms
      // for each query, get a word in the query -> frequency
      val queryTermsToFreq = mutable.LinkedHashMap[String, MutMap[String, Int]]()

      // frequency of all tokens in document
      val tfs : Map[String,Int]= doc.tokens.groupBy(identity).mapValues(l => l.length)

      // filter map of all tokens to only those in query
      for ((_, topic) <- topicNumToTitle) {
        val qterms = topic.split(" ")
        val qtermsModify = for (qterm <- qterms) yield qterm.replaceAll("[^A-Za-z0-9]", "")
        val qtfs = MutMap[String, Int]()
        qtermsModify.foreach { q => qtfs(q) = 0 }
        tfs.filter(qtermsModify.toSet).foreach{case (k,v) => qtfs(k) = v}
        queryTermsToFreq(topic) = qtfs
      }

      val docId = doc.name
      // PASS TO MODEL
      LanguageModel.smoothing(queryTermsToFreq, docId, queryTermsFreqTotal, numWordsDoc, numWordsCollection)
      // pass queryTermsToFreq, queryTermsNumDocuments, docId, (numDocuments)

      /************* TESTING SECOND PASS *****************/
      /*
      println("num words in doc: " + numWordsDoc)
      println("doc id: " + docId)
      */
      /*
      for ((_, map) <- queryTermsToFreq) {
        for ((term, freq) <- map) {
          val count = testQueryTerms.getOrElse(term, 0)
          testQueryTerms(term) = count + freq
        }
      }
      */
    }

    sw2.stop
    println("Stopped time = " + sw2.stopped)

    /************* TESTING SECOND PASS *****************/
    /*
    for ((term, freq) <- testQueryTerms) {
      bw.write(term + ": " + freq + "\n")
    }
    */

    /************* TESTING - WRITE OUTPUT TO FILE  *****************/
    //bw.close()
    //return

    // TODO: output results to a file - esp when given final 10 test queries
    /*
    var heapCount = 1
    for (minHeap <- minHeapsLang; (_, topics) <- topicsNumToTitle) {
       val heapFile = new File("heaps-" + heapCount.toString() + ".txt")
       val hbw = new BufferedWriter(new FileWriter(heapFile))

       hbw.write("query: " + topics + "\n")
       for (doc <- minHeap.takeRight(100) {
        hbw.write(doc._2 + "\n")
       }
       heapCount += 1
    }
     */

    /************* TESTING - LANG HEAP  *****************/
    val heapFile = new File("heaps.txt")
    val hbw = new BufferedWriter(new FileWriter(heapFile))
    var heapNum = 0
    for ((_, topics) <- topicNumToTitle) {
      val minHeap = minHeapsLang(heapNum)
      hbw.write("query: " + topics + "\n")
      for (doc <- minHeap.takeRight(100)) {
        hbw.write(doc._2 + "\n")
      }
      hbw.write("\n")
      heapNum += 1
    }

    println("printed heaps")
    Evaluation.evaluate(minHeapsLang)

  }

  def log2 (x: Double) = scala.math.log10(x) / scala.math.log10(2.0)
}
