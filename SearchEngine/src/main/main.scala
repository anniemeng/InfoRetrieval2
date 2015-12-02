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
      val docId = values(2).filter(_.isLetterOrDigit)
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
    /*
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
    */

    // testing for term freq
    val heapTermFile = new File("qrelHeapTerm.txt")
    val htbw = new BufferedWriter(new FileWriter(heapTermFile))
    var heapNumTerm = 0

    var sumAPs = 0
    // iterate over all queries
    for ((topics, _) <- topicNumToTitle) {
      val minHeap = minHeapsTerm(heapNumTerm)
      htbw.write("query: " + topics + "\n")

      // get qrels documents for each query
      val relevant = topicToRelevant.get(topics) // qrels - docs w/ 1
      val irrelevant = topicToIrrelevant.get(topics) // qrels - docs w/ 0

      var relevantInHeap = 0 // TP
      var irrelevantInHeap = 0 // FP

      var numeratorAP = 0

      for (doc <- minHeap.takeRight(100)) {
        var pRel = 0
        if (relevant.contains(doc._2)) {
          relevantInHeap += 1
          pRel = 1
        } else if (irrelevant.contains(doc._2)) {
          irrelevantInHeap += 1
        }

        // AP/MAP calculation
        val TPFP = relevantInHeap + irrelevantInHeap

        var currPrecision = 0
        if (TPFP != 0) {
          currPrecision = relevantInHeap / TPFP
        }
        numeratorAP += pRel * currPrecision
      }

      // FN
      val FN = relevant.size - relevantInHeap

      val TPFP = relevantInHeap + irrelevantInHeap
      var precision = 0
      if (TPFP != 0) {
        precision = relevantInHeap / TPFP
      }

      val recall = relevantInHeap / (relevantInHeap + FN)

      var precisionFrac = 0
      var recallFrac = 0
      if (precision != 0) {
        precisionFrac = 1 / precision
      }
      if (recall != 0) {
        recallFrac = 1 / recall
      }

      var f1 = 0
      if (precisionFrac != 0 && recallFrac != 0) {
        f1 = 2 / (precisionFrac + recallFrac)
      }

      var AP = 0
      if (relevantInHeap + FN != 0) {
        AP = numeratorAP / (relevantInHeap + FN)
      }
      sumAPs += AP

      htbw.write("num relevant in heap: " + relevantInHeap + "\n")
      htbw.write("num irrelevant in heap: " + irrelevantInHeap + "\n")
      htbw.write("num in heap in the qrels: " + relevantInHeap + irrelevantInHeap + "\n")
      htbw.write("precision: " + precision)
      htbw.write("recall: " + recall)
      htbw.write("f1: " + f1)
      htbw.write("\n")
      heapNumTerm += 1
    }

    var MAP = sumAPs / topicNumToTitle.size
    htbw.write("MAP: " + MAP + "\n")
    htbw.close()
  }*/

  def main(args: Array[String]) {

    // TODO: let user specify how many n best queries they want

    /******************************
     * 1) GATHERING QUERIES
     ********************************/
    val topicStream = DocStream.getStream("Tipster/topics")
    val topics = scala.io.Source.fromInputStream(topicStream).mkString.split("</top>")

    // topic number to query
    val topicNumToTitle = mutable.LinkedHashMap[String, String]() // preserve order

    // all terms of all queries
    var queryTerms = Set[String]()
    for (topicBody <- topics) {
      val numParts = topicBody.split("Number:")
      val topicParts = topicBody.split("Topic:")

      if (numParts.length > 1 && topicParts.length > 1) {
        // get topic number and query
        val num = numParts(1).split("\n")(0).trim().substring(1) // strip the 0
        val title = topicParts(1).split("\n\n")(0).trim()
        val newTitle = title.replaceAll("[^A-Za-z0-9 \n.'\\/-]", "").replaceAll("[\\/\n-]", " ").replaceAll("  ", " ").replaceAll("  ", " ").replaceAll("    ", " ")
        topicNumToTitle(num) = newTitle

        // collect query terms (individual words in each query) for general access later on
        val terms = newTitle.split(" ")
        for (term <- terms) {
          queryTerms += term
        }
      }
    }

    // TESTING STRIPPING THE QUERY TERM
    // return

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

    /******************************
     * 2) PROCESS DOCUMENT STREAM
     ******************************/
    /* FIRST PASS
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
    val queryTermsFreqTotal = mutable.LinkedHashMap[String, Int]()
    queryTerms.foreach { t => queryTermsFreqTotal(t) = 0 }

    // number of documents that have each query term for WHOLE document - TERM MODEL
    val queryTermsNumDocuments = mutable.LinkedHashMap[String, mutable.LinkedHashMap[String, Int]]()

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

      // TODO: consider other transformations like square root
      /*
      // log frequency of all tokens in document
      val sum = tfs.values.sum.toDouble
      val logtf : Map[String,Double] = tfs.mapValues( v => log2( (v.toDouble+1.0) / sum) )
      */

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
          // set num doc to 0 if query term not in
          val curr = map.getOrElse(term, 0)
          val additional = if (qtfs.getOrElse(term, 0) > 0) 1 else 0
          map(term) = curr + additional
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
    while (tipster.hasNext) {
      val doc = tipster.next()

      // get word count of document
      val numWordsDoc = doc.tokens.length
      val docId = doc.name

      // get frequency of each term for query terms
      // for each query, get a word in the query -> frequency
      val queryTermsToFreq = mutable.LinkedHashMap[String, MutMap[String, Int]]()

      // frequency of all tokens in document
      val tfs : Map[String,Int]= doc.tokens.groupBy(identity).mapValues(l => l.length)

      // filter map of all tokens to only those in query
      for ((_, topic) <- topicNumToTitle) {
        val qterms = topic.split(" ")
        val qtfs = MutMap[String, Int]()
        qterms.foreach { q => qtfs(q) = 0 }
        tfs.filterKeys(qterms.toSet).foreach{case (k,v) => qtfs(k) = v}
        queryTermsToFreq(topic) = qtfs
      }

      // PASS TO MODEL
      LanguageModel.smoothing(queryTermsToFreq, docId, queryTermsFreqTotal, numWordsDoc, numWordsCollection)
      //TFScore.score(queryTermsToFreq, docId)
      TFIDFScore.score(queryTermsToFreq, queryTermsNumDocuments, numDocuments, docId)

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

    // TODO: output results to a file - esp when given final 10 test queries
    /*
    var heapNumTerm = 0
    var heapNumLang = 0
    val heapTermFile = new File("ranking-t-12")
    val heapLangFile = new File("ranking-l-12.run")
    val htbw = new BufferedWriter(new FileWriter(heapTermFile))
    val hlbw = new BufferedWriter(new FileWriter(heapLangFile))
    for ((num, topics) <- topicsNumToTitle) {
       val minHeapTerm = minHeapsTerm(heapNumTerm)
       val minHeapLang = minHeapsLang(heapNumLang)

       var rankT = 1
       for (doc <- minHeapTerm.takeRight(100) {
        htbw.write(num + " " + rank.toString + " " + doc._2 + "\n")
        rankT += 1
       }

       var rankL = 1
       for (doc <- minHeapLang.takeRight(100) {
        hlbw.write(num + " " + rank.toString + " " + doc._2 + "\n")
        rankL += 1
       }

       heapNumTerm += 1
       heapNumLang += 1
    }
    htbw.close()
    hlbw.close()

     */

    /************* TESTING - LANG HEAP  *****************/
    val heapLangFile = new File("heapsLang.txt")
    val hlbw = new BufferedWriter(new FileWriter(heapLangFile))
    var heapNumLang = 0
    for ((_, topics) <- topicNumToTitle) {
      val minHeap = minHeapsLang(heapNumLang)
      hlbw.write("query: " + topics + "\n")
      for (doc <- minHeap.takeRight(100)) {
        hlbw.write(doc._2 + "\n")
      }
      hlbw.write("\n")
      heapNumLang += 1
    }
    hlbw.close()

    /************* TESTING - TERM HEAP  *****************/

    val heapTermFile = new File("heapsTerm.txt")
    val htbw = new BufferedWriter(new FileWriter(heapTermFile))
    var heapNumTerm = 0
    for ((_, topics) <- topicNumToTitle) {
      val minHeap = minHeapsTerm(heapNumTerm)
      htbw.write("query: " + topics + "\n")
      for (doc <- minHeap.takeRight(100)) {
        htbw.write(doc._2 + "\n")
      }
      htbw.write("\n")
      heapNumTerm += 1
    }
    htbw.close()

    println("printed heaps")

    // qrels(topicNumToTitle)

    Evaluation.evaluate(minHeapsLang)
    Evaluation.evaluate(minHeapsTerm)

  }

  def log2 (x: Double) = scala.math.log10(x) / scala.math.log10(2.0)
}
