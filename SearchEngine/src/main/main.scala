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
  var stopWords = Set("in", "from", "to", "the", "for", "of", "by", "on", "what", "does")

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
        val num = numParts(1).split("\n")(0).trim().toInt.toString // strip the 0
        val title = topicParts(1).split("\n\n")(0).trim()
        val newTitle = title.replaceAll("[^A-Za-z0-9 \n.'\\/-]", "").replaceAll("[\\/\n-]", " ").replaceAll("  ", " ").replaceAll("  ", " ").replaceAll("    ", " ").toLowerCase

        // TODO: STOP WORDS
        // val filtered = newTitle.split(" ").filterNot(stopWords).mkString(" ")
        // topicNumToTitle(num) = filtered

        // TODO: comment out if using STOP WORDS
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

      // filter map of all tokens to only those in query
      val qtfs = tfs.filterKeys(queryTerms)

      // testing filtering
      // println("QUERY TERMS: num query terms in doc: " + qtfs.size)
      // println(qtfs.keys)

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

    //println("num query terms: " + queryTerms.size)
    //println("DOC TESTING size query terms num docs: " + queryTermsNumDocuments.size)

    sw.stop
    println("num documents: " + numDocuments)
    println("Stopped time = " + sw.stopped)

    /************* TESTING FIRST PASS *****************/
    //println("total num words:" + numWordsCollection)
    /*
    println("num query terms: " + queryTerms.size)
    println("num query terms in map: " + queryTermsFreqTotal.size)
    for ((term, freq) <- queryTermsFreqTotal) {
      println(term + ": " + freq)
    }
    return
    */
    /*
    var numQueryTerms = 0
    for ((query, map) <- queryTermsNumDocuments) {
      numQueryTerms += map.size
      for ((term, numDoc) <- map) {
        println(term + ": " + numDoc)
      }
    }
    println("total num query terms in num docs: " + numQueryTerms)
    return
    */


    /* SECOND PASS
     * 1) Find word count of document
     * 2) Get frequency of each query term in the document for all queries
     * 3) Call models
     */
    val sw2 = new StopWatch
    sw2.start

    /************* TESTING SECOND PASS *****************/
    // check if it matches total counts
    // val testQueryTerms = mutable.LinkedHashMap[String, Int]()

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
      println(term + ": " + freq + "\n")
    }
    */

    // TESTING HEAP
    /*
    println("lang heap first: ")
    println("last item in heap: " + minHeapsLang.head.last)
    val langOrder = minHeapsLang.head.takeRight(100)
    for (doc <- langOrder) {
      println(doc._2 + " score: " + doc._1)
    }
    println("")
    println("WITH REVERSE")
    val langOrderRev = langOrder.reverse
    for (doc <- langOrderRev) {
      println(doc._2 + " score: " + doc._1)
    }

    println("term heap first: ")
    println("last item in heap: " + minHeapsTerm.head.last)
    val termOrder = minHeapsTerm.head.takeRight(100)
    for (doc <- termOrder) {
      println(doc._2 + " score: " + doc._1)
    }
    */

    // TODO: output results to a file - esp when given final 10 test queries
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
      if (heapNumTerm == 0) {
        println("TERM:")
      }

      val minHeapTerm = minHeapsTerm(heapNumTerm)
      var rankT = 1
      val reverseHeapTerm = minHeapTerm.takeRight(100).reverse
      while (reverseHeapTerm.nonEmpty) {
        val doc = reverseHeapTerm.dequeue()
        if (heapNumTerm == 0) {
          println(doc._2 + "score: " + doc._1)
        }
        htbw.write(num + " " + rankT.toString + " " + doc._2 + "\n")
        rankT += 1
      }

      if (heapNumTerm == 0) {
        println("LANG:")
      }

       // lang
       val minHeapLang = minHeapsLang(heapNumLang)
       val reverseHeapLang = minHeapTerm.takeRight(100).reverse
       var rankL = 1
       while(reverseHeapLang.nonEmpty) {
         val doc = reverseHeapLang.dequeue()

         if (heapNumTerm == 0) {
           println(doc._2 + "score: " + doc._1)
         }
         hlbw.write(num + " " + rankL.toString + " " + doc._2 + "\n")
         rankL += 1
       }

       heapNumTerm += 1
       heapNumLang += 1
    }
    htbw.close()
    hlbw.close()

    /************* TESTING - LANG HEAP  *****************/
    /*
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
    */
    /************* TESTING - TERM HEAP  *****************/
    /*
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
    */
    println("printed heaps")

    Evaluation.evaluate(minHeapsLang, "lang")
    Evaluation.evaluate(minHeapsTerm, "term")
  }

  def log2 (x: Double) = scala.math.log10(x) / scala.math.log10(2.0)
}
