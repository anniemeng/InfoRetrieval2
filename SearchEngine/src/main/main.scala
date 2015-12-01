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

object main {

  // binary relevance judgement
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

  def main(args: Array[String]) {

    // TODO: let user specify how many n best queries they want

    /************* TESTING - WRITE OUTPUT TO FILE  *****************/
    //val file = new File("output.txt")
    //val bw = new BufferedWriter(new FileWriter(file))

    // 1) GATHERING QUERIES
    val topicStream = DocStream.getStream("Tipster/topics_small_57")
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
    var tipster = new TipsterCorpusIterator("Tipster/zips")
    println("Number of files in zips = " + tipster.length)

    val sw = new StopWatch
    sw.start

    /* FIRST PASS
     * 1) Get total number of words in general
     * 2) Get query term frequency for all documents in the collection
     */
    var numWordsCollection = 0
    val queryTermsFreqTotal = mutable.LinkedHashMap[String, Int]() // query terms freq for WHOLE document collection
    while(tipster.hasNext){
      val doc = tipster.next()
      // total number of words in collection
      numWordsCollection += doc.tokens.length

      // query term frequency
      // frequency of all tokens in document
      val tfs : Map[String,Int]= doc.tokens.groupBy(identity).mapValues(l => l.length)

      // filter map of all tokens to only those in query
      val qtfs = tfs.filterKeys(queryTerms)
      for ((queryTerm, freq) <- qtfs) {
        val count = queryTermsFreqTotal.getOrElse(queryTerm, 0)
        queryTermsFreqTotal(queryTerm) = count + freq
      }
    }

    sw.stop
    println("Stopped time = " + sw.stopped)
    /************* TESTING FIRST PASS *****************/
    /*
    println("total num words:" + numWordsCollection)
    for ((term, freq) <- queryTermsFreqTotal) {
      println(term + ": " + freq)
    }
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
    //val testQueryTerms = mutable.LinkedHashMap[String, Int]()

    tipster = new TipsterCorpusIterator("Tipster/zips")
    while (tipster.hasNext) {
      val doc = tipster.next()
      // get word count of document
      val numWordsDoc = doc.tokens.length

      // get frequency of each term for query terms
      // for each query, get a word in the query -> frequency
      val queryTermsToFreq = mutable.LinkedHashMap[String, Map[String, Int]]()

      // frequency of all tokens in document
      val tfs : Map[String,Int]= doc.tokens.groupBy(identity).mapValues(l => l.length)

      // filter map of all tokens to only those in query
      for ((_, topic) <- topicNumToTitle) {
        val qterms = topic.split(" ")
        val qtfs = tfs.filterKeys(qterms.toSet)
        queryTermsToFreq(topic) = qtfs
      }

      val docId = doc.name
      // PASS TO MODEL
      // languageModel(queryTermsToFreq, docId, queryTermsFreqTotal, numWordsDoc, numWordsCollection)

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


    // TODO: relevance - P, R, F1, MAP
//    val rel = new TipsterGroundTruth("Tipster/qrels").judgements.get("51").get.toSet
//    val retTerm = alertsTermQueries(0).results.map(r => r.title)
//    val retLang = alertsLangQueries(0).results.map(r => r.title)
//    val prTerm = new PrecisionRecall(retTerm, rel)
//    println("Term:")
//    println(prTerm.relevIdx.mkString(" "))
//    println(prTerm.precs.mkString(" "))
//    println(prTerm.iprecs.mkString(" "))
//
//    val prLang = new PrecisionRecall(retLang, rel)
//    println("Lang:")
//    println(prLang.relevIdx.mkString(" "))
//    println(prLang.precs.mkString(" "))
//    println(prLang.iprecs.mkString(" "))

  }
}
