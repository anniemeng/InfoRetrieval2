package main

import Parsing._
import LanguageBasedModel._
import TermBasedModel._

object main {
  def main(args: Array[String]) {
    val numResults = 100

    // 1) GATHERING QUERIES
    // For testing - comment out
    val query = "Airbus Subsidies"
    val queries = new Array[String](1)
    queries(0) = query

    // For actual command line - uncomment
    // val queries = args

    // 2) STORE A MODEL FOR EACH QUERY
    // might want to change to dictionary mapping query -> model
    val alertsTermQueries = new Array[TermBasedModel.AlertsTipster](queries.length)
    val alertsLangQueries = new Array[LanguageBasedModel.AlertsTipster](queries.length)
    var index = 0
    for (query <- queries) {
      val alertsTerm = new TermBasedModel.AlertsTipster(query, numResults)
      val alertsLanguage = new LanguageBasedModel.AlertsTipster(query, numResults)
      alertsTermQueries(index) = alertsTerm
      alertsLangQueries(index) = alertsLanguage
      index += 1
    }

    // 3) PROCESS DOCUMENT STREAM
    val tipster = new TipsterStream("Tipster/zips")
    println("Number of files in zips = " + tipster.length)

    val sw = new StopWatch
    sw.start
    for (doc <- tipster.stream) {
      // process document for term based model
      for (alertsTerm <- alertsTermQueries) {
        alertsTerm.process(doc.name, doc.tokens)
      }

      // process document for lang based model
      for (alertsLanguage <- alertsLangQueries) {
        alertsLanguage.process(doc.name, doc.tokens)
      }
    }

    sw.stop
    println("Stopped time = " + sw.stopped)
//    println("Term:")
//    alertsTerm.results.take(5).foreach(println)
//    println("Lang:")
//    alertsLanguage.results.take(5).foreach(println)

    // 4) PRINTING RELEVANCE FOR EACH QUERY
    // For command line purposes:
    var topic = 51
    var indexAlert = 0
    while(indexAlert < queries.length) {
      val rel = new TipsterGroundTruth("Tipster/qrels").judgements.get(topic.toString()).get.toSet
      val retTerm = alertsTermQueries(indexAlert).results.map(r => r.title)
      val prTerm = new PrecisionRecall(retTerm, rel)
      println(alertsTermQueries(indexAlert).query) // print query
      println("Term:")
      println(prTerm.relevIdx.mkString(" "))
      println(prTerm.precs.mkString(" "))
      println(prTerm.iprecs.mkString(" "))

      val retLang = alertsLangQueries(indexAlert).results.map(r => r.title)
      val prLang = new PrecisionRecall(retLang, rel)
      println("Lang:")
      println(prLang.relevIdx.mkString(" "))
      println(prLang.precs.mkString(" "))
      println(prLang.iprecs.mkString(" "))

      topic += 1
      indexAlert += 1
    }

    // testing purposes - COMMENT OUT
    val rel = new TipsterGroundTruth("Tipster/qrels").judgements.get("51").get.toSet
    val retTerm = alertsTermQueries(0).results.map(r => r.title)
    val retLang = alertsLangQueries(0).results.map(r => r.title)
    val prTerm = new PrecisionRecall(retTerm, rel)
    println("Term:")
    println(prTerm.relevIdx.mkString(" "))
    println(prTerm.precs.mkString(" "))
    println(prTerm.iprecs.mkString(" "))

    val prLang = new PrecisionRecall(retLang, rel)
    println("Lang:")
    println(prLang.relevIdx.mkString(" "))
    println(prLang.precs.mkString(" "))
    println(prLang.iprecs.mkString(" "))
}
}
