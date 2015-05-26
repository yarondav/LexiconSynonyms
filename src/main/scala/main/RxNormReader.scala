package main

import scala.util.matching.Regex

/**
 * Created by yada on 5/26/15.
 */


case class Relation(
                     RXCUI1 :  String,
                     RXAUI1 :  String,
                     STYPE1 :  String,
                     REL :  String,
                     RXCUI2 :  String,
                     RXAUI2 :  String,
                     STYPE2 :  String,
                     RELA :  String,
                     RUI :  String,
                     SRUI :  String,
                     SAB :  String,
                     SL :  String,
                     DIR :  String,
                     RG :  String,
                     SUPPRESS :  String,
                     CVF : String
                     )

object Relation {
  def fromLine(line : String) : Relation = {
    val params = line.split('|')

    Relation(params(0), params(1), params(2), params(3), params(4), params(5), params(6), params(7), params(8), params(9), params(10), params(11), params(12), params(13), params(14), params(15))

  }

}


case class Concept(
                    RXCUI : String,
                    LAT : String,
                    TS : String,
                    LUI : String,
                    STT : String,
                    SUI : String,
                    ISPREF : String,
                    RXAUI : String,
                    SAUI : String,
                    SCUI : String,
                    SDUI : String,
                    SAB : String,
                    TTY : String,
                    CODE : String,
                    STR : String,
                    SRL : String,
                    SUPPRESS : String,
                    CVF : String
                    )

object Concept {
  def fromLine(line : String) : Concept = {
    val params = line.split('|')
    Concept(params(0), params(1), params(2), params(3), params(4), params(5), params(6), params(7), params(8), params(9), params(10), params(11), params(12), params(13), params(14), params(15), params(16), params(17))
  }

}

object RxNormReader extends App {


  override def main(args: Array[String]) = {


    val bb = "Allantoin 0.01 MG/MG"
    val mm = removeUnitsFromName(bb)


    val allRelations = scala.io.Source.fromFile("./rx_norm/RXNREL.RRF").getLines().map( line => {
      Relation.fromLine(line)
    }).toList

    val allConcepts = scala.io.Source.fromFile("./rx_norm/RXNCONSO.RRF").getLines().map( line => {
      Concept.fromLine(line)
    }).map( (concept: Concept) => (concept.RXCUI, concept)).toMap


    //allRelations.map(_.RELA).toList.distinct.foreach((s: String) => println(s))

    //allRelations.map(_.RXCUI1).toList.distinct.foreach((s: String) => println(s))


    allRelations.filter(_.RELA.contentEquals("tradename_of")).map( rel => {
      val a = (allConcepts.get(rel.RXCUI1).map(_.STR).getOrElse("<>") , rel.REL, allConcepts.get(rel.RXCUI2).map(_.STR).getOrElse("<>"))
      //println(a)
      a
    }).map((tuple: (String, String, String)) => (tuple._1, tuple._3))
      .groupBy(_._1)
      .map((tuple: (String, List[(String, String)])) => (tuple._1, tuple._2.map(_._2)) )
      .map( (tuple: (String, List[String])) => (tuple._1, tuple._2.flatMap( (s: String) => "\\[(\\w+)\\]".r.findAllMatchIn(s) ).map(_.group(1)).filterNot(isDrugNameIrrelevant(_)) ) )
      .filter( p=> p._2.length > 0)
      .map(p => (removeUnitsFromName(p._1), p._2.map(o => removeUnitsFromName(o) )))
      //.foreach(println(_))
      .foreach( a => println(s""""${a._1}",""" + a._2.map(b => s""""${b}"""").mkString(",")) )
  }

  def isDrugNameIrrelevant(name : String) : Boolean = {
    (name.length < 3)
  }


  def removeUnitsFromName(drugFormulation : String) : String = {
    val unitNames = List("%", "g", "gm", "ug", "ml", "mg", "meq", "iu", "hr", "unt", "mg/mg","mg/ml", "meq/ml", "unt/ml", "mcg/ml")

    val stopWords = List("injectable", "solution", "topical", "powder", "oral", "urethral" ,"tablet", "gel", "capsule", "Ophthalmic", "suspension", "rectal",
    "suppository", "patch", "prefilled", "syringe", "lotion", "vaginal", "kit", "ring", "nasal", "cream", "transdermal", "spray", "extended", "release").map( p=> " ?" + p + " ?").map(_.r)

    val floatRegex = " ?[-+]?\\b[0-9,]*\\.?[0-9]+\\b ?"

    val unitRegex = unitNames.map( p => ( floatRegex +" ?" + p).r)

    val unitInRegex = unitNames.map( p => (floatRegex + " ?" + p + " *(in|per) *" + floatRegex).r)

    val withoutUnitsIn = unitInRegex.foldRight(drugFormulation.toLowerCase()) { (currentRegex : Regex, strippedStr) => currentRegex.replaceAllIn(strippedStr, "") }
    val withoutUnits = unitRegex.foldRight(withoutUnitsIn) { (currentRegex : Regex, strippedStr) => currentRegex.replaceAllIn(strippedStr, "") }

    val withoutStopWords = stopWords.foldRight(withoutUnits) { (currentRegex : Regex, strippedStr) => currentRegex.replaceAllIn(strippedStr, "") }

    withoutStopWords.stripPrefix(" ").stripSuffix(" ")
  }
}
