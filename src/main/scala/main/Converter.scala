package main

import java.io.StringWriter

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import com.fasterxml.jackson.module.scala.{DefaultScalaModule, JacksonModule}

import scala.collection.immutable.HashMap
import scala.reflect.io.File

import com.fasterxml.jackson.module.scala._

import com.fasterxml.jackson._

/**
 * Created by yada on 5/20/15.
 */
object Converter extends App {

  case class Item(
                 base : String,
                 items : List[Entry]
                   )

  case class Entry(
                  name : String,
                  value : String
                    )

  override def main (args: Array[String]) {

    val lexiconContent = scala.io.Source.fromFile("./LEXICON2").mkString

    val asJson = toJSONString(lexiconContent)

    scala.reflect.io.File("./lexicon2.json").writeAll(asJson)

    val mapper = new ObjectMapper() with ScalaObjectMapper
    mapper.registerModule(DefaultScalaModule)
    val myMap : List[Item] = mapper.readValue[List[Item]](asJson)

    //println(myMap)

    val csvSynonymsString = myMap.map( p=> (p.base, p.items.filter(kk => kk.name.contains("acro") || kk.name.contains("spelling") || kk.name.contains("abbreviation_of") ).map(o => "([^|]*)".r.findFirstMatchIn(o.value).map(_.group(0)).getOrElse("")).mkString(",") ) ).filter( _._2.length>0).map( cc => cc._1 + "," + cc._2)
      .mkString("\n")

    scala.reflect.io.File("./lexicon2.csv").writeAll(csvSynonymsString)


  }

  def toJSONString(rawFileContent : String) : String = {
    val array = rawFileContent.replaceAll("}", "},")
      .replaceAll("\"", """\\"""")
      .replaceAll("\\]", "")
      .replaceAll("\\[", "")
      //.replaceAll("\\{(?!base)|\\}(?!\\n)","")
      .replaceAll("\\{base=(.*)(\\n[^\\}]*)*}", "\\{\\\"base\":\"$1\",\"items\":[$2\n]\\}")
      .replaceAll("\\{base=(.*)\n", "\\{\\\"base\":\"$1\",\n")
      .replaceAll("(\\w+)=(.*)\n", "\\{ \"name\":\"$1\",\"value\":\"$2\" \\},\n")
      .replaceAll("\t", "")
      .replaceAll("([\\w_]*)=.*[\\{\\]].*\n", "\n$1=vfvdfvdfvdf\n")
      .replaceAll("proper\n", "")
      .replaceAll("trademark\n", "")
      .replaceAll("intran;.*\n", "")
      .replaceAll("intran\n", "")
      .replaceAll("broad_negative\n", "")
      .replaceAll("interrogative\n", "")
      .replaceAll("stative\n", "")
      .replaceAll("demonstrative\n", "")
      .replaceAll("negative\n", "")

      .replaceAll(",\n\\}", "\n\\}")
      .replaceAll(",\n\\]", "\n\\]")
      .replaceAll(",\n\n\\]", "\n\\]")


    ("[" + array + "]").replace("},\n]", "}]")
  }

}
