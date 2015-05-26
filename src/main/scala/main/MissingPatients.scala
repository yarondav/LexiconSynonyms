package main

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper


/**
 * Created by yada on 5/25/15.
 */
object MissingPatients extends App {

  case class PatientFromAllPatients(
                                     _id : Option[String],
                                     patientId : String
                                     )

  override def main (args: Array[String]): Unit = {


    val allPatientsStr = scala.io.Source.fromFile("./all_patientIds_nmhi.json").mkString

    val mapper = new ObjectMapper() with ScalaObjectMapper
    mapper.registerModule(DefaultScalaModule)

    val allPatients = mapper.readValue[List[PatientFromAllPatients]](allPatientsStr).map(_.patientId)

    val dledPatientsStr = scala.io.Source.fromFile("./dled_patientsNMHI.json").mkString
    val dledPatients = mapper.readValue[List[PatientFromAllPatients]](dledPatientsStr).map(_.patientId)

    val missings = allPatients.filterNot( dledPatients.contains(_)  )

    println(missings.length)
    println(missings.mkString(", "))


  }
}
