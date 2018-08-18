package simakka.core

import scala.collection.mutable._
import net.liftweb.json._
import net.liftweb.json.Serialization.write
import simakka.examples.mm1.MM1.MM1Config

object SimUtils {

  implicit val formats = DefaultFormats

  def toJsonString(d: Any): String = {
    implicit val formats = DefaultFormats
    val jsonString = write(d)
    return jsonString
  }

  def toJValue(data: String): JValue = {
    implicit val formats = DefaultFormats
    parse(data)
  }

  def toJValueFromPath(fileName: String): JValue =
    toJValue(io.Source.fromFile(fileName).mkString)

}

object MyApp extends App {

  implicit val formats = DefaultFormats

  case class Person(name: String, address: Address)

  case class Address(city: String, state: String)

  val p = Person("Alvin Alexander", Address("Talkeetna", "AK"))


//  println(SimUtils.toJsonString(p))
  val v: MM1Config = SimUtils.toJValueFromPath("data/config/examples/mm1/mm1.json")
      .extract[MM1Config]
  println(v)
}