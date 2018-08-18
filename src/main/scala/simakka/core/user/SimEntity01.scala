package simakka.core.user

import akka.actor.Props
import org.slf4j.LoggerFactory
import simakka.core.{SimEntity, SimEvent}
import net.liftweb.json._
import net.liftweb.json.JsonDSL._

object SimEntity01 {

  val autoEvents = true

  //  def props(name: String, id: Long) = Props(new SimEntity(name, id))
  def props(name: String, id: Long, params: Option[String] = None) = Props(classOf[SimEntity01], name, id, params)

  // if time < 0 done is true
  def main(args: Array[String]): Unit = {
    val v = parse(
      """
        |"name":"Adam", "age":50
      """.stripMargin)

  }

}

class SimEntity01(override val name: String, params: Option[String] = None)
  extends SimEntity(name, params) {
  log.info("Construct SimEntity01")

  override def handleMessage(msg: SimEvent): Seq[SimEvent] = {
    log.debug("handle message {}", msg)
    return null
  }


}
