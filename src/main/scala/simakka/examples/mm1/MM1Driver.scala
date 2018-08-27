package simakka.examples.mm1

import akka.actor.Props
import simakka.core.{SimEntity, SimEvent}
import simakka.distributions.SimRndExp

object MM1Driver {
  def props(name: String, params: Option[String] = None) =
    Props(classOf[MM1Driver], name, params)
}

class MM1Driver(override val name: String,
                params: Option[String] = None)
  extends SimEntity(name) {

  val rnd = SimRndExp(name + "-exp", 10)

  var t = 0.0;
  val mm1id = getRefId("MM1").get

  def startEvents(): Unit = {
    1.to(10).foreach(i => {
      t = t + rnd.sample()
      scheduleID(t, 444, mm1id, Some("suhel data"))
    })
  }

  override def receive: Receive = {
    case se: SimEvent =>
      log.debug("entity {} received {}", name, se)
      handleMessage(se)
      if (outEvents.nonEmpty) {
        fel ! outEvents.toList
        outEvents.clear()
      }
      done()

    case _ =>
      simTrace("Start Events")
      startEvents()
  }
}
