package simakka.examples.mm1

import akka.actor.Props
import simakka.core.SimEntity
import simakka.core.SimPredicateGenerator.DEFAULT
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


  override def defineBehaviours(): Unit = {
    var numEvents = 0

    on(DEFAULT) {
      case m: Any =>
        numEvents += 1
        simTrace("Driver numEvents ={}, at time = {}", numEvents, simTime)
        val event = scheduleName(0, 3, "mm1")
        simTrace("{} : Driver schedule event {} ", simTime, event)
        pause(10)
    }
  }


}
