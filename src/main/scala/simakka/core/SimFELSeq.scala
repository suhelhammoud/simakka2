package simakka.core

import akka.actor.{Actor, ActorLogging, Props}
import simakka.core.SimFEL.{LookAhead, SafeTime}
import simakka.core.SimFELSeq.Done

object SimFELSeq {
  def props() = Props(classOf[SimFELSeq])

  /*Indicate Entity is:
   * 1 - Done processing event(s)
   * 2 - No more events expected from this
   * 3 - Ready to receive next event*/
  case class Done(id: Long)

}

class SimFELSeq extends Actor
  with SimEntityLookup with ActorLogging with SimTrace {

  var simTime: Double = 0.0
  val name = SimNames.fel.name()
  val id: Long = getRefId(name).get

  val felQueue = PriorityEventQueue.newInstance()

  var lastEntity: Long = -1;

  log.info("FEL_SEQ name = {}, id = {}", name, id)
  def tick(): Unit = {
    if (felQueue.size == 0) return
    val eventToFire = felQueue.dequeue()
    simTime = eventToFire.time

    simTrace("event to fire = {}", eventToFire)
    getRef(eventToFire.dest).get ! eventToFire
    lastEntity = eventToFire.dest

  }

  override def receive: Receive = {
    case lst: List[SimEvent] =>
      simTrace("event list = {}", lst)
      felQueue ++= lst

    case m: SimEvent =>
      simTrace("one event ={}", m)
      felQueue += m

    case Done(id) =>
      simTrace("Done({}), last {} ", id, lastEntity)
      tick()


    case LookAhead(lhId, lhTime) =>
      log.debug("LookAhead( {}, {})", lhId, lhTime)
      simTrace("LookAhead( {}, {})", lhId, lhTime)


    case SafeTime(stId, stTime) =>
      log.debug("SafeTime( {}, {})", stId, stTime)
      simTrace("SafeTime( {}, {})", stId, stTime)
      tick()

    case _ => log.error("unknown message")
  }
}
