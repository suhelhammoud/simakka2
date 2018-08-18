package simakka.core

import akka.actor.{Actor, ActorLogging, Props}
import simakka.core.SimFEL.{Done, SafeTime}
import simakka.core.SimFEL.LookAhead


object SimFEL {
  def props() = Props(classOf[SimFEL])

  /*Indicate Entity is:
   * 1 - Done processing event(s)
   * 2 - No more events expected from this
   * 3 - Ready to receive next event*/
  case class Done(id: Long)

  /* Transparent message should work without it  */
  case class LookAhead(id: Long, time: Double)

  case class SafeTime(id: Long, time: Double)

}

class SimFEL extends Actor
  with SimEntityLookup with ActorLogging {


  //  val id = refName(SimNames.fel.name(), self)

  val felQueue = PriorityEventQueue.newInstance()
  val lookAHeads = new collection.mutable.LongMap[Double]
  val currents = new collection.mutable.LongMap[Double]


  def canFireEvents(): Boolean = {
    if (felQueue.size == 0) return false
    if (currents.size == 0) return true
    //there are at least one futureEvent with lower time than current fired events
    return currents.forall(p => p._2 >= felQueue.head.time)
  }

  def extractAndFireEvents() = {
    val eventsToFire = felQueue.takeWhile(p => p.time < currents.values.min)
    assert(eventsToFire.size > 0)
    log.debug("events to fire size = {}", eventsToFire.size)

    felQueue.drop(eventsToFire.size)

    eventsToFire.foreach(f => {
      val addition = if (lookAHeads.contains(f.dest))
        lookAHeads.get(f.dest).get else 0.0
      currents.put(f.dest, f.time + addition)
    })

    //send out events
    eventsToFire.foreach(ev => getRef(ev.dest).get ! ev)
  }

  def tick(): Unit = {
    if (canFireEvents())
      extractAndFireEvents()
  }

  override def receive: Receive = {
    case lst: List[SimEvent] => felQueue ++= lst

    case m: SimEvent => felQueue += m

    case Done(id) =>
      log.debug("done for id = {}", id)
      currents.remove(id)
      tick()


    case LookAhead(lhId, lhTime) =>
      log.debug("LookAhead( {}, {})", lhId, lhTime)
      if (lhTime < 0)
        lookAHeads.remove(lhId)
      else
        lookAHeads.update(lhId, lhTime)
      tick()

    case SafeTime(stId, stTime) =>
      log.debug("SafeTime( {}, {})", stId, stTime)
      assert(currents.contains(stId))
      currents.update(stId, stTime)
      tick()

    case _ => log.error("unknown message")
  }
}
