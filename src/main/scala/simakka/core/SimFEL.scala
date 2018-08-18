package simakka.core

import akka.actor.{Actor, ActorLogging, Props}
import simakka.core.SimFEL.{ SafeTime}
import simakka.core.SimFEL.LookAhead
import simakka.core.SimFELSeq.Done


object SimFEL {
  def props() = Props(classOf[SimFEL])

  val isSequential = true

  /*Indicate Entity is:
   * 1 - Done processing event(s)
   * 2 - No more events expected from this
   * 3 - Ready to receive next event*/
//  case class Done(id: Long)

  /* Transparent message should work without it  */
  case class LookAhead(id: Long, time: Double)

  case class SafeTime(id: Long, time: Double)

}

class SimFEL extends Actor
  with SimEntityLookup with ActorLogging with SimTrace {

  var simTime: Double = 0.0
  val name = SimNames.fel.name()
  val id: Long = getRefId(name).get

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
    //update fel event
    if (eventsToFire.size > 0)
      simTime = eventsToFire.head.time
  }

  def tick(): Unit = {
    if (canFireEvents())
      extractAndFireEvents()
  }

  def tickSeq(): Unit ={
    if(felQueue.size == 0) return
    //extract and fire events
    val headEventTime = felQueue.head.time
    felQueue.takeWhile( ev => ev.time == headEventTime)
  }
  override def receive: Receive = {
    case lst: List[SimEvent] =>
      simTrace("event list = {}", lst)
      felQueue ++= lst

    case m: SimEvent =>
      simTrace("one event ={}", m)
      felQueue += m

    case Done(id) =>
      simTrace("done for id = {}", id)
      currents.remove(id)
      tick()


    case LookAhead(lhId, lhTime) =>
      log.debug("LookAhead( {}, {})", lhId, lhTime)
      simTrace("LookAhead( {}, {})", lhId, lhTime)

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
