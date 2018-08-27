package simakka.core

import akka.actor.{Actor, ActorLogging, Props}

import scala.collection.mutable

object SimFEL {
  def props() = Props(classOf[SimFEL])
}

class SimFEL extends Actor
  with SimEntityLookup with ActorLogging with SimTrace with NameMe {

  val name = SimNames.fel.name()
  val me: Long = getRefId(name).get

  /**
    * contains two types of elements
    * SimEvent:
    * SimPredicate[Timed]
    */
  val felQueue = PriorityEventQueue.newInstance()

  /**
    * lookahead values for entities
    */
  val lookAheads = new collection.mutable.LongMap[LookAhead]

  /**
    * predicates with conditions to be matched
    */
  val predicates = new mutable.LongMap[SimPredicate]

  /* contains current entities ids yet to acknowledge fired events */
  val currents = new collection.mutable.LongMap[Double]

  var simTime: Double = 0.0

  def canFireEvents(): Boolean = {
    if (felQueue.size == 0) return false

    if (currents.size == 0) return true
    //TODO do we need simTime here ?
    //there are at least one futureEvent with lower time than current fired events
    return currents.values.forall(_ >= felQueue.head.time)
  }

  def extractAndFireEvents() = {
    val nextSimTime = felQueue.head.time

    val eventsToFire = felQueue.takeWhile(p => p.time < currents.values.min)
    assert(eventsToFire.size > 0)
    log.debug("events to fire size = {}", eventsToFire.size)

    felQueue.drop(eventsToFire.size)

    eventsToFire.foreach {
      case se: SimEvent =>
        fireSimEvent(se)

      case sp: SimTimedPredicate =>
        fireSimPredicate(sp)

      case _ => throw new Exception("Not defined")
    }
    simTime = nextSimTime
  }

  def fire(v: Any, target: Long): Unit = {
    getRef(target).get ! v
  }

  def lookAheadBeforeFiring(target: Long, time: Double): Unit = {
    //check lookahead and setup current time
    val lh = lookAheads.get(target)
    if (lh.isEmpty)
      currents.put(target, time)
    else lh.get.getType() match {
      case LookAhead.ONCE =>
        currents.update(target, time + lh.get.step)
        lookAheads.remove(target)
      case LookAhead.REPEATED =>
        currents.update(target, time + lh.get.step)
      case _ => {}
    }
  }

  def fireSimPredicate(sp: SimTimedPredicate): Unit = {
    val entry = predicates.remove(sp.target)
    if (entry.isDefined) {
      lookAheadBeforeFiring(sp.target, sp.time)
      fire(sp, sp.target)
    }
  }

  def fireSimEvent(se: SimEvent): Unit = {
    //check for predicate
    val pr = predicates.get(se.dest)

    val checkLookAhead = if (pr.isEmpty)
      true
    else if (pr.get.isMatching(se)) {
      //no timeout is triggered yet
      predicates.remove(se.dest)
      //TODO what about TimedPredicates which also have an entry in felQueue ?
      true
    } else false //predicate exists but does not match

    if (checkLookAhead)
      lookAheadBeforeFiring(se.dest, se.time)

    fire(se, se.dest)
  }

  def tick(): Unit = {
    if (canFireEvents())
      extractAndFireEvents()
  }

  override def receive: Receive = {
    case lst: List[Any] =>
      handleList(lst)
      tick()

    case _ => log.error("unknown message")
  }

  //ak message
  def handleList(lst: List[Any]) = {
    if (!(lst.last.isInstanceOf[Long]))
      throw new Exception("last element in list is not Done ")

    val from = lst.last.asInstanceOf[Long]
    currents.remove(from)

    lst.asInstanceOf[List].foreach {
      case se: SimEvent =>
        if (se.time >= simTime)
          felQueue += se

      case lh: LookAhead =>
        if (lh.isToRemoveAllSafe)
          lookAheads.remove(lh.target)
        else
          lookAheads.update(lh.target, lh)

      case p: SimTimedPredicate => //keep this order before case SimPredicate
        if (p.time >= simTime) {
          predicates.update(p.target, p)
          felQueue += p
        }

      case p: SimPredicate =>
        predicates.update(p.target, p)

      case id: Long => currents.remove(id) //TODO remove later

      case _ => println(s"message not known {}")
    }
  }
}
