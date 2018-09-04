package simakka.core

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Stash}
import akka.util.Timeout
import net.liftweb.json.{DefaultFormats, parse}
import simakka.config.SimJEntity

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._

trait NameMe {
  val name: String
  val me: Long
}

object SimEntity {

  def configFromPath(configPath: String): SimJEntity = {
    implicit val formats = DefaultFormats
    val data = io.Source.fromFile(configPath).mkString
    val result = parse(data).extract[SimJEntity]
    result
  }

  //  val autoEvents = false //TODO remove it

  def props(name: String, params: Option[String] = None) =
    Props(classOf[SimEntity], name, params)

}

class SimEntity(val name: String, params: Option[String] = None)
  extends Actor with SimEntityLookup
    with ActorLogging with SimTrace with NameMe with Stash {

  import SimPredicateGenerator.DEFAULT

  /*Used as timeout for synchronous calls*/
  implicit val timeout = Timeout(10 seconds)


  val fel: ActorRef = getRef(SimNames.fel.name()).get

  lazy val me = getRefId(name).get

  var simTime = 0.0
  var lastEventTime = 0.0
  val outMessages = ArrayBuffer[Any]()

  var lookahead: Option[LookAhead] = None

  val predicate = new SimPredicateGenerator(me)

  if (params.nonEmpty)
    initParams(params.get)

  defineBehaviours()


  def on(behaviourName: String)(pf: PartialFunction[Any, Unit]): Unit = {
    predicate.on(behaviourName)(pf)
  }

  //TODO
  def setLookAhead(lh: LookAhead): Unit = {
    lookahead = Some(lh)
    outMessages.append(lh)
  }

  def removeLookAhead(): Unit = {
    if (lookahead.isDefined) {
      outMessages.append(
        lookahead.get.copy(tag = LookAhead.REMOVE_All_SAFE))
    }
    lookahead = None
  }

  def initParams(data: String): Unit = {
    log.debug("initParams({})", data)
  }

  def defineBehaviours(): Unit = {
    log.debug("Entity {}, defineBehaviours ", name)
    //
    //    var someVariable = 3L
    //
    //    predicate.notForTag(4, 5)
    //
    //    predicate.on(DEFAULT) {
    //      case a: Any =>
    //        someVariable += 1
    //        println("defalt behaviour")
    //    }
  }

  def pause(delay: Double, behaviour: String = DEFAULT): Unit = {
    predicate.until(simTime + delay)(behaviour)
  }

  def process(delay: Double, behaviour: String): Unit = {
    //TODO
  }

  def pauseFor(from: Long, timeout: Double = Double.MaxValue): Unit = {
    //TODO
  }

  /**
    * Send local events
    *
    * @param delay
    * @param tag
    * @param data
    */
  def scheduleLocal(delay: Double, tag: Int, data: Option[Any] = None): SimEvent = {
    schedule(delay, tag, me, me, data)
  }

  /**
    * Send event from this SimEntity to other SimEntity by its id
    *
    * @param delay
    * @param tag
    * @param dest
    * @param data
    */
  def scheduleID(delay: Double, tag: Int, dest: Long, data: Option[Any] = None): SimEvent = {
    schedule(delay, tag, me, dest, data)
  }

  /**
    * Send event from this SimEntity to other SimEntity by its name
    *
    * @param delay
    * @param tag
    * @param dest
    * @param data
    */
  def scheduleName(delay: Double, tag: Int, dest: String, data: Option[Any] = None): SimEvent = {
    //    val thatId = entitiesNames.get(toS)
    val to = getRefId(dest)

    if (to == None) {
      log.error("Could not find id for on of actor : {}", dest)
      SimEvent.NONE
    } else {
      schedule(delay, tag, me, to.get, data)
    }
  }

  /**
    * Send event between any other two SimEntities from this place, use SimEntity name for the source and destination
    *
    * @param delay
    * @param tag
    * @param src
    * @param dest
    * @param data
    */
  def scheduleName(delay: Double, tag: Int, src: String, dest: String, data: Option[Any]): SimEvent = {

    val from = getRefId(src)
    val to = getRefId(dest)

    if (from == None || to == None) {
      //TODO use getRef() methods here
      log.error("Could not find id for on of actors : {}, {}", src, dest)
      SimEvent.NONE
    } else {
      schedule(delay, tag, from.get, to.get, data)
    }
  }

  /**
    * This method is called by all other variations.
    *
    * @param delay
    * @param tag
    * @param src
    * @param dest
    * @param data
    */
  def schedule(delay: Double,
               tag: Int,
               src: Long,
               dest: Long,
               data: Option[Any] = None): SimEvent = {

    //    assert(delay >= 0 || containsRef(src) || containsRef(dest))

    val nextTime = simTime + delay
    val ev = SimEvent(nextTime, tag, src, dest, data)
    outMessages += ev
    ev
  }


  override def receive: Receive = {

    case m: Timed =>

      if (predicate.isPredicateActive) {
        if (predicate.isMatching(m)) {
          predicate.done()
          unstashAll()
          //TODO check akka documentaiton examples
        } else stash() //predicate is not matched

      } else {
        //TODO update stats

        //update time
        if (m.time > simTime) {
          lastEventTime = simTime
          simTime = m.time
        }

        //handle message
        handle(m)
      }

    case m: Any =>
      log.error("Entity name {} , received unknown message = {}", name, m)
  }

  def handle(m: Timed): Unit = {
    if (lookahead.isDefined
      && lookahead.get.isAllSafe()) {
      //Do nothing
    } else {
      outMessages.append(me)
      predicate.handleMessage(m)
      fel ! outMessages.toList
      outMessages.clear()
    }
  }

  def toSting() = {
    s"${getClass.getName}, name = $name, id = $me"
  }

}
