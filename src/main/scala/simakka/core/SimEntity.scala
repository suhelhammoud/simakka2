package simakka.core

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.util.Timeout
import net.liftweb.json.{DefaultFormats, parse}
import simakka.config.SimJEntity
import simakka.core.SimFEL.{Done, LookAhead, SafeTime}

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._


trait NameID {
  val name: String
  val id: Long
}

object SimEntity {

  def configFromPath(configPath: String): SimJEntity = {
    implicit val formats = DefaultFormats
    val data = io.Source.fromFile(configPath).mkString
    val result = parse(data).extract[SimJEntity]
    result
  }

  val autoEvents = true

  def props(name: String, params: Option[String] = None)
  = Props(classOf[SimEntity], name, params)

  // if time < 0 done is true


}

class SimEntity(val name: String, params: Option[String] = None)
  extends Actor with SimEntityLookup with ActorLogging with NameID {

  val fel: ActorRef = getRef("fel").get

  lazy val id = getRefId(name).get

  val simTime = 0.0
  val lastEventTime = 0.0
  val outEvents = ArrayBuffer[SimEvent]()
  val autoEvents = true

  var lookahead = 0.0

  def setLookAhead(lh: Double): Unit = {
    lookahead = lh
    fel ! LookAhead(id, lh)
  }

  def removeLookAhead(): Unit = {
    setLookAhead(-1)
  }

  def setSafeTime(st: Double): Unit = {
    fel ! SafeTime(id, st)
  }

  def done(): Unit = {
    fel ! Done(id)
  }

  /*Used as timeout for synchronous calls*/
  implicit val timeout = Timeout(10 seconds)

  //  def sendOutEvents(s: Seq[SimEvent]): Unit = {
  //    fel ! s
  //  }

  if (params.nonEmpty)
    initParams(params.get)

  def initParams(data: String): Unit = {
    log.debug("initParams({})", data)
  }

  /**
    * Send local events
    *
    * @param delay
    * @param tag
    * @param data
    */
  def scheduleLocal(delay: Double, tag: Int, data: Option[Any] = None): Unit = {
    schedule(delay, tag, id, id, data)
  }


  /**
    * Send event from this SimEntity to other SimEntity by its id
    *
    * @param delay
    * @param tag
    * @param dest
    * @param data
    */
  def scheduleID(delay: Double, tag: Int, dest: Long, data: Option[Any] = None): Unit = {
    schedule(delay, tag, id, dest, data)
  }

  /**
    * Send event from this SimEntity to other SimEntity by its name
    *
    * @param delay
    * @param tag
    * @param dest
    * @param data
    */
  def scheduleName(delay: Double, tag: Int, dest: String, data: Option[Any] = None): Unit = {
    //    val thatId = entitiesNames.get(toS)
    val to = getRefId(dest)

    if (to == None) {
      log.error("Could not find id for on of actor : {}", dest)
    } else {
      schedule(delay, tag, id, to.get, data)
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
  def scheduleName(delay: Double, tag: Int, src: String, dest: String, data: Option[Any]): Unit = {

    val from = getRefId(src)
    val to = getRefId(dest)

    if (from == None || to == None) {
      //TODO use getRef() methods here
      log.error("Could not find id for on of actors : {}, {}", src, dest)
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
               data: Option[Any] = None): Unit = {

    //    assert(delay >= 0 || containsRef(src) || containsRef(dest))

    val nextTime = simTime + delay

    val ev = SimEvent(nextTime, tag, src, dest, data)
    if (SimEntity.autoEvents)
      fel ! ev
    else
      outEvents += ev
  }

  /**
    * Entity Logic
    *
    * @param msg simEvent to be processed
    * @return
    */
  def handleMessage(msg: SimEvent): Seq[SimEvent] = {
    log.debug("handle message {}", msg)
    Nil
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

    case _ => log.debug("received a message")
  }

  def toSting() = {
    s"${getClass.getName}, name = $name, id = $id"
  }

}
