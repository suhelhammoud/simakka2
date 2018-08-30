package simakka.core

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Stash}
import akka.util.Timeout
import net.liftweb.json.{DefaultFormats, parse}
import simakka.config.SimJEntity

import scala.collection.mutable
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

  // if time < 0 done is true
}

class SimEntity(val name: String, params: Option[String] = None)
  extends Actor with SimEntityLookup
    with ActorLogging with SimTrace with NameMe with Stash{

  val fel: ActorRef = getRef(SimNames.fel.name()).get

  lazy val me = getRefId(name).get

  var simTime = 0.0
  var lastEventTime = 0.0
  val outMessages = ArrayBuffer[Any]()

  var lookahead: Option[LookAhead] = None

  val predicate = new SimPredicateGenerator(me)

  val behaviour = mutable.Map[String, PartialFunction[Any, Unit]]()
  var lastBehaviour: Option[String] = None
  var lastPredicate: Option[SimPredicate] = None

  def on(name: String)(pf: PartialFunction[Any, Unit]): Unit = {
    if (behaviour contains (name))
      throw new Exception("key already in handlers " + name)
    behaviour += name -> pf
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

  def done(): Unit = {
    fel ! me
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


  def pause(delay: Double, behaviour: String): Unit = {
    val p = predicate.until(simTime + delay)
    outMessages.append(p)
    lastBehaviour = Some(behaviour)
    lastPredicate = Some(p)

    //TODO check that behaviour is actually saved in behaviour map
  }

  //  def foo(): Unit = {
  //    var myvar = 10;
  //    ;
  //    process(100, "b1")
  //    process(50, "b2")
  //
  //    //block after perdicate is matched
  //    //def behaviour b2
  //    {
  //
  //    }
  //
  //    on("b2") {
  //      myvar += 3
  //    }
  //
  //    on("b1") {
  //      //read myvar
  //    }
  //
  //  }

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
  def scheduleLocal(delay: Double, tag: Int, data: Option[Any] = None): Unit = {
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
  def scheduleID(delay: Double, tag: Int, dest: Long, data: Option[Any] = None): Unit = {
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
  def scheduleName(delay: Double, tag: Int, dest: String, data: Option[Any] = None): Unit = {
    //    val thatId = entitiesNames.get(toS)
    val to = getRefId(dest)

    if (to == None) {
      log.error("Could not find id for on of actor : {}", dest)
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
    simTrace("schedule event {}", ev)
    outMessages += ev
  }

  /**
    * Entity Logic
    *
    * @param msg simEvent to be processed
    * @return
    */
  def handleMessage(msg: SimEvent) {
    log.debug("handle message {}", msg)
    simTrace("handle message {}", msg)

    var localValue = 2

    pause(10, "b1")

    on("b1"){
      case se:SimEvent if se.tag == 10 =>
        log.info("test")
      case l: Long => println("ok")
      case i: Int =>
        localValue +=2
        pause(20, "b2")
    }

    on("b2"){
      case se: SimEvent if se.src == 20 => _

      case t: Timed if t.time < 500 =>

        predicate.notForTag(5).

    }


  }

  def setBehaviour(b: String): Unit ={
    context.become(behaviour.get(b).get)
  }

  override def receive: Receive = {

    case m: Any =>
      if(lastPredicate.isDefined) {
        if(m.isInstanceOf[SimEvent]) {
          if( !lastPredicate.get
            .isMatching(m.asInstanceOf[SimEvent])) {
            stash()

          }else{
            setBehaviour(lastBehaviour.get)
            unstashAll()
          }
        }
      }
    case se: SimEvent =>

      //
      context.become(behaviour.get(lastBehaviour.get).get)

      log.debug("entity {} received {}", name, se)
      if (se.time > simTime) {
        lastEventTime = simTime
        simTime = se.time
      }


      handleMessage(se)

      if (lookahead.isDefined
        && lookahead.get.isAllSafe()) {
        //Do nothing
      } else {
        outMessages.append(me)
        fel ! outMessages.toList
        outMessages.clear()
      }

    case _ => log.debug("received a message")
  }

  def toSting() = {
    s"${getClass.getName}, name = $name, id = $me"
  }

}
