package simakka.core

import akka.actor.{Actor, ActorRef, Stash}

object SimEntityQ {
  type SimPredicate = PartialFunction[Any, Boolean]
}

class SimEntityQ(override val name: String)
  extends SimEntity(name) with Stash {
  override val fel: ActorRef = self

  val receivers = Map[String, Actor.Receive](
    "main" -> receive
  )

  //  def predicate2(sp: SimPredicate, rName: Option[String] = None): Receive = {
  //    if (rName.isEmpty)
  //      predicate(sp)()
  //    else if (!(receivers.contains(rName.get))) {
  //      predicate(sp)()
  //    } else {
  //      predicate(sp)(receivers.get(rName.get))
  //    }
  //  }

  def predicate(sp: SimPredicate)(r: Receive = null): Receive = {
    case msg if sp(msg) =>
      stash()
      unstashAll()
      if (r == null)
        context.unbecome()
      else
        context.become(r)
    case _ => stash()
  }

  def process(delay: Double)(r: Receive): Receive = {
    predicate({ case se: SimEvent => se.time == simTime + delay })(r)
  }

  def waitFrom(id: Long)(r: Receive): Receive = {
    predicate({
      case se: SimEvent => se.src == id
    })(r)
  }

  override def receive: Receive = {

    case se: SimEvent if se.tag == 3 =>
      process(4) {
        case _ => println("delay end callback")
      }
      val wf = waitFrom(4)(_)

    case se: SimEvent =>
      handleMessage(se)
      if (outMessages.nonEmpty) {
        fel ! outMessages.toList
        outMessages.clear()
      }

      fel ! Done(me)

    case _ => println("received a message")
  }


  /**
    * Entity Logic
    *
    * @param msg simEvent to be processed
    * @return
    */
  override def handleMessage(msg: SimEvent): Seq[SimEvent] = {
    List()
    //    case _ => println("")
  }
}

