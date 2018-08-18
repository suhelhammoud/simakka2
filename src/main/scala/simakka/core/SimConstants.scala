package simakka.core

import akka.actor.ActorRef

object SimConstants {


  case object READY

  case object CLOSE


  case object SEND_STATS


  /**
    * Stat Messages
    */


  case class MeasureName(name: String, actorRef: ActorRef)

  case class MeasureId(id: Long, actorRef: ActorRef)

  case class AddMeasure(name: String, id: Long, actorRef: ActorRef)


  //TODO may add actorRef to authenticate the sender or to restrict the authority of removing stat
  case class DeleteMeasure(id: Long)


  case class SimMeasureValue(id: Long, time: Double, value: Double) {
    def csv = s"$id, $time, $value"
  }

  object SimMeasureValue {
    val header = s"id, time, value"
  }


}
