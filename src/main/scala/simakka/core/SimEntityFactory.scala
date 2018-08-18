package simakka.core

import akka.actor.{Actor, ActorLogging, Props}
import simakka.core.SimApp.END_OF_SIMULATION
import simakka.core.SimEntityFactory.CreateEntity


class SimEntityFactory extends Actor with SimEntityLookup with ActorLogging {

  override def receive: Receive = {
    case CreateEntity(name, propVal) => {
      val entityRef = context.actorOf(propVal, name)
      val id = refName(name, entityRef)
      log.debug("created entity with name = {}, id = {}", name)
      sender() ! entityRef
    }
    case END_OF_SIMULATION =>
      log.error("END_OF_SIMULATION received")

    case _ => log.error("Entity Factory unrecognized message")
  }

}

object SimEntityFactory {

  case class CreateEntity(name: String, propVal: Props)

  def props() = Props(classOf[SimEntityFactory])

}

//case class CreateEntity(name: String, params: Option[String])