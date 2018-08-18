package simakka.core

import akka.actor.{Actor, ActorLogging, PoisonPill, Props}
import simakka.config.SimJConfig
import simakka.core.SimApp.{CreateChild, END_OF_SIMULATION}
import simakka.core.SimEntityFactory.CreateEntity


object SimApp {

  def props(configPath: String) = Props(classOf[SimApp], configPath)


  case class CreateChild(name: String, propVal: Props)


  case object END_OF_SIMULATION

}

/**
  * Centralized Actor for all sub-actors created in the system
  *
  * @param configPath
  */
class SimApp(configPath: String)
  extends Actor with ActorLogging with SimEntityLookup {

  val config = SimJConfig.of(configPath)

  val id = refName(SimNames.app.name(), self)

  lazy val entityFactory = getRef(SimNames.entityFactory.name())
  lazy val fel = getRef(SimNames.fel.name())
  lazy val statCollector = getRef(SimNames.statCollector.name())


  override def receive: Receive = {

    case CreateChild(name, propVal) =>
      log.info("create child {} from {}", name, sender())
      val ref = context.actorOf(propVal, name)
      refName(name, ref)
      sender() ! ref

    case ce: CreateEntity =>
      entityFactory.get forward ce
      log.debug("Create Entity with name = {}", ce.name)

    case END_OF_SIMULATION =>
      log.info("End of Simulation")
      entityFactory.get ! PoisonPill
      fel.get ! PoisonPill
      statCollector.get ! PoisonPill
      self ! PoisonPill

    case _ => log.debug("SimApp received default message")
  }
}

