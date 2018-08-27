package simakka

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import org.slf4j.LoggerFactory
import simakka.config.SimJConfig
import simakka.core.SimApp.CreateChild
import simakka.core.SimEntityFactory.CreateEntity
import simakka.core._
import simakka.stat.SimStatCollectorFile

import scala.concurrent.Await
import scala.concurrent.duration._


class SimAkkaAppDriver(val configPath: String) {

  val log = LoggerFactory.getLogger("Driver")

  SimTrace.writeHeader

  implicit val timeout = Timeout(20 seconds)

  val config = SimJConfig.of(configPath)

  val actorSystem = ActorSystem(config.name)
  log.info("Created actorSystem {}", actorSystem.name)

  val ref = SimEntityLookupExtension(actorSystem)

  /* SimApp Actor */
  val app = actorSystem.actorOf(
    SimApp.props("data/config/config.json"),
    SimNames.app.name())
  log.info("Created SimApp name = {}",
    SimNames.app.name())


  /* SimCollector Actor */
  val statCollector = newActor(SimNames.statCollector.name(),
    SimStatCollectorFile.props(config.stat.outPath))
  log.info("Created SimStatCollector name = {}",
    SimNames.statCollector.name())


  /* SimEntityFactory Actor */
  val entityFactory = newActor(SimNames.entityFactory.name(),
    SimEntityFactory.props())

  log.info("Created SimEntityFactory name = {}",
    SimNames.entityFactory.name())


  /* SimFEL Actor */
  val fel = newActor(SimNames.fel.name(),
    SimFEL.props())
//  val fel = newActor(SimNames.fel.name(),
//    SimFEL.props())

  log.info("Create Future Event List name = {}",
    SimNames.fel.name())


  def newActor(name: String, props: Props) = {
    val future = app ? CreateChild(name, props)
    Await.result(future, timeout.duration).asInstanceOf[ActorRef]
  }

  def newEntity(name: String, props: Props) = {
    val future = app ? CreateEntity(name, props)
    Await.result(future, timeout.duration).asInstanceOf[ActorRef]
  }

  def initSimulation(): Unit = {
    log.info("initSimulation with config.name = {}", config.name)
  }

  def runSimulation(): Unit = {
    log.info("Run simulation")
  }


  def stopSimulation(): Unit = {
    log.info("stopSimulation")
  }

}

object SimAkkaAppDriver {
  val log = LoggerFactory.getLogger(SimAkkaAppDriver.getClass.getName)

  def main(args: Array[String]): Unit = {

    val configPath = "data/config/config.json"

    val config = SimJConfig.of(configPath)

    log.info("{}", config.name)

    //    val configPath = "data/config/config.json"
    //
    //    val config = SimJConfig.of(configPath)

    val appDriver = new SimAkkaAppDriver(configPath) {
      override def runSimulation(): Unit = {
        super.runSimulation()
        log.info("more run behaviour")
      }
    }

    appDriver.initSimulation()
    appDriver.runSimulation()

    log.info("done SimAkkaAppDriver")

  }

}
