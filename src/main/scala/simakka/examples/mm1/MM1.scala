package simakka.examples.mm1

import akka.actor.{PoisonPill, Props}
import net.liftweb.json.DefaultFormats
import org.slf4j.LoggerFactory
import simakka.SimAkkaAppDriver
import simakka.config.SimJConfig
import simakka.core.SimApp.END_OF_SIMULATION
import simakka.core.SimUtils.toJsonString
import simakka.core.{SimEntity, SimTrace, SimUtils, Timed}



object MM1 {

  val log = LoggerFactory.getLogger(MM1.getClass.getName)

  case class Arrive(val time: Double) extends Timed

  case class Depart(val time: Double) extends Timed


  def props2(name: String, data: Any) = {
    props(name, Some(toJsonString(data)))
  }

  def props(name: String, params: Option[String] = None)
  = Props(classOf[MM1], name, params)

  case class MM1Config(interArrival: Double,
                       service: Double,
                       qCapacity: Int,
                       endTime: Double
                      )

  def getConfig(filename: String) = {
    implicit val formats = DefaultFormats
    SimUtils.toJValueFromPath(filename)
      .extract[MM1Config]
  }


  def main(args: Array[String]): Unit = {
    val configPath = "data/config/config.json"

    val config = SimJConfig.of(configPath)

    log.info("{}", config.name)

    //    val configPath = "data/config/config.json"
    //
    //    val config = SimJConfig.of(configPath)

    val appDriver = new SimAkkaAppDriver(configPath) {

      val mm1config = MM1Config(10, 5, 100, 1000)
      toJsonString(mm1config)

      val mm1 = newEntity("MM1",
        MM1.props2("MM1", mm1config))

      //      val mm1 =
      override def initSimulation(): Unit = {
        super.initSimulation()


      }

      override def runSimulation(): Unit = {
        super.runSimulation()
        log.info("more run behaviour")
        mm1 ! Arrive(22)
        mm1 ! Arrive(33)
        mm1 ! Depart(44)

        mm1 ! PoisonPill
        app ! END_OF_SIMULATION
      }
    }

    appDriver.initSimulation()
    appDriver.runSimulation()

    log.info("done SimAkkaAppDriver")
  }
}

class MM1(override val name: String, params: Option[String])
  extends SimEntity(name, params) with SimTrace{

//  val tr = LoggerFactory.getLogger("TRACE")

  override def initParams(data: String): Unit = {
    super.initParams(data)
    log.debug("initParams with data = {}", data)

  }

  override def receive: Receive = {
    case a: MM1.Arrive =>
      simTime = a.time
      simTrace("template =  {} end.", a.toString)
      log.info("arrive at time = {}", a.time)
    case d: MM1.Depart =>
      simTime = d.time
      simTrace("template =  {} end.", d.toString)
      log.info("depart at time = {}", d.time)
    case _ => log.error("unknown message")
  }
}
