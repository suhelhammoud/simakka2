package simakka.stat

import java.io.{FileWriter, PrintWriter}
import java.nio.file.{Files, Paths}

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import simakka.config.SimJConfig
import simakka.core.SimConstants._
import simakka.core.{SimApp, SimUtils}


object SimStatCollector {
}

trait SimStatCollector {
  _: Actor =>



  private val stats = new scala.collection.concurrent.TrieMap[Long, ActorRef]
  private val statsNames = new scala.collection.concurrent.TrieMap[String, Long]

  def collect(s: SimMeasureState)

  def collect(r: SimMeasureRate)

  def collect(i: SimMeasureInterval)

  def collect(v: SimMeasureValue)

  def close()

  def getMeasure(id: Long, actorRef: ActorRef): Unit = {
    val measure = stats.get(id)
    if (measure.nonEmpty) {
      measure.get ! MeasureId(id, actorRef)
    }
    println(s"could not find stat measure $id")
  }

  def getMeasure(name: String, actorRef: ActorRef): Unit = {
    val id = statsNames.get(name)
    if (id.nonEmpty) {
      stats.get(id.get).get ! MeasureName(name, actorRef)
    } else {
      println(s"could not find stat measure $name")
    }
  }

  def receive: Receive = {

    case s: SimMeasureState => collect(s)
    case r: SimMeasureRate => collect(r)
    case i: SimMeasureRate => collect(i)
    case v: SimMeasureValue => collect(v)


    case AddMeasure(name, id, actorRef) =>
      //TODO check if already exists
      stats.put(id, actorRef)
      statsNames.put(name, id)

    case DeleteMeasure(id) =>
      if (stats.contains(id)) {
        stats.remove(id)
        val (name, _) = statsNames.filter(entry => entry._2 == id).head
        statsNames.remove(name)
      }

    //TODO subscribe, step schedule, etc

    case CLOSE => close()

    case _ => println("Unknown message")
  }

}

class SimStatCollectorFile(val outPath: String)
  extends Actor with SimStatCollector with ActorLogging {

  //  val statConfig = SimJConfig.of(configPath).stat

  def _createFile(filePath: String) = {

    SimJConfig.createDirsOver(outPath)
    val path = Paths.get(filePath)
    val file = Files.createFile(path).toFile
    val pw = new PrintWriter(new FileWriter(file))
    pw.println(SimMeasureState.header)
    pw
  }

  val pwState = _createFile(outPath + "/state.txt")
  val pwRate = _createFile(outPath + "/rate.txt")
  val pwInterval = _createFile(outPath + "/interval.txt")
  val pwValue = _createFile(outPath + "/else.txt")


  pwState.println(SimMeasureState.header)
  pwRate.println(SimMeasureRate.header)
  pwInterval.println(SimMeasureInterval.header)
  pwValue.println(SimMeasureValue.header)


  override def collect(s: SimMeasureState): Unit = pwState.println(s.csv)

  override def collect(r: SimMeasureRate): Unit = pwRate.println(r.csv)

  override def collect(i: SimMeasureInterval): Unit = pwInterval.println(i.csv)

  override def collect(i: SimMeasureValue): Unit = pwValue.println(i.csv)

  override def close(): Unit = {
    pwState.close()
    pwRate.close()
    pwInterval.close()
    pwValue.close()
  }
}

object SimStatCollectorFile {

  def props(outPath: String, name: String = "Stat Collector") = Props(classOf[SimStatCollectorFile], outPath)


  def main(args: Array[String]) {
    //experimental test
    println("SimStatCollector Test")

    val system = ActorSystem("SimAkka")
    val statCollect = system.actorOf(Props(classOf[SimStatCollectorFile],
      s"/mnt/SHARED/Downloads/temp/stat_collector_${System.nanoTime()}.txt"), "statCollect")

    val rnd = scala.util.Random

    SimUtils.timeIt {

      for (i <- 1 to 2000000) {
        val sm = SimMeasureValue(rnd.nextInt(3).toLong, rnd.nextDouble(), rnd.nextDouble())


        statCollect ! sm
      }

      statCollect ! CLOSE
    }

    println("finished sending, wait and terminate")

    Thread.sleep(4000)
    println("call end of simulation")
    statCollect ! SimApp.END_OF_SIMULATION
    //    system.terminate()
  }
}