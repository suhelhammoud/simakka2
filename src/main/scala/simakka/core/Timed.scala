package simakka.core

trait Timed {

  def time: Double

  def compare(that: SimEvent) = Math.signum(this.time - that.time)

  def notBefore(): PartialFunction[Any, Boolean] = {
    case that: Timed  => this.time > that.time
  }
}

object SimEvent {
  val PAUSE = -100
  val PROCESS = -101
}

case class PauseEvent(time: Double, src: Long) extends Timed
case class ProcessEvent(time: Double, src: Long) extends Timed


case class SimEvent(time: Double,
                    tag: Int,
                    src: Long = -1,
                    dest: Long,
                    data: Option[Any] = None) extends Timed {


  def isPause = tag == SimEvent.PAUSE

  def isProcess = tag == SimEvent.PROCESS

}

//case class SimEventPause(override val time: Double,
//                         override val src: Long,
//                         override val dest: Long)
//  extends SimEvent(time, SimEvent.PAUSE, src = src, dest = dest, None)
//
//case class SimEventProcess(override val time: Double,
//                           override val src: Long,
//                           override val dest: Long)
//  extends SimEvent(time, SimEvent.PROCESS, src = src, dest = dest, None)


object Test {
  def main(args: Array[String]): Unit = {
    SimEvent(0.0, dest = 4, tag = 4)
  }
}
