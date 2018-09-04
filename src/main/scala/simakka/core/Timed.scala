package simakka.core

trait Timed {
  def time: Double

  def compare(that: SimEvent) = Math.signum(this.time - that.time)

  def notBefore(): PartialFunction[Any, Boolean] = {
    case that: Timed => this.time > that.time
  }
}

object SimEvent {
  final val NONE = SimEvent(0,0,0,0,None)

  val PAUSE = -100
  val PROCESS = -101
}

case class Pause(time: Double, src: Long) extends Timed //eventual
case class Process(time: Double, src: Long) extends Timed //eventual

/*Indicate Entity is:
 * 1 - Done processing event(s)
 * 2 - No more events expected from this
 * 3 - Ready to receive next event*/
case class Done(id: Long)

/* Transparent message should work without it  */
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