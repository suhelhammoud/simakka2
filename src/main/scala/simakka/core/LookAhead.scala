package simakka.core


object LookAhead {
  val ONCE: Int = 0
  val REPEATED: Int = -1
  val ALL_SAFE: Int = -2
  val REMOVE_All_SAFE: Int = -3

  def once(target: Long, time: Double) =
    new LookAhead(target, time, 0, ONCE)

  def repeated(target: Long, time: Double, step: Double) =
    new LookAhead(target, time, step, REPEATED)

  def allSafe(target: Long, time: Double) =
    new LookAhead(target, time, 0.0, ALL_SAFE)

  def removeAllSafe(target: Long, time: Double) =
    new LookAhead(target, time, 0.0, REMOVE_All_SAFE)

}

/**
  *
  * @param target
  * @param time if = -1 then remove this lookAhead message
  */
case class LookAhead(target: Long, time: Double,
                     step: Double, tag: Int = LookAhead.ONCE) extends Timed {

  def isOnce() = tag == LookAhead.ONCE

  def isAllSafe() = tag == LookAhead.ALL_SAFE

  def isToRemoveAllSafe = tag == LookAhead.REMOVE_All_SAFE

  //allow to set current values automatically
  def isRepeated() = tag == LookAhead.REPEATED

  def getType() = tag

  def nextCurrentTime(t: Double) = time + step

}

class SafeTime(val target: Long, var time: Double, var step: Double) {

  def update(t: Double) {
    if (step > 0)
      time = t + step
  }

  def update(): Unit = {
    if (step > 0)
      time += step
  }

  def update(lh: LookAhead): Unit = {
    assert(target == lh.target)
    time = lh.time
    step = lh.step
  }
}

