package simakka.core

object PriorityEventQueue {
  type PEQueueType = collection.mutable.PriorityQueue[Timed]

  def newInstance() = new PEQueueType()(new Ordering[Timed] {
    override def compare(x: Timed, y: Timed): Int =
      Math.signum(y.time - x.time).toInt
  })
}
