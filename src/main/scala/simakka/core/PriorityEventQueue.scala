package simakka.core

object PriorityEventQueue {
  type PEQueueType = collection.mutable.PriorityQueue[SimEvent]

  def newInstance() = new PEQueueType()(new Ordering[SimEvent] {
    override def compare(x: SimEvent, y: SimEvent): Int =
      Math.signum(y.time - x.time).toInt
  })

}
