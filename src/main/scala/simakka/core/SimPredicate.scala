package simakka.core

trait SimPredicate {
  val target: Long

  def isMatching(se: SimEvent): Boolean
}

trait SimTimedPredicate extends SimPredicate with Timed {
  def done = SimPredicateDone(target, this.time)

  def getLookAhead() = {
  }
}

trait Repeated {

  def repeat(): SimPredicate
}

class SimPredicateGenerator(val me: Long) {
  def until(time: Double) = Until(me, time)

  def from(ids: Long*) = From(me, ids.toList)

  def notFrom(ids: Long*) = NotFrom(me, ids.toList)

  def anyMessage() = AnyMessage(me)

  def noneMessage() = NoneMessage(me)

  def untilOrFrom(time: Double, ids: Long*) = UntilOrFrom(me, time, ids.toList)

  def untilOrNotFrom(time: Double, ids: Long*) = UntilOrFrom(me, time, ids.toList)

  def forTag(tags: Int*) = ForTag(me, tags.toList)

  def notForTag(tags: Int*) = NotForTag(me, tags.toList)

  def forTagUntil(time: Double, tags: Int*) = ForTagUntil(me, time, tags.toList)

  def notForTagUntil(time: Double, tags: Int*) = NotForTagUntil(me, time, tags.toList)

}

object SimPredicate {
  def generator(target: Long) = new SimPredicateGenerator(target)
}

case class SimPredicateDone(val target: Long, val time: Double) extends Timed

case class Until(val target: Long, val time: Double)
  extends SimPredicate with Timed {
  override def isMatching(se: SimEvent): Boolean =
    se.time >= time
}

case class UntilR(val target: Long, val time: Double, val step: Double)
  extends SimPredicate with Timed with Repeated {

  override def isMatching(se: SimEvent): Boolean = se.time >= time

  override def repeat(): SimPredicate = this.copy(time = this.time + this.step)

}

case class From(val target: Long, val ids: List[Long]) extends SimPredicate {
  override def isMatching(se: SimEvent): Boolean =
    ids.contains(se.dest)
}

case class NotFrom(val target: Long, val ids: List[Long]) extends SimPredicate {
  override def isMatching(se: SimEvent): Boolean =
    !(ids.contains(se.dest))
}

case class AnyMessage(val target: Long) extends SimPredicate {
  override def isMatching(se: SimEvent): Boolean = true
}

//TODO add predicate to match certain types of messages

case class NoneMessage(val target: Long) extends SimPredicate {
  override def isMatching(se: SimEvent): Boolean = false
}

case class UntilOrFrom(val target: Long, val time: Double, ids: List[Long])
  extends SimPredicate with Timed {
  override def isMatching(se: SimEvent): Boolean =
    se.time >= time || ids.contains(se.dest)
}

case class UntilOrNotFrom(val target: Long, val time: Double, ids: List[Long])
  extends SimPredicate with Timed {
  override def isMatching(se: SimEvent): Boolean =
    se.time >= time || !(ids.contains(se.dest))
}

case class ForTag(val target: Long, val tags: List[Int])
  extends SimPredicate {
  override def isMatching(se: SimEvent): Boolean =
    tags.contains(se.tag)
}

case class NotForTag(val target: Long, val tags: List[Int])
  extends SimPredicate {
  override def isMatching(se: SimEvent): Boolean =
    !(tags.contains(se.tag))
}

case class ForTagUntil(val target: Long, val time: Double, val tags: List[Int])
  extends SimPredicate with Timed {
  override def isMatching(se: SimEvent): Boolean =
    tags.contains(se.tag) || se.time >= time
}

case class NotForTagUntil(val target: Long, val time: Double, val tags: List[Int])
  extends SimPredicate with Timed {
  override def isMatching(se: SimEvent): Boolean =
    !(tags.contains(se.tag)) || se.time >= time
}

//TODO check if useful to add case class for one tag, or one id
