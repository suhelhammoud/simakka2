package simakka.core

import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable


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

object SimPredicateGenerator {
  val log: Logger = LoggerFactory.getLogger(SimPredicateGenerator.getClass.getName)
  final val DEFAULT = "default"
}

class SimPredicateGenerator(val me: Long) {

  import SimPredicateGenerator.{log, DEFAULT}
  //TODO add constraints to

  val behaviourMap = mutable.Map[String, PartialFunction[Any, Unit]]()

  var lastName: Option[String] = None
  var lastPredicate: Option[SimPredicate] = None

  var lastBehaviour: Option[PartialFunction[Any, Unit]] = None

  var isPredicateActive = false


  /**
    * called if "isActive" only
    *
    * @param msg
    */
  def isMatching(msg: Timed): Boolean = {
    msg match {
      case se: SimEvent => lastPredicate.get.isMatching(se)
      case sp: SimTimedPredicate => true
    }
    log.error("out of cases for id ={}, isActive = {}, msg = {}", me, isPredicateActive, msg)
    false
  }

  def done(): Unit = {
    assert(lastName.isDefined)
    assert(behaviourMap.contains(lastName.get))

    lastBehaviour = Some(behaviourMap.get(lastName.get).get)
    isPredicateActive = false
  }

  def setPredicate(sp: SimPredicate, name: String = DEFAULT): Unit = {
    assert(behaviourMap.contains(name))

    lastName = Some(name)
    lastPredicate = Some(sp)
    isPredicateActive = true
  }

  def handleMessage(msg: Any): Unit ={
    assert(isPredicateActive = false)
    assert(lastBehaviour.isDefined)

    lastBehaviour.get.apply(msg)
  }

  /**
    * called once in the startup,
    * should at least define default behaviour on("default"){ case _....}
    */
  def setBehaviours(): Unit = {

    on("default") {
      case any: Any => println(any)
    }
  }

  def on(name: String)(pf: PartialFunction[Any, Unit]): Unit = {
    if (!behaviourMap.contains(name))
      behaviourMap += name -> pf
  }


  def until(time: Double)(name: String = DEFAULT) {
    //    val p = Until(me, time)
    setPredicate(Until(me, time), name)
  }

  def from(ids: Long*)(name: String = DEFAULT) {
    setPredicate(From(me, ids.toList), name)
  }

  def notFrom(ids: Long*)(name: String = DEFAULT): Unit = {
    setPredicate(NotFrom(me, ids.toList), name = name)
  }

  def anyMessage(name: String = DEFAULT): Unit = {
    setPredicate(AnyMessage(me), name)
  }

  def noneMessage(name: String): Unit = {
    setPredicate(NoneMessage(me), name)
  }

  def untilOrFrom(time: Double, ids: Long*)(name: String): Unit = {
    setPredicate(UntilOrFrom(me, time, ids.toList), name)
  }

  def untilOrNotFrom(time: Double, ids: Long*)(name: String): Unit = {
    setPredicate(UntilOrFrom(me, time, ids.toList), name)
  }

  def forTag(tags: Int*)(name: String): Unit = {
    setPredicate(ForTag(me, tags.toList), name)
  }

  def notForTag(tags: Int*)(name: String): Unit = {
    setPredicate(NotForTag(me, tags.toList), name)
  }

  def forTagUntil(time: Double, tags: Int*)(name: String): Unit = {
    setPredicate(ForTagUntil(me, time, tags.toList), name)
  }

  def notForTagUntil(time: Double, tags: Int*)(name: String): Unit = {
    setPredicate(NotForTagUntil(me, time, tags.toList), name)
  }

}

object SimPredicate {
  def generator(target: Long) = new SimPredicateGenerator(target)
}

case class SimPredicateDone(val target: Long, val time: Double) extends Timed

case class Until(val target: Long, val time: Double)
  extends SimTimedPredicate {
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
  extends SimTimedPredicate {
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
  extends SimTimedPredicate {
  override def isMatching(se: SimEvent): Boolean =
    tags.contains(se.tag) || se.time >= time
}

case class NotForTagUntil(val target: Long, val time: Double, val tags: List[Int])
  extends SimTimedPredicate {
  override def isMatching(se: SimEvent): Boolean =
    !(tags.contains(se.tag)) || se.time >= time
}

//TODO check if useful to add case class for one tag, or one id
