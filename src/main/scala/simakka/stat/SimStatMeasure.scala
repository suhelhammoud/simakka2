package simakka.stat

import java.util.concurrent.atomic.AtomicLong

import akka.actor.{Actor, ActorRef}
import simakka.core.SimConstants.{AddMeasure, SimMeasureValue}


trait SimStat {
  _: Actor =>

  val collector: Option[ActorRef]
  val statNames = new scala.collection.concurrent.TrieMap[String, Long]
  val stats = new scala.collection.concurrent.TrieMap[Long, SimMeasure]

  def emmit(msg: Any) = {
    if (collector.nonEmpty)
      collector.get ! msg
    else
      println(msg.toString)


  }

  def getMeasure(id: Long) = {
    stats.get(id)
  }

  def getMeasure(name: String) = {
    if (statNames.contains(name))
      stats.get(statNames.get(name).get)
    else
      Option.empty[SimMeasure]
  }

  def newMeasure(name: String, mType: Int) = {
    val measure = mType match {
      case SimMeasure.STATE_BASED => SimMeasure.ofState(name)
      case SimMeasure.RATE_BASED => SimMeasure.ofRate(name)
      case SimMeasure.INTERVAL_BASED => SimMeasure.ofInterval(name)
      case _ => throw new Exception(s"No measure of type $mType found")
    }
    //    if(statCollector.nonEmpty)
    emmit(measure.msgAddMeasure(context.self))
    statNames.put(measure.name, measure.id)
    stats.put(measure.id, measure)
    measure
  }


}

object SimMeasure {
  val STATE_BASED = 0
  val RATE_BASED = 1
  val INTERVAL_BASED = 2

  val ID = new AtomicLong(0)

  def ofState(name: String) = new SimMeasureState(name, ID.incrementAndGet())

  def ofRate(name: String) = new SimMeasureRate(name, ID.incrementAndGet())

  def ofInterval(name: String) = new SimMeasureInterval(name, ID.incrementAndGet())
}


trait SimMeasure {


  val name: String
  val id: Long

  protected var _value: Double = 0.0
  protected var _time: Double = 0.0
  protected var _minV = Double.MaxValue
  protected var _maxV = Double.MinValue
  protected var _count = 0

  def update(t: Double, v: Double): SimMeasure

  def value: Double = _value

  def value_=(t: Double, v: Double) = update(t, v)

  def min: Double

  def max: Double

  def avg: Double

  def header: String

  def csv: String

  def msgStatValue() = SimMeasureValue(id, _time, _value)

  def msgAddMeasure(actorRef: ActorRef) = AddMeasure(name, id, actorRef)


  def reportObservation(actorRef: Option[ActorRef]): Unit = {
    if (actorRef.nonEmpty)
      actorRef.get ! msgStatValue()
    else
      println(msgStatValue())
  }

  def +=(t: Double, v: Double) = {
    update(t, _value + v)
    this
  }

  def +=(t: Double, sm: SimMeasure) = {
    update(t, _value + sm.value)
    this
  }

  def -=(t: Double, v: Double) = {
    update(t, _value - v)
    this
  }

  def -=(t: Double, sm: SimMeasure) = {
    update(t, _value - sm.value)
    this
  }

  def *=(t: Double, v: Double) = {
    update(t, _value * v)
    this
  }

  def *=(t: Double, sm: SimMeasure) = {
    update(t, _value * sm.value)
    this
  }

  def /=(t: Double, v: Double) = {
    update(t, _value / v)
    this
  }

  def /=(t: Double, sm: SimMeasure) = {
    update(t, _value / sm.value)
    this
  }


  def +(v: Double) = value + v

  def -(v: Double) = value - v

  def *(v: Double) = value * v

  def /(v: Double) = value / v

  implicit def SimMeasureToDouble(sm: SimMeasure) = sm.value
}


//object SimMeasureState {
//
//
//
//  def apply(name: String, id: Long) = new SimMeasureState(name, id)
//
//}

object SimMeasureState {
  def header = "name, type, time, value, min, max, count, intervals, avg"
}

class SimMeasureState(val name: String, val id: Long) extends SimMeasure {

  //  private var _time = 0.0
  private var _intervals = 0.0
  private var _accum = 0.0
  private var _avg = 0.0

  override def update(t: Double, v: Double) = {
    _minV = _minV min v
    _maxV = _maxV max v
    val d = t - _time
    _intervals += d
    _count += 1
    _accum += _value * _intervals
    _avg = _accum / _intervals
    _time = t
    _value = v
    this
  }

  override def min: Double = _minV

  override def max: Double = _maxV

  override def avg: Double = _avg

  override def header: String = SimMeasureValue.header

  override def csv: String =
    f"$name, ${SimMeasure.STATE_BASED}, ${_time}%1.5f, ${_value}%1.5f, ${_minV}%1.5f, ${_maxV}%1.5f, ${_count}, ${_intervals}%1.5f, ${_avg}%1.5f"


}


object SimMeasureRate {
  def header = "name, type, time, value, min, max, count, intervals, avg"
}

class SimMeasureRate(val name: String, val id: Long) extends SimMeasure {

  override def update(t: Double, v: Double) = {
    this
  }

  override def value: Double = 0.0

  override def min: Double = 0.0

  override def max: Double = 0.0

  override def avg: Double = 0.0

  override def header: String = ""

  override def csv: String = ","
}

object SimMeasureInterval {
  def header = "name, type, time, value, min, max, count, intervals, avg"
}

class SimMeasureInterval(val name: String, val id: Long) extends SimMeasure {
  override def update(t: Double, v: Double) = {
    this
  }


  override def min: Double = 0.0

  override def max: Double = 0.0

  override def avg: Double = 0.0

  override def header: String = ""

  override def csv: String = ","
}

