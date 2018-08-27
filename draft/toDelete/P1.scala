package simakka.toDelete

import simakka.core.SimEvent
import scala.collection.mutable


// Test event based predicates
object P1 {

  type SimPredicate = PartialFunction[Any, Boolean]
  type SimEventPredicate = PartialFunction[Any, Boolean]

  def main(args: Array[String]): Unit = {
    println("predicates")

    val map = mutable.Map[String, PartialFunction[Any, Unit]]()

    def on(name: String)(pf: PartialFunction[Any, Unit]): Unit = {
      if (map contains (name))
        throw new Exception("key already in handlers " + name)
      map += name -> pf
    }


    var logger = "";
    var counter = 0;

    on("once") {
      case se: SimEvent if se.src == 4 =>
        counter += 1
        println("once from 4")

      case se: SimEvent =>
        counter += 1
        println("once from else =" + se)
    }

    on("twice") {
      case msg: String =>
        logger += msg + ", "
        println("twice string msg = " + msg)
      case se: SimEvent if se.src == 4 =>
        counter += 100
        println("twice from 4")
      case se: SimEvent =>
        counter += 100
        println("twice from else " + se)
    }

    val once = map.get("once").get
    val twice = map.get("twice").get

    val se1 = SimEvent(0.0, -1, 4, 100)
    val se2 = SimEvent(0.0, -1, 9, 100)

    once(se1)
    once(se1)
    println(counter)

    once(se2)
    println(counter)

    twice(se1)
    println(counter)
    twice(se2)
    println(counter)

    twice("hello from suhel")
  }
}
