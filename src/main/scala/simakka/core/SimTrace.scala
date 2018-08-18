package simakka.core

import akka.actor.Actor
import org.slf4j.{Logger, LoggerFactory}

object SimTrace {
  private val _tr: Logger = LoggerFactory.getLogger("SIMTRACE")
}

trait SimTrace {
  this: Actor ⇒

  def simTime: Double
  def id: Long
  def name: String

  def _preMessage(tag: String = "") =
    f"$simTime%4.4f, $id%6d, $name, $tag%8s, "

  def simTrace(tag: String, template: String, params: Any): Unit ={
    if (SimTrace._tr.isInfoEnabled)
      SimTrace._tr.info(_preMessage(tag) + template, params)
  }

  def simTrace(template: String, params: Any): Unit = {
    if (SimTrace._tr.isInfoEnabled)
      SimTrace._tr.info(_preMessage() + template, params)
  }
}