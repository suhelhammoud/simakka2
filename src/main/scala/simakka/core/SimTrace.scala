package simakka.core

import akka.actor.Actor
import org.slf4j.{Logger, LoggerFactory}

object SimTrace {
  private val _tr: Logger = LoggerFactory.getLogger("SIMTRACE")

  def writeHeader(): Unit = {
    if (SimTrace._tr.isInfoEnabled)
      SimTrace._tr.info("time, id, name, tag, message")
  }
}

trait SimTrace {
  this: Actor ⇒

  def simTime: Double

  def me: Long

  def name: String

  def _preMessage(tag: String = "") =
    f"$simTime%4.4f, $me%6d, $name, $tag%8s, "

  def simTraceTag(tag: String,
                  template: String): Unit = {
    if (SimTrace._tr.isInfoEnabled)
      SimTrace._tr.info(_preMessage(tag) + template)
  }

  def simTraceTag(tag: String,
                  template: String,
                  arg1: Any): Unit = {
    if (SimTrace._tr.isInfoEnabled)
      SimTrace._tr.info(_preMessage(tag) + template,
        arg1)
  }

  def simTraceTag(tag: String,
                  template: String,
                  arg1: Any, arg2: Any): Unit = {
    if (SimTrace._tr.isInfoEnabled)
      SimTrace._tr.info(_preMessage(tag) + template,
        arg1, arg2)
  }

//  def simTraceTag(tag: String,
//                  template: String,
//                  arg1: Any, arg2: Any, arg3: Any): Unit = {
//    if (SimTrace._tr.isInfoEnabled)
//      SimTrace._tr.info(_preMessage(tag) + template,
//        arg1, arg2, arg3)
//  }

//  def simTraceTag(tag: String,
//                  template: String,
//                  arg1: Any, arg2: Any, arg3: Any, arg4: Any): Unit = {
//    if (SimTrace._tr.isInfoEnabled)
//      SimTrace._tr.info(_preMessage(tag) + template,
//        arg1, arg2, arg3, arg4)
//  }

  def simTrace(template: String): Unit = {
    if (SimTrace._tr.isInfoEnabled)
      SimTrace._tr.info(_preMessage() + template)
  }

  def simTrace(template: String,
               arg1: Any): Unit = {
    if (SimTrace._tr.isInfoEnabled)
      SimTrace._tr.info(_preMessage() + template,
        arg1)
  }

  def simTrace(template: String,
               arg1: Any, arg2: Any): Unit = {
    if (SimTrace._tr.isInfoEnabled)
      SimTrace._tr.info(_preMessage() + template,
        arg1, arg2)
  }

//  def simTrace(template: String,
//               arg1: Any, arg2: Any, arg3: Any): Unit = {
//    if (SimTrace._tr.isInfoEnabled)
//      SimTrace._tr.info(_preMessage() + template,
//        arg1, arg2, arg3)
//  }

//  def simTrace(template: String,
//               arg1: Any, arg2: Any, arg3: Any, arg4: Any): Unit = {
//    if (SimTrace._tr.isInfoEnabled)
//      SimTrace._tr.info(_preMessage() + template,
//        arg1, arg2, arg3, arg4)
//  }
}