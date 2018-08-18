package simakka.config

import net.liftweb.json.{DefaultFormats, parse}

object SimJEntity {
  def of(configPath: String): SimJEntity = {
    implicit val formats = DefaultFormats

    val data = io.Source.fromFile(configPath).mkString
    val result = parse(data).extract[SimJEntity]
    result
  }
}

case class SimJEntity(lookahead: Double = 0.0,
                      autoEvents: Boolean = false)

