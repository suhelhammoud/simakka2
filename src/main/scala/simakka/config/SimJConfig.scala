package simakka.config

import better.files._
import net.liftweb.json.{DefaultFormats, parse}


case class SimJConfig(name: String,
                      paths: SimJPaths,
                      trace: SimJTrace,
                      stat: SimJStat)

//                      entity: Option[SimJEntity])

case class SimJApp(app: String)

case class SimJPaths(rootPath: String,
                     outPath: String)

case class SimJTrace(useTracing: Boolean,
                     tracePath: String,
                     traceLevel: Int)

case class SimJStat(collector: String,
                    outPath: String)




object SimJConfig {
  //  implicit val formats = DefaultFormats

  def of(configPath: String): SimJConfig = {
    implicit val formats = DefaultFormats

    val data = io.Source.fromFile(configPath).mkString
    val result = parse(data).extract[SimJConfig]
    result
  }

  def createDirsOver(path: String): Unit = {
    val p = path.toFile
    if (p.exists && p.isDirectory)
      p.delete()
    p.createDirectories()
  }


  def main(args: Array[String]): Unit = {
    val config = SimJConfig.of("data/config/config.json")
    println(config)

  }


  //  createDirsOver("/tmp/a/b")

}