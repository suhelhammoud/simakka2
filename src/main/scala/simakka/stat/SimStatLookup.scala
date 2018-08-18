package simakka.stat

;

import akka.actor.{Actor, ActorRef, ExtendedActorSystem, Extension, ExtensionId, ExtensionIdProvider}


/**
  * to be extended by any class or to be used as an akka Extension
  */
trait SimStatLookup {
  self: Actor =>

  val stat = SimStatLookupExtension(context.system)

  def getRef(name: String) =
    stat.getRef(name)

  def getRef(id: Int) =
    stat.getRef(id)

  def putRef(name: String, id: Int, actorRef: ActorRef) =
    stat.putRef(name, id, actorRef)

  def containsRef(name: String) =
    stat.containsRef(name)

  def containsRef(id: Int) =
    stat.containsRef(id)

  def toStringRef() =
    stat.toStringRef()

}

class SimStatLookupExtensionImp extends Extension {
  private val stats = new scala.collection.concurrent.TrieMap[Long, ActorRef]
  private val statsNames = new scala.collection.concurrent.TrieMap[String, Long]

  //TODO handle None results
  def getRef(idd: String) = stats.get(statsNames.get(idd).get)

  def getRef(id: Long) = stats.get(id)

  def putRef(name: String, id: Long, actorRef: ActorRef) = {
    statsNames.put(name, id)
    stats.put(id, actorRef)
  }

  def containsRef(name: String) = statsNames.contains(name)

  def containsRef(id: Long) = stats.contains(id)

  def toStringRef(): String = stats.toString()
}

object SimStatLookupExtension extends ExtensionId[SimStatLookupExtensionImp]
  with ExtensionIdProvider {

  override def lookup(): ExtensionId[_ <: Extension] = SimStatLookupExtension

  override def createExtension(system: ExtendedActorSystem) = new SimStatLookupExtensionImp

  //  override def get(system: ActorSystem): SimEntityLookupExtensionImp = super.get(system)


}
