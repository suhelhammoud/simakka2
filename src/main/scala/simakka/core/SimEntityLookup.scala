package simakka.core

import java.util.concurrent.atomic.AtomicLong
import akka.actor.{Actor, ActorRef, ExtendedActorSystem, Extension, ExtensionId, ExtensionIdProvider}

/**
  * to be extended by any class or to be used as an akka Extension
  */
trait SimEntityLookup {
  self: Actor =>
  val entityLookUp = SimEntityLookupExtension(context.system)

  def getRef(name: String) =
    entityLookUp.getRef(name)

  def getRefId(name: String) =
    entityLookUp.getRefId(name)

  def getRef(id: Long) =
    entityLookUp.getRef(id)

  def putRef(name: String, id: Long, actorRef: ActorRef) =
    entityLookUp.putRef(name, id, actorRef)

  def refName(name: String, actorRef: ActorRef): Long =
    entityLookUp.refName(name, actorRef)

  def containsRef(name: String) =
    entityLookUp.containsRef(name)

  def containsRef(id: Long) =
    entityLookUp.containsRef(id)

  def toStringRef() =
    entityLookUp.toStringRef()
}

class SimEntityLookupExtensionImp extends Extension {
  private val atomicID = new AtomicLong(0L)

  private val entities = new scala.collection.concurrent.TrieMap[Long, ActorRef]
  private val entitiesNames = new scala.collection.concurrent.TrieMap[String, Long]

  //TODO handle None results
  def getRefId(name: String) = entitiesNames.get(name)

  def getRef(name: String) = entities.get(entitiesNames.get(name).get)

  def getRef(id: Long) = entities.get(id)

  def putRef(name: String, id: Long, actorRef: ActorRef) = {
    entitiesNames.put(name, id)
    entities.put(id, actorRef)
  }

  def refName(name: String, actorRef: ActorRef): Long = {
    val id = atomicID.incrementAndGet()
    putRef(name, id, actorRef)
    return id
  }

  def containsRef(name: String) = {
    entitiesNames.contains(name)
  }

  def containsRef(id: Long) = entities.contains(id)

  def toStringRef(): String = entities.toString()
}

object SimEntityLookupExtension extends ExtensionId[SimEntityLookupExtensionImp]
  with ExtensionIdProvider {

  override def lookup(): ExtensionId[_ <: Extension] = SimEntityLookupExtension

  override def createExtension(system: ExtendedActorSystem) = new SimEntityLookupExtensionImp

  //  override def get(system: ActorSystem): SimEntityLookupExtensionImp = super.get(system)
}
