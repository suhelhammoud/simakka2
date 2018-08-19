package simakka.core

import akka.actor.Stash


object SimEmbedQueue{
  type SimPredicate = PartialFunction[Any, Boolean]
  type SimEventPredicate = PartialFunction[Any, Boolean]

}

trait SimEmbedQueue extends Stash{
  self: SimEntity =>


}
