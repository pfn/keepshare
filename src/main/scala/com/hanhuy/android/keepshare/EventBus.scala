package com.hanhuy.android.keepshare

import AndroidConversions._

import android.os.{Handler, Looper}

sealed trait BusEvent
case object ServiceExit extends BusEvent
case object KeyboardExit extends BusEvent

object EventBus {

  class Owner {
    var handlers = Seq.empty[EventBus.Handler]
  }

  trait RefOwner {
    implicit val __eventBusRefOwner__ = new Owner
  }

  // this is terribad -- only EventBus.Remove has any meaning
  type Handler = PartialFunction[BusEvent,Any]
  object Remove // result object for Handler, if present, remove after exec
}
abstract class EventBus {
  import ref.WeakReference

  private var queue = Seq.empty[WeakReference[EventBus.Handler]]

  protected def broadcast(e: BusEvent) = queue foreach { r =>
    r.get map { h =>
      if (h.isDefinedAt(e)) if (h(e) == EventBus.Remove) this -= r
    } getOrElse { this -= r }
  }

  def clear() = queue = Seq.empty

  def send(e: BusEvent) = broadcast(e)

  // users of += must have trait EventBus.RefOwner
  def +=(handler: EventBus.Handler)(implicit owner: EventBus.Owner) {
    // long-lived objects that use EventBus must purge their owner list
    // keep the handler only for as long as the weak reference is valid
    owner.handlers = handler +: owner.handlers
    queue = new WeakReference(handler) +: queue
  }

  def size = queue.size

  // don't know the owner to remove it from  :-/
  private def -=(e: WeakReference[EventBus.Handler]) =
    queue = queue filterNot (_ == e)
}
object UiBus extends EventBus {

  lazy val handler = new Handler(Looper.getMainLooper)

  def post(f: => Unit) = handler.post(f _)

  def run(f: => Unit) = if (isMainThread) f else post(f)

  override def send(e: BusEvent) =
    if (isMainThread) broadcast(e) else post { broadcast(e) }
}
object ServiceBus extends EventBus
