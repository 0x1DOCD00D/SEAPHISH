package Agents

import akka.actor.{Actor, ActorLogging}

class AppStore extends Actor with ActorLogging {
  override def receive: Receive = {
    case m => log.info(s"Received unknown message: $m")
  }
}

