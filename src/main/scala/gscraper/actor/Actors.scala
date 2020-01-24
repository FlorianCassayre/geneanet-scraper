package gscraper.actor

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.routing.RoundRobinPool
import net.ruippeixotog.scalascraper.browser.JsoupBrowser

object Actors {

  implicit val browser: JsoupBrowser = new JsoupBrowser()

  browser.setCookie("", "", "?")


  val system = ActorSystem()

  val scraperRouter: ActorRef = system.actorOf(Props(classOf[ActorScrape], browser).withRouter(RoundRobinPool(nrOfInstances = 10)), name = "router")
  val dataKeeper: ActorRef = system.actorOf(Props(classOf[ActorDataManager]))

}
