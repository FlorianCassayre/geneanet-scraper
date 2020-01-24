package gscraper.actor

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.routing.RoundRobinPool
import gscraper.actor.Messages.ScrapingRequest
import gscraper.genealogy.GenealogicalTree
import net.ruippeixotog.scalascraper.browser.JsoupBrowser

class GeneanetScraper private(bootstrapUrl: String, cookieValue: String, private[actor] val callback: GenealogicalTree[String] => Unit) {

  private[actor] val browser: JsoupBrowser = new JsoupBrowser()

  cookieValue.split(";\\w*").foreach{ s =>
    val Array(key, value) = s.split("=", 2)
    browser.setCookie("", key, value)
  }

  private val system = ActorSystem()

  private[actor] val scraperRouter: ActorRef = system.actorOf(Props(classOf[ActorScrape], this).withRouter(RoundRobinPool(nrOfInstances = 10)), name = "router")
  private[actor] val dataKeeper: ActorRef = system.actorOf(Props(classOf[ActorDataManager], this))

  dataKeeper ! ScrapingRequest(bootstrapUrl, 1)

}

object GeneanetScraper {
  def scrape(url: String, cookieValue: String)(callback: GenealogicalTree[String] => Unit): Unit = {
    new GeneanetScraper(url, cookieValue, callback)
  }
}