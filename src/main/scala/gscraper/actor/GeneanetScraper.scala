package gscraper.actor

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.routing.RoundRobinPool
import gscraper.actor.Messages.ScrapingRequest
import gscraper.genealogy.GenealogicalTree
import net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}
import scalaj.http.{Http, HttpRequest}
import collection.JavaConverters._

import java.net.HttpCookie

class GeneanetScraper private(bootstrapUrl: String, cookieValue: String, userAgent: String, private[actor] val callback: GenealogicalTree[String] => Unit) {

  private[actor] val browser: JsoupBrowser = new JsoupBrowser(userAgent)
  private[actor] def buildRequest(url: String): HttpRequest =
    Http(url)
      .cookies(HttpCookie.parse(cookieValue).asScala)
      .header("User-Agent", userAgent)

  cookieValue.split(";\\w*").foreach{ s =>
    val List(key, value) = s.split("=", 2).toList
    browser.setCookie("", key, value)
  }

  private val system = ActorSystem()

  private[actor] val scraperRouter: ActorRef = system.actorOf(Props(classOf[ActorScrape], this).withRouter(RoundRobinPool(nrOfInstances = 1)), name = "router")
  private[actor] val dataKeeper: ActorRef = system.actorOf(Props(classOf[ActorDataManager], this))

  dataKeeper ! ScrapingRequest(bootstrapUrl)

}

object GeneanetScraper {
  def scrape(url: String, cookieValue: String, userAgent: String)(callback: GenealogicalTree[String] => Unit): Unit = {
    new GeneanetScraper(url, cookieValue, userAgent, callback)
  }
}
