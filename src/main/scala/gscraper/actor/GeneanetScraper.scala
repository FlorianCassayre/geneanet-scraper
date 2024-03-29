package gscraper.actor

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.routing.RoundRobinPool
import gscraper.actor.Messages.ScrapingRequest
import gscraper.genealogy.GenealogicalTree
import net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}
import scalaj.http.{Http, HttpRequest}

import collection.JavaConverters._
import java.net.HttpCookie
import scala.util.matching.Regex

class GeneanetScraper private(bootstrapUrl: String, cookieValue: String, userAgent: String, val pathMatcher: Regex, private[actor] val callback: GenealogicalTree[String] => Unit) {

  private[actor] val browser: JsoupBrowser = new JsoupBrowser(userAgent)
  private[actor] def buildRequest(url: String): HttpRequest =
    Http(url)
      .header("Cookie", cookieValue)
      .header("User-Agent", userAgent)

  private val system = ActorSystem()

  private[actor] val scraperRouter: ActorRef = system.actorOf(Props(classOf[ActorScrape], this).withRouter(RoundRobinPool(nrOfInstances = 1)), name = "router")
  private[actor] val dataKeeper: ActorRef = system.actorOf(Props(classOf[ActorDataManager], this))

  dataKeeper ! ScrapingRequest(bootstrapUrl, "")

}

object GeneanetScraper {
  def scrape(url: String, cookieValue: String, userAgent: String, pathMatcher: Regex)(callback: GenealogicalTree[String] => Unit): Unit = {
    new GeneanetScraper(url, cookieValue, userAgent, pathMatcher, callback)
  }
}
