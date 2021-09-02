package gscraper.actor

import akka.actor.Actor
import gscraper.actor.Messages._
import gscraper.geneanet.GeneanetUtils
import net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}

import scala.util.{Failure, Success, Try}

class ActorScrape(scraper: GeneanetScraper) extends Actor {

  implicit val browser: JsoupBrowser = scraper.browser

  override def receive: Receive = {
    case request: ScrapingRequest =>
      Try(GeneanetUtils.scrape(request.url, scraper.buildRequest)) match {
        case Success(result) => scraper.dataKeeper ! ScrapingResult(request.url, result)
        case Failure(ex) => scraper.dataKeeper ! ScrapingFailure(request.url, ex)
      }
  }

}
