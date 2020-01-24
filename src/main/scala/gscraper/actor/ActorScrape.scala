package gscraper.actor

import akka.actor.Actor
import gscraper.actor.Messages._
import gscraper.geneanet.GeneanetUtils
import net.ruippeixotog.scalascraper.browser.Browser

import scala.util.{Failure, Success, Try}

class ActorScrape(implicit browser: Browser) extends Actor {

  override def receive: Receive = {
    case request: ScrapingRequest =>
      Try(GeneanetUtils.scrape(request.url)) match {
        case Success(result) => Actors.dataKeeper ! ScrapingResult(request.url, result, request.sosa)
        case Failure(ex) => Actors.dataKeeper ! ScrapingFailure(request.url, ex)
      }
  }

}
