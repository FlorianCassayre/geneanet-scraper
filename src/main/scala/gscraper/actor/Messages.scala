package gscraper.actor

import gscraper.geneanet.GeneanetUtils.GeneanetIndividual

object Messages {

  case class ScrapingRequest(url: String)
  case class ScrapingResult(url: String, result: GeneanetIndividual)
  case class ScrapingFailure(url: String, ex: Throwable)

}
