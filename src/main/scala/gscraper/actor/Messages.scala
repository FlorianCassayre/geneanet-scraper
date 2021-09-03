package gscraper.actor

import gscraper.geneanet.GeneanetUtils.GeneanetIndividual

object Messages {

  case class ScrapingRequest(url: String, relationship: String)
  case class ScrapingResult(url: String, result: GeneanetIndividual, relationship: String)
  case class ScrapingFailure(url: String, ex: Throwable)

}
