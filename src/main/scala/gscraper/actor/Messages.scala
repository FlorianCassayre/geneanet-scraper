package gscraper.actor

import gscraper.geneanet.GeneanetUtils.GeneanetIndividual

object Messages {

  case class ScrapingRequest(url: String, sosa: Int)
  case class ScrapingResult(url: String, result: GeneanetIndividual, sosa: Int)
  case class ScrapingFailure(url: String, ex: Throwable)

}
