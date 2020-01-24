package gscraper

import gscraper.actor.Messages.ScrapingRequest
import gscraper.actor.Actors

object Main extends App {

  // Initial message
  Actors.dataKeeper ! ScrapingRequest("https://gw.geneanet.org/gntstarmacrone?lang=fr&pz=emmanuel+jean+michel+frederic&nz=macron&p=emmanuel+jean+michel+frederic&n=macron", 1)

}
