package gscraper

import gscraper.actor.GeneanetScraper

object Main extends App {

  val cookieValue = ""
  val url = "https://gw.geneanet.org/gntstarmacrone?lang=fr&pz=emmanuel+jean+michel+frederic&nz=macron&p=emmanuel+jean+michel+frederic&n=macron"

  GeneanetScraper.scrape(url, cookieValue){ tree =>
    import gscraper.gedcom.GedcomUtils._

    writeGedcom(toGedcom(tree), "test3.ged")
  }

}
