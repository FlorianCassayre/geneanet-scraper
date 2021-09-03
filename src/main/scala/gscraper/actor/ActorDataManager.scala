package gscraper.actor

import akka.actor.Actor
import gscraper.actor.Messages._
import gscraper.genealogy._

class ActorDataManager(scraper: GeneanetScraper) extends Actor {

  val TokenAncestor: Char = 'A'
  val TokenDescendant: Char = 'D'
  val TokenSpouse: Char = 'S'

  var pending = 0
  var forceExit = false

  // TODO create handler for that
  var personOrder: Seq[String] = Vector.empty
  var persons: Map[String, Person[String]] = Map.empty
  var familyOrder: Seq[(String, String)] = Vector.empty
  var families: Map[(String, String), Family[String]] = Map.empty

  var scraping: Set[String] = Set.empty

  private def forwardRequest(request: ScrapingRequest): Unit = {
    val id = request.url
    val matches = request.relationship match {
      case scraper.pathMatcher(_*) => true
      case _ => false
    }
    if(!persons.contains(id) && !scraping.contains(id) && matches) { // persons.size <= 100
      pending += 1
      scraping += id
      scraper.scraperRouter ! request
    }
  }

  override def receive: Receive = {
    case request: ScrapingRequest => // Initial and subsequent requests
      forwardRequest(request)

    case ScrapingResult(url, result, relationship) => // Callback
      println(s"[I: ${persons.size}, F: ${families.size}, I': ${pending}] Received ${result.person}")
      pending -= 1
      scraping -= url

      personOrder :+= url
      persons += url -> result.person

      val familyOpt = if(result.fatherUrl.isDefined && result.motherUrl.isDefined) Seq(Family(result.fatherUrl.get, result.motherUrl.get, Seq.empty, Seq(url))) else Seq.empty

      for(family <- result.families ++ familyOpt) { // TODO
        val key = (family.father, family.mother)
        if(families.contains(key)) { // Merge
          val other = families(key)
          families += key -> other.copy(events = other.events ++ family.events.diff(other.events), children = other.children ++ family.children.diff(other.children))
        } else {
          families += key -> family
          familyOrder :+= key
        }
      }

      Seq(result.fatherUrl, result.motherUrl).flatten.foreach(url => forwardRequest(ScrapingRequest(url, relationship + TokenAncestor)))

      result.children.foreach(url => forwardRequest(ScrapingRequest(url, relationship + TokenDescendant)))

      // Note that spouses could be ancestors or descendants too, but in practice this should rarely occur because
      // we visit them at the end.
      result.families.flatMap(fam => Seq(fam.father, fam.mother)).foreach(url => forwardRequest(ScrapingRequest(url, relationship + TokenSpouse)))

      checkDone()
    case fail: ScrapingFailure =>
      forceExit = true // Force program exit
      pending -= 1

      println("FAIL: " + fail.url)
      fail.ex.printStackTrace()

      checkDone()
  }

  var done = false

  def checkDone(): Unit = {
    if(pending == 0 || forceExit) { // Job done
      if(done) {
        throw new IllegalStateException("Done was called twice")
      }
      done = true

      val tree = GenealogicalTree(personOrder.map(persons), familyOrder.map(families))

      scraper.callback(tree)

      println("Job done. Exiting.")
      System.exit(0) // TODO
    }
  }

}
