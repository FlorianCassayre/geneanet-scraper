package gscraper.actor

import akka.actor.Actor
import gscraper.actor.Messages._
import gscraper.genealogy._

class ActorDataManager(scraper: GeneanetScraper) extends Actor {

  var pending = 0
  var forceExit = false

  val visitAncestors = true
  val visitDescendants = true
  val visitSpouse = true

  // TODO create handler for that
  var personOrder: Seq[String] = Vector.empty
  var persons: Map[String, Person[String]] = Map.empty
  var familyOrder: Seq[(String, String)] = Vector.empty
  var families: Map[(String, String), Family[String]] = Map.empty

  var scraping: Set[String] = Set.empty

  private def forwardRequest(request: ScrapingRequest): Unit = {
    val id = request.url
    if(!persons.contains(id) && !scraping.contains(id)) { // persons.size <= 100
      pending += 1
      scraping += id
      scraper.scraperRouter ! request
    }
  }

  override def receive: Receive = {
    case request: ScrapingRequest => // Initial and subsequent requests
      forwardRequest(request)

    case result: ScrapingResult => // Callback
      println(s"[I: ${persons.size}, F: ${families.size}, I': ${pending}] Received ${result.result.person}")
      pending -= 1
      scraping -= result.url

      personOrder :+= result.url
      persons += result.url -> result.result.person

      val familyOpt = if(result.result.fatherUrl.isDefined && result.result.motherUrl.isDefined) Seq(Family(result.result.fatherUrl.get, result.result.motherUrl.get, Seq.empty, Seq(result.url))) else Seq.empty

      for(family <- result.result.families ++ familyOpt) { // TODO
        val key = (family.father, family.mother)
        if(families.contains(key)) { // Merge
          val other = families(key)
          families += key -> other.copy(events = other.events ++ family.events.diff(other.events), children = other.children ++ family.children.diff(other.children))
        } else {
          families += key -> family
          familyOrder :+= key
        }
      }

      val res = result.result
      if(visitAncestors) {
        Seq(res.fatherUrl, res.motherUrl).flatten.foreach(url => forwardRequest(ScrapingRequest(url)))
      }

      if(visitDescendants) {
        res.children.foreach(url => forwardRequest(ScrapingRequest(url)))
      }

      if(visitSpouse) {
        res.families.flatMap(fam => Seq(fam.father, fam.mother)).foreach(url => forwardRequest(ScrapingRequest(url)))
      }

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
