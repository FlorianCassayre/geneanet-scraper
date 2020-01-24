package gscraper.actor

import java.io.File

import akka.actor.Actor
import gscraper.actor.Messages._
import gscraper.genealogy._
import org.folg.gedcom.model.{ChildRef, Gedcom, SpouseRef}

class ActorDataManager(scraper: GeneanetScraper) extends Actor {

  var pending = 0

  // TODO create handler for that
  var personOrder: Seq[String] = Vector.empty
  var persons: Map[String, Person[String]] = Map.empty
  var familyOrder: Seq[(String, String)] = Vector.empty
  var families: Map[(String, String), Family[String]] = Map.empty

  private def forwardRequest(request: ScrapingRequest): Unit = {
    if(!persons.contains(request.url) && persons.size <= 100) {
      pending += 1
      scraper.scraperRouter ! request
    }
  }

  override def receive: Receive = {
    case request: ScrapingRequest => // Initial and subsequent requests
      forwardRequest(request)

    case result: ScrapingResult => // Callback
      println("[" + pending + "] Received " + result.sosa + "\t" + result.result.person)
      pending -= 1

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

      result.result.fatherUrl.foreach(url => forwardRequest(ScrapingRequest(url, result.sosa * 2)))
      result.result.motherUrl.foreach(url => forwardRequest(ScrapingRequest(url, result.sosa * 2 + 1)))

      checkDone()
    case fail: ScrapingFailure =>
      pending -= 1

      println("FAIL: " + fail.url)
      fail.ex.printStackTrace()

      checkDone()
  }

  var done = false

  def checkDone(): Unit = {
    if(pending == 0) { // Job done
      if(done) {
        throw new IllegalStateException("Done was called twice")
      }
      done = true

      val tree = GenealogicalTree(personOrder.map(persons), familyOrder.map(families))

      scraper.callback(tree)

      System.exit(0) // TODO
    }
  }

}
