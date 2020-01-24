package gscraper.gedcom

import java.io.File

import gscraper.genealogy._
import org.folg.gedcom._
import org.folg.gedcom.model.{ChildRef, Gedcom, SpouseRef}

object GedcomUtils {

  def toGedcom(tree: GenealogicalTree[String]): Gedcom = {

    var idCounter = 0
    var urlIdMap: Map[String, Int] = Map.empty

    def getOrCreateId(url: String): Int = {
      if(urlIdMap.contains(url)) {
        urlIdMap(url)
      } else {
        val allocated = idCounter
        urlIdMap += url -> allocated
        idCounter += 1
        allocated
      }
    }

    def idFormat(prefix: String, id: Int): String = f"$prefix$id%04d"

    def idIndForm(id: Int): String = idFormat("I", id)

    val gedcom = new Gedcom()
    val header = new model.Header()
    val charset = new model.CharacterSet()
    charset.setValue("UTF-8")
    header.setCharacterSet(charset)
    val generator = new model.Generator()
    generator.setValue("gscraper.geneanet-scraper")
    generator.setName("gscraper.geneanet-scraper")
    generator.setVersion("0.1")
    header.setGenerator(generator)
    gedcom.setHeader(header)

    def eventToTag(ev: Event): model.EventFact = {
      val event = new model.EventFact()
      event.setTag(ev.eventType match {
        case EventBirth => "BIRT"
        case EventDeath => "DEAT"
        case EventBaptism => "BAPT"
        case EventBurial => "BURI"
        case EventMarriage => "MARR"
      })
      ev.place.foreach{ pl =>
        event.setPlace(pl)
      }
      def modifierToTag(modifier: DateModifier): String = modifier match {
        case NoModifier => ""
        case AboutModifier => "ABT "
        case AfterModifier => "AFT "
        case BeforeModifier => "BEF "
        case MaybeModifier => "EST "
      }
      def monthToTag(m: Int): String = m match {
        case 1 => "JAN"
        case 2 => "FEB"
        case 3 => "MAR"
        case 4 => "APR"
        case 5 => "MAY"
        case 6 => "JUN"
        case 7 => "JUL"
        case 8 => "AUG"
        case 9 => "SEP"
        case 10 => "OCT"
        case 11 => "NOV"
        case 12 => "DEC"
      }
      event.setDate(ev.date match {
        case NoDate => ""
        case YearDate(year, modifier) => modifierToTag(modifier) + year
        case MonthYearDate(month, year, modifier) => modifierToTag(modifier) + monthToTag(month) + " " + year
        case FullDate(day, month, year, modifier) => modifierToTag(modifier) + day + " " + monthToTag(month) + " " + year
        case YearJulian(year, modifier) => "" // TODO
      })

      event
    }

    for(individual <- tree.persons) {
      val id = getOrCreateId(individual.id)
      val person = new model.Person()
      person.setId(idFormat("I", id))
      // sex
      val names = new model.Name()
      names.setGiven(individual.name)
      names.setSurname(individual.surname)
      person.addName(names)
      individual.occupation.foreach{ occ =>
        val occupation = new model.EventFact()
        occupation.setTag("OCCU")
        occupation.setValue(occ)
        person.addEventFact(occupation)
      }

      individual.events.foreach{ ev =>
        person.addEventFact(eventToTag(ev))
      }

      gedcom.addPerson(person)
    }

    var familyCounter = 0
    for(fam <- tree.families) {
      val family = new model.Family()
      val id = idFormat("F", familyCounter)
      familyCounter += 1
      family.setId(id)
      val (husband, wife) = (new SpouseRef(), new SpouseRef())
      urlIdMap.get(fam.father).foreach(id => husband.setRef(idIndForm(id)))
      urlIdMap.get(fam.mother).foreach(id => wife.setRef(idIndForm(id)))
      family.addHusband(husband)
      family.addWife(wife)
      for(ch <- fam.children; if urlIdMap.contains(ch)) {
        val child = new ChildRef()
        child.setRef(idIndForm(urlIdMap(ch)))
        family.addChild(child)
      }
      for(ev <- fam.events) {
        family.addEventFact(eventToTag(ev))
      }

      gedcom.addFamily(family)
    }

    gedcom
  }

  def writeGedcom(gedcom: Gedcom, file: String): Unit = {
    val writer = new visitors.GedcomWriter()
    writer.write(gedcom, new File(file))
  }

}
