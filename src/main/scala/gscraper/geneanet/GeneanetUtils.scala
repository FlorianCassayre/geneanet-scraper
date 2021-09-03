package gscraper.geneanet

import gscraper.genealogy._
import net.ruippeixotog.scalascraper.browser.Browser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.model._
import scalaj.http.HttpRequest
import spray.json._
import DefaultJsonProtocol._

import scala.util.{Failure, Success, Try}

object GeneanetUtils {

  type Id = String // A hopefully unique URL, for now

  case class GeneanetIndividual(person: Person[Id], families: Seq[Family[Id]], fatherUrl: Option[Id], motherUrl: Option[Id], children: Seq[Id])

  def scrape(url: String, requestBuilder: String => HttpRequest)(implicit browser: Browser): GeneanetIndividual = {
    def withRetry[T](n: Int, wait: Long, coefficient: Long)(f: () => T): T = {
      Try(f()) match {
        case Success(value) => value
        case Failure(exception) =>
          if(n > 0) {
            println("Failure:")
            exception.printStackTrace()
            println(s"Retrying in ${wait}ms...")
            Thread.sleep(wait)
            withRetry(n - 1, wait * coefficient, coefficient)(f)
          } else {
            throw new Exception(exception)
          }
      }
    }

    val response = withRetry(5, 1000, 2)(() => requestBuilder(url).asString)
    val doc = browser.parseString(response.body)

    // TODO (doc >?> element("div#person-title")).isEmpty

    //println(doc)
    val blacklistTitle = (doc >> elementList("h1")).find(e => e.text == "Navigation inhabituelle")
    if(blacklistTitle.nonEmpty) {
      val form = blacklistTitle.get.parent >> element(".panel > .row > div > form")
      println(form.get.outerHtml)
      throw new Exception("Captcha")
    }

    val personTitleElement = doc >> element("div#person-title")
    val personGeneralElement = personTitleElement >> element("div.columns > h1")

    val columnElement = (doc >> element("h2 > span")).parent.get.parent.get

    val sex = personGeneralElement >> attr("title")("img") match {
      case "H" => SexMale
      case "F" => SexFemale
      case "?" => SexUnknown
    }

    val (name: String, surname: String) = (personGeneralElement >> elementList("a:not(.edit-button-action)")).map(_.text) match {
      case List(first, second) => (first, second)
      case _ =>
        (personTitleElement.parent.get >> elementList("em")).find(e => (e >> elementList("a")).lengthCompare(2) == 0) match {
          case Some(e) =>
            (e >> texts("a")).toList match {
              case List(first, second) => (first, second)
            }
        }
    }

    val sosa = (doc >?> text("em.sosa > a")).map(_.replace(" ", "").toInt)

    val informationUlElement = columnElement >> element("ul")
    val informationElements = (informationUlElement >> elementList("li")).map(_.text)

    val categoryTitleElements = (columnElement >> elementList("div:not(#block-media) > h2 > span:not(.htitle)")).map(_.text)

    val categoryContent = (columnElement >> elementList("ul")).filter(_.parent.get.tagName == "div").filter(_ != informationUlElement)

    val categoryZipped = categoryTitleElements.zip(categoryContent)

    val (fatherUrl: Option[String], motherUrl: Option[String]) = categoryZipped.find(_._1 == "Parents").map(_._2).map{ e =>
      def urlOption(li: Element): Option[String] = (li >?> attr("href")("a")).map(absoluteUrl) // TODO deprecate
      (e >> elementList("li")).map(firstPersonLink) match {
        case List(father, mother) => (Some(father), Some(mother))
      }
    }.getOrElse((None, None))

    // TODO
    val (unions, childrenSub) = categoryZipped.find(_._1.startsWith("Union(s)")).map(_._2).toSeq.flatMap { e =>
      val lis = e >> elementList("ul.fiche_union > li")
      lis.map{ li =>
        // TODO divorce
        def isEmFirst(e: Element): Boolean = {
          val parent = e.parent.get.parent.get
          parent.hasAttr("class") && parent.attr("class").contains("fiche_union")
        }
        val eventOpt = li.text.split(" ", 2).head.stripSuffix(",") match {
          case "Marié" | "Mariée" | "Fiancé" | "Fiancée" | "Relation" | "Contrat" => Some((li >?> element(":not(a) > em")).filter(isEmFirst).map(_.text).map(mar => parseEventRaw(mar, ", ", EventMarriage)).getOrElse(Event(EventMarriage, NoDate, None)))
          case "Avec" => None
        }
        val spouse = firstPersonLink(li)
        val (father, mother) = if(sex.isMale) (url, spouse) else (spouse, url) // FIXME dirty url

        val children = (li >> elementList(":not(div) > ul > li")).map(firstPersonLink)

        (Family(father, mother, eventOpt.toSeq, children), children) // We don't include children yet (thus Seq.empty)
      }
    }.unzip

    val events = informationElements.map(str => str -> parseEventIndividual(str))
    val eventsFlat = events.flatMap(_._2)
    val otherInformations = {
      val filtered = events.filter(_._2.isEmpty).map(_._1).map(_.trim).filter(_.nonEmpty).filter(!_.startsWith("Âge"))
      if(filtered.nonEmpty) {
        Some(filtered.mkString(", "))
      } else {
        None
      }
    }

    val jsonMetadata = (doc >> elementList("script")).map(_.innerHtml).filter(_.contains("MODE_VISITOR")).head
      .split("elements, ")(1).split("\\);").head
      .parseJson.asJsObject
    val medias = jsonMetadata.fields("gntGeneweb").asJsObject.fields.get("media").map(_.convertTo[Seq[JsObject]].map { mediaData =>
      val srcRaw = mediaData.fields("src").convertTo[String]
      assert(srcRaw.startsWith("//gw.geneanet.org/") && srcRaw.matches(".*/medium\\.(jpg|JPG|png|PNG)\\?t=\\d+$"), srcRaw) // Hackish
      val src = "https:" + srcRaw.split("\\?").head.replace("/medium.", "/normal.")
      Media(mediaData.fields("title").convertTo[String], src)
    }).toSeq.flatten

    val person = Person(url, name, surname, sex, eventsFlat, otherInformations, medias)

    GeneanetIndividual(person, unions, fatherUrl, motherUrl, childrenSub.flatten)
  }

  def absoluteUrl(relative: String): String = s"https://gw.geneanet.org/$relative"

  def parseEventIndividual(string: String): Option[Event] = {
    def parseEventType(str: String): Option[EventType] = str match {
      case "Né" | "Née" | "Né(e)" => Some(EventBirth)
      case "Décédé" | "Décédée" | "Décédé(e)" => Some(EventDeath)
      case "Baptisé" | "Baptisée" | "Baptisé(e)" => Some(EventBaptism)
      case "Inhumé" | "Inhumée" | "Inhumé(e)" => Some(EventBurial)
      case _ => None
    }

    string.split(" ", 2).toSeq match {
      case Seq(key) => parseEventType(key).map(eventType => Event(eventType, NoDate, None))
      case Seq(key, rest) =>
        parseEventType(key).map(eventType => parseEventRaw(rest, "\\w*- ", eventType))
      case _ => None
    }
  }

  def firstPersonLink(elem: Element): String = absoluteUrl((elem >> elementList("a:not(.edit-fam-button-action)")).find(e => (e >?> element("img[alt=sosa]")).isEmpty).map(_.attr("href")).get)


  def parseEventRaw(string: String, separator: String, eventType: EventType): Event = {
    def parseDateComplete(str: String): Date = {
      if(str.startsWith("entre")) { // Not supported yet TODO
        NoDate
      } else if (str.startsWith("(") && str.endsWith(")")) { // Unrecognized date
        NoDate
      } else {
        val months = Seq("janvier", "février", "mars", "avril", "mai", "juin", "juillet", "août", "septembre", "octobre", "novembre", "décembre")
        def dayToInt(day: String): Int = day match {
          case "1er" => 1
          case _ => day.toInt
        }
        str.split(" ", 2).toSeq match {
          case Seq("") => NoDate
          case Seq(keyword, dateRest) =>
            def parseDate(dateOnly: String, modifier: DateModifier): Date = dateOnly.split(" ").toSeq match {
              case Seq(yearStr, "julien") => YearJulian(yearStr.toInt)
              case Seq(yearStr) => YearDate(yearStr.toInt, modifier)
              case Seq(monthStr, yearStr) if months.contains(monthStr) => MonthYearDate(months.indexOf(monthStr) + 1, yearStr.toInt, modifier)
              case Seq(dayStr, monthStr, yearStr) if months.contains(monthStr) => FullDate(dayToInt(dayStr), months.indexOf(monthStr) + 1, yearStr.toInt, modifier)
              case _ => // Possible republican
                dateOnly.split("\\(", 2).map(_.split("\\)", 2).toSeq).toSeq match {
                  case Seq(_, Seq(normal, _)) => parseDate(normal, modifier)
                }
            }
            val modifier = keyword match {
              case "le" => NoModifier // day-month-year
              case "en" => NoModifier // month-year OR year
              case "avant" => BeforeModifier // day-month-year OR month-year OR year
              case "après" => AfterModifier // day-month-year OR month-year OR year
              case "vers" => AboutModifier // year OR ???
              case "peut-être" => MaybeModifier
            }
            parseDate(dateRest.stripPrefix("en "), modifier)
        }
      }
    }

    val ageSuffix = ", à l'âge d" // [e OR 'environ]
    string.split(separator, 2).toSeq match {
      case Seq(dateStr) => Event(eventType, parseDateComplete(dateStr.split(ageSuffix, 2).head), None)
      case Seq(dateStr, placeOther) =>
        val date = parseDateComplete(dateStr)
        val place = placeOther.split(ageSuffix, 2).head.trim.stripSuffix(",")

        Event(eventType, date, Some(place))
    }
  }


}
