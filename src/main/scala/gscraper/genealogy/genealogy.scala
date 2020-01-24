package gscraper

package object genealogy {

  sealed abstract class Sex {
    def isMale: Boolean = this == SexMale
    def isFemale: Boolean = this == SexFemale
    def isKnown: Boolean = this != SexUnknown
    override def toString: String = this match {
      case SexMale => "M"
      case SexFemale => "F"
      case SexUnknown => "?"
    }
  }
  case object SexMale extends Sex
  case object SexFemale extends Sex
  case object SexUnknown extends Sex

  sealed abstract class DateModifier {
    override def toString: String = this match {
      case NoModifier => ""
      case AboutModifier => "~"
      case AfterModifier => "after "
      case BeforeModifier => "before "
      case MaybeModifier => "maybe "
    }
  }
  case object NoModifier extends DateModifier
  case object AboutModifier extends DateModifier
  case object AfterModifier extends DateModifier
  case object BeforeModifier extends DateModifier
  case object MaybeModifier extends DateModifier

  sealed abstract class Date {
    override def toString: String = this match {
      case NoDate => "?"
      case YearDate(year, modifier) => s"$modifier$year"
      case MonthYearDate(month, year, modifier) => s"$modifier$month/$year"
      case FullDate(day, month, year, modifier) => s"$modifier$day/$month/$year"
      case YearJulian(year, modifier) => s"$year julian"
    }
  }
  case object NoDate extends Date
  case class YearDate(year: Int, modifier: DateModifier = NoModifier) extends Date
  case class MonthYearDate(month: Int, year: Int, modifier: DateModifier = NoModifier) extends Date
  case class FullDate(day: Int, month: Int, year: Int, modifier: DateModifier = NoModifier) extends Date
  case class YearJulian(year: Int, modifier: DateModifier = NoModifier) extends Date

  type Place = String

  case class Event(eventType: EventType, date: Date, place: Option[Place])

  sealed trait WithEvents {
    val events: Seq[Event]
    def event(eventType: EventType): Option[Event] = events.find(_.eventType == eventType)
  }

  case class Person(name: String, surname: String, sex: Sex, events: Seq[Event], occupation: Option[String]) extends WithEvents {
    def birth: Option[Event] = event(EventBirth)
    def birthOrBaptism: Option[Event] = birth.orElse(event(EventBaptism))
    def death: Option[Event] = event(EventDeath)
  }

  case class Family[I](father: I, mother: I, events: Seq[Event], children: Seq[I]) extends WithEvents {
    def marriage: Option[Event] = event(EventMarriage)
  }

  sealed abstract class EventType
  case object EventBirth extends EventType
  case object EventDeath extends EventType
  case object EventBaptism extends EventType
  case object EventBurial extends EventType
  case object EventMarriage extends EventType

}
