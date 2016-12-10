package com.rokuan.calliopecore.fr.autoroute.data

import java.text.SimpleDateFormat
import java.util.{Calendar, Date, Locale}

import com.rokuan.calliopecore.fr.autoroute.sentence.{TimePreposition, Word}
import com.rokuan.calliopecore.sentence.ITimePreposition
import com.rokuan.calliopecore.sentence.structure.data.time.{SingleTimeObject, TimeAdverbial, TimePeriodObject}
import com.rokuan.calliopecore.sentence.structure.data.time.TimeAdverbial.{DateDefinition, TimeContext, TimeUnit}

/**
  * Created by Christophe on 27/11/2016.
  */
object DateConverter {
  import com.rokuan.calliopecore.fr.autoroute.pattern.WordRules._
  import com.rokuan.calliopecore.fr.autoroute.sentence.Word.WordType._
  import com.rokuan.autoroute.rules.Rule._

  private val NullDateValue = -1

  def parseRelativeSingleDate(dayStr: String) = {
    val oneDay = new SingleTimeObject() {
      dateDefinition = DateDefinition.DATE_ONLY
      date = parseRelativeDate(dayStr)
    }
    oneDay
  }

  def parseRelativeDate(dayStr: String) = {
    val calendar = Calendar.getInstance()

    val additionalDays =
      if(dayStr == "aujourd'hui"){
        0
      } else {
        dayStr.split("-").foldLeft(0) { case (acc, part) =>
          val bonus = part match {
            case "avant" | "hier" => -1
            case "après" | "demain" => 1
            case _ => 0
          }
          acc + bonus
        }
      }

    calendar.add(Calendar.DAY_OF_MONTH, additionalDays)
    calendar.getTime
  }

  def buildDateFromArray(dateArray: Array[Int]) = {
    val calendar = Calendar.getInstance()

    Option(dateArray(TimeUnit.YEAR.ordinal()))
      .collect { case v if v != NullDateValue => calendar.set(Calendar.YEAR, v) }
    Option(dateArray(TimeUnit.MONTH.ordinal()))
      .collect { case v if v != NullDateValue => calendar.set(Calendar.MONTH, v) }
    Option(dateArray(TimeUnit.DAY.ordinal()))
      .collect { case v if v != NullDateValue => calendar.set(Calendar.DAY_OF_MONTH, v) }

    calendar.set(Calendar.HOUR_OF_DAY,
      Option(dateArray(TimeUnit.HOURS.ordinal())).collect { case v if v != NullDateValue => v }.getOrElse(0))
    calendar.set(Calendar.MINUTE,
      Option(dateArray(TimeUnit.MINUTES.ordinal())).collect { case v if v != NullDateValue => v }.getOrElse(0))
    calendar.set(Calendar.SECOND,
      Option(dateArray(TimeUnit.SECONDS.ordinal())).collect { case v if v != NullDateValue => v }.getOrElse(0))

    calendar.getTime()
  }

  val TimePrepositionTransformer = word(TIME_PREPOSITION, CONTRACTED) {
    _.getTimePreposition
  } | (word(TIME_PREPOSITION) ~ word(DEFINITE_ARTICLE)) {
    case List(p: Word, _) => p.getTimePreposition
  }
  val MonthTransformer = word(DATE_MONTH) { w =>
    val monthDate = new SimpleDateFormat("MMMM", Locale.FRANCE).parse(w.getValue())
    val calendar = Calendar.getInstance()
    calendar.setTime(monthDate)
    calendar.get(Calendar.MONTH)
  }
  val NumberTransformer = word(NUMBER) { _.getValue.toInt }
  val PositionTransformer = word(NUMERICAL_POSITION) { w => CountConverter.parsePosition(w.getValue) }
  val DayTransformer = NumberTransformer | PositionTransformer
  val FromDate = (DayTransformer ~ (opt(MonthTransformer ~ opt(NumberTransformer)))) {
    case List(day: Int, Some(List(month: Int, Some(year: Int)))) =>
      val date = Array.fill[Int](TimeUnit.values().length)(NullDateValue)
      date(TimeUnit.DAY.ordinal()) = day
      date(TimeUnit.MONTH.ordinal()) = month
      date(TimeUnit.YEAR.ordinal()) = year
      buildDateFromArray(date)
    case List(day: Int, Some(List(month: Int, _))) =>
      val date = Array.fill[Int](TimeUnit.values().length)(NullDateValue)
      date(TimeUnit.DAY.ordinal()) = day
      date(TimeUnit.MONTH.ordinal()) = month
      buildDateFromArray(date)
    case List(day: Int, _) =>
      val date = Array.fill[Int](TimeUnit.values().length)(NullDateValue)
      date(TimeUnit.DAY.ordinal()) = day
      buildDateFromArray(date)
  }
  val RelativeDateTransformer = word(DATE) { w => parseRelativeDate(w.getValue()) }
  val RelativeDate = RelativeDateTransformer { d: Date =>
    new SingleTimeObject {
      dateDefinition = DateDefinition.DATE_ONLY
      date = d
    }
  }
  val ToDate = (DayTransformer ~ MonthTransformer ~ opt(NumberTransformer)) {
    case List(day: Int, month: Int, year: Option[Int]) =>
      val date = Array.fill[Int](TimeUnit.values().length)(-1)
      date(TimeUnit.DAY.ordinal()) = day
      date(TimeUnit.MONTH.ordinal()) = month
      year.foreach(y => date(TimeUnit.YEAR.ordinal()) = y)
      buildDateFromArray(date)
  }
  val DatePreposition = word(TIME_PREPOSITION, CONTRACTED) { w =>
    w.getTimePreposition
  } | (word(TIME_PREPOSITION) ~ word(DEFINITE_ARTICLE)) {
    case List(w: Word, _) => w.getTimePreposition
  }
  val FixedDate = (DatePreposition ~ ToDate) {
    case List(preposition: ITimePreposition, d: Date) =>
      val single = new SingleTimeObject() {
        dateDefinition = DateDefinition.DATE_ONLY
        date = d
      }
      single.setTimePreposition(preposition)
      single
  }
  val StartDate = (word(DEFINITE_ARTICLE) ~ FromDate) {
    case List(_, date: Date) => date
  } | RelativeDate
  val EndDate = (word(DEFINITE_ARTICLE) ~ ToDate) {
    case List(_, date: Date) => date
  } | RelativeDate
  val TimePrefix = (word("quand") ~ word("il") ~ word("sera")){
    _ => new TimePreposition("quand il sera", TimeContext.WHEN)
  } | word(PREPOSITION_AT) {
    _ => new TimePreposition("à", TimeContext.WHEN)
  } | word(TIME_PREPOSITION) { _.getTimePreposition }
  val FromToDate = (word(PREPOSITION_FROM) ~ (FromDate | RelativeDateTransformer) ~
    word(PREPOSITION_TO) ~ (ToDate | RelativeDateTransformer)) {
    case List(_, f: Date, _, t: Date) =>
      // TODO: adjust the month of "from" when only the date is given
      // TODO: add an additional parameter
      new TimePeriodObject(){
        fromDateDefinition = DateDefinition.DATE_ONLY
        toDateDefinition = DateDefinition.DATE_ONLY
        from = f
        to = t
      }
  }
  val BetweenDate = (word(PREPOSITION_BETWEEN) ~ StartDate ~ word(PREPOSITION_AND) ~ EndDate) {
    case List(_, f: Date, _, t: Date) =>
      // TODO: adjust the month of "from" when only the date is given
      // TODO: add an additional parameter
      new TimePeriodObject(){
        fromDateDefinition = DateDefinition.DATE_ONLY
        toDateDefinition = DateDefinition.DATE_ONLY
        from = f
        to = t
      }
  }
  val FullTimeRegex = "(\\d+)h(\\d+)"
  val HourOnlyRegex = "(\\d+)h"
  val FullTimeTransformer = word(FullTimeRegex) { w =>
    val reg = FullTimeRegex.r
    val reg(hour, minutes) = w.getValue
    val calendar = Calendar.getInstance()
    calendar.set(Calendar.HOUR_OF_DAY, hour.toInt)
    calendar.set(Calendar.MINUTE, minutes.toInt)
    calendar.getTime
  }
  val HourOnlyTransformer = word(HourOnlyRegex) { w =>
    val reg = HourOnlyRegex.r
    val reg(hour) = w.getValue
    val calendar = Calendar.getInstance()
    calendar.set(Calendar.HOUR_OF_DAY, hour.toInt)
    calendar.set(Calendar.MINUTE, 0)
    calendar.getTime
  }
  val TimeTransformer = (FullTimeTransformer | HourOnlyTransformer)
  val TimeDeclaration = (TimePrefix ~ TimeTransformer){
    case List(preposition: ITimePreposition, d: Date) =>
      new SingleTimeObject {
        dateDefinition = DateDefinition.TIME_ONLY
        date = d
      }
  }
  /*val DateWithPrepositionRule = (TimePrepositionTransformer ~ FromDate) {
    case List(prep: ITimePreposition, d: Date) =>
      val single = new SingleTimeObject {
        dateDefinition = DateDefinition.DATE_ONLY
        date = d
      }
      single.setTimePreposition(prep)
      single
  }*/

  // TODO: parse nominal date (ex: avant la date/le jour de mon mariage)
  val TimeAdverbialRule = FromToDate | BetweenDate |
    FixedDate | TimeDeclaration | RelativeDate /*| DateWithPrepositionRule*/
}
