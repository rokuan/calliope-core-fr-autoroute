package com.rokuan.calliopecore.fr.autoroute.data

/**
  * Created by Christophe on 27/11/2016.
  */
object DateConverter {
  import com.rokuan.calliopecore.fr.autoroute.pattern.WordRules._
  import com.rokuan.calliopecore.fr.sentence.Word.WordType._
  import com.rokuan.autoroute.rules.Rule._

  val Day = (word(NUMBER) { _.getValue.toInt} | word(NUMERICAL_POSITION){ w => CountConverter.parsePosition(w.getValue) })
  val FromDate = (Day ~ (opt(word(DATE_MONTH) ~ opt(word(NUMBER))))) {
      case List(day: Int, Some(List(month: Int, Some(year: Int)))) =>

      case List(day: Int, Some(List(month: Int, _))) =>

      case List(day: Int, _) =>
    }
  val ToDate = (Day ~ word(DATE_MONTH) ~ opt(word(NUMBER))) {
    case List(day: Int, month: Int, year: Option[Int]) =>
  }
  val FromToDate = word(PREPOSITION_FROM)
}
