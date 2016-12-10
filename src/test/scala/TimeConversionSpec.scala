import java.text.SimpleDateFormat

import com.rokuan.autoroute.matchers.Route
import com.rokuan.calliopecore.fr.autoroute.data.DateConverter
import com.rokuan.calliopecore.fr.autoroute.sentence.{TimePreposition, Word, WordInfo}
import com.rokuan.calliopecore.fr.autoroute.sentence.Word.WordType._
import com.rokuan.calliopecore.sentence.structure.data.time.TimeAdverbial.{DateDefinition, TimeContext, TimeType}
import com.rokuan.calliopecore.sentence.structure.data.time.{SingleTimeObject, TimePeriodObject}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Christophe on 06/12/2016.
  */
class TimeConversionSpec extends FlatSpec with Matchers {
  val parser = new Route(DateConverter.TimeAdverbialRule)
  val dateOnlyFormat = new SimpleDateFormat("dd/MM/yyyy")
  val timeOnlyFormat = new SimpleDateFormat("HH:mm")

  "1" should "2" in {
    val words = List(
      new Word("du", PREPOSITION_FROM),
      new Word("1", NUMBER),
      new Word("janvier", DATE_MONTH),
      new Word("2012", NUMBER),
      new Word("au", PREPOSITION_TO),
      new Word("7", NUMBER),
      new Word("mars", DATE_MONTH),
      new Word("2015", NUMBER)
    )

    val dateObject = parser(words)
    dateObject.getTimeType shouldBe TimeType.PERIOD
    val timePeriodObject = dateObject.asInstanceOf[TimePeriodObject]

    dateOnlyFormat.format(timePeriodObject.from) shouldBe "01/01/2012"
    dateOnlyFormat.format(timePeriodObject.to) shouldBe "07/03/2015"
  }

  "2" should "3" in {
    val words = List(
      new Word("entre", PREPOSITION_BETWEEN),
      new Word("le", DEFINITE_ARTICLE),
      new Word("1er", NUMERICAL_POSITION),
      new Word("mai", DATE_MONTH),
      new Word("2012", NUMBER),
      new Word("et", PREPOSITION_AND),
      new Word("le", DEFINITE_ARTICLE),
      new Word("4", NUMBER),
      new Word("septembre", DATE_MONTH),
      new Word("2013", NUMBER)
    )

    val dateObj = parser(words)
    dateObj.getTimeType() shouldBe TimeType.PERIOD
    val period = dateObj.asInstanceOf[TimePeriodObject]

    dateOnlyFormat.format(period.from) shouldBe "01/05/2012"
    dateOnlyFormat.format(period.to) shouldBe "04/09/2013"
  }

  "3" should "4" in {
    val words = List(
      new Word("à", PREPOSITION_AT),
      new Word("5h30", TIME)
    )

    val dateObj = parser(words)
    dateObj.getTimeType() shouldBe TimeType.SINGLE
    val fixedTime = dateObj.asInstanceOf[SingleTimeObject]
    fixedTime.dateDefinition shouldBe DateDefinition.TIME_ONLY
    timeOnlyFormat.format(fixedTime.date) shouldBe "05:30"
  }

  "4" should "5" in {
    val words = List(
      new Word("à", PREPOSITION_AT),
      new Word("5h", TIME)
    )

    val dateObj = parser(words)
    dateObj.getTimeType() shouldBe TimeType.SINGLE
    val fixedTime = dateObj.asInstanceOf[SingleTimeObject]
    fixedTime.dateDefinition shouldBe DateDefinition.TIME_ONLY
    timeOnlyFormat.format(fixedTime.date) shouldBe "05:00"
  }

  "8" should "9" in {
    val until = new Word("jusqu'au",
      wordInfo = new WordInfo("jusqu'au", CONTRACTED),
      timePreposition = new TimePreposition("jusqu'au", TimeContext.UNTIL))
    val words = List(
      until,
      new Word("1er", NUMERICAL_POSITION),
      new Word("mai", DATE_MONTH),
      new Word("2012", NUMBER)
    )
    val dateObject = parser(words)
    dateObject.getTimeType shouldBe TimeType.SINGLE
    val single = dateObject.asInstanceOf[SingleTimeObject]
    single.getTimePreposition.getContext shouldBe TimeContext.UNTIL
  }
}
