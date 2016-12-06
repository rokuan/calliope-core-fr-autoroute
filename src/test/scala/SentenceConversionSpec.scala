import com.rokuan.autoroute.matchers.Route
import com.rokuan.calliopecore.fr.autoroute.data.InterpretationObjectConverter
import com.rokuan.calliopecore.fr.sentence.Verb.ConjugationTense
import com.rokuan.calliopecore.fr.sentence._
import com.rokuan.calliopecore.fr.sentence.Word.WordType._
import com.rokuan.calliopecore.sentence.IAction.{ActionType, Form}
import com.rokuan.calliopecore.sentence.structure.InterpretationObject.RequestType
import com.rokuan.calliopecore.sentence.structure.QuestionObject
import com.rokuan.calliopecore.sentence.structure.QuestionObject.QuestionType
import com.rokuan.calliopecore.sentence.structure.data.place.NamedPlaceObject
import com.rokuan.calliopecore.sentence.structure.data.place.PlaceAdverbial.{PlaceContext, PlaceType}
import com.rokuan.calliopecore.sentence.structure.data.way.TransportObject
import com.rokuan.calliopecore.sentence.structure.data.way.TransportObject.TransportType
import com.rokuan.calliopecore.sentence.structure.data.way.WayAdverbial.{WayContext, WayType}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Christophe on 06/12/2016.
  */
class SentenceConversionSpec extends FlatSpec with Matchers {
  val parser = new Route(InterpretationObjectConverter.InterpretationObjectRule)

  "1" should "2" in {
    val go = new Word("aller", VERB, COMMON_NAME)
    val toGo = new Verb("aller", false, new Action(ActionType.GO))
    val toGoConjug = new VerbConjugation(ConjugationTense.PRESENT, Form.INFINITIVE, null, "aller", toGo)
    toGoConjug.setVerb(toGo)
    go.setVerbInfo(toGoConjug)
    val to = new Word("à", PLACE_PREPOSITION)
    to.setPlacePreposition(new PlacePreposition("à", PlaceContext.TO, PlaceType.NAMED_PLACE, PlaceType.CITY))
    val by = new Word("en", WAY_PREPOSITION)
    val car = new Word("voiture", COMMON_NAME, MEAN_OF_TRANSPORT)

    by.setWayPreposition(new WayPreposition("en", WayContext.BY, WayType.TRANSPORT))
    car.setTransportInfo(new TransportInfo("voiture", TransportType.CAR))

    val words = List(
      new Word("comment", INTERROGATIVE_PRONOUN),
      go,
      to,
      new Word("la", DEFINITE_ARTICLE),
      new Word("Mairie", PLACE_TYPE, PROPER_NAME, COMMON_NAME),
      new Word("de", PREPOSITION_OF),
      new Word("Paris", PROPER_NAME, CITY),
      by,
      car
    )

    val obj = parser(words)
    obj.getRequestType shouldBe RequestType.QUESTION
    obj.action.getMainAction.getAction shouldBe ActionType.GO

    val question = obj.asInstanceOf[QuestionObject]
    question.questionType shouldBe QuestionType.HOW

    val monument = obj.where.asInstanceOf[NamedPlaceObject]
    monument.name shouldBe "Mairie de Paris"
    val transport = obj.how.asInstanceOf[TransportObject]
    transport.transportType shouldBe TransportType.CAR
  }
}
