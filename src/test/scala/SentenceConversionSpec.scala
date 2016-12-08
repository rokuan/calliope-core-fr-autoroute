import com.rokuan.autoroute.matchers.Route
import com.rokuan.calliopecore.fr.autoroute.data.InterpretationObjectConverter
import com.rokuan.calliopecore.fr.sentence.Verb.ConjugationTense
import com.rokuan.calliopecore.fr.sentence._
import com.rokuan.calliopecore.fr.sentence.Word.WordType._
import com.rokuan.calliopecore.sentence.IAction.{ActionType, Form}
import com.rokuan.calliopecore.sentence.structure.InterpretationObject.RequestType
import com.rokuan.calliopecore.sentence.structure.QuestionObject
import com.rokuan.calliopecore.sentence.structure.QuestionObject.QuestionType
import com.rokuan.calliopecore.sentence.structure.data.nominal.AdditionalPerson
import com.rokuan.calliopecore.sentence.structure.data.nominal.NominalGroup.GroupType
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

  "2" should "3" in {
    val be = new Word("est", VERB, AUXILIARY)
    val toBe = new Verb("être", true, new Action(ActionType.BE))
    val toBeConjug = new VerbConjugation(ConjugationTense.PRESENT, Form.INDICATIVE, Verb.Pronoun.IL_ELLE_ON, "être", toBe)
    toBeConjug.setVerb(toBe)
    be.setVerbInfo(toBeConjug)
    val personName = "Arnold Schwarzenegger"
    val person = new Word(personName, CUSTOM_PERSON)
    val schwarzy = new CustomPerson("Arnold Schwarzenegger", "SCHWARZY")
    person.setCustomPerson(schwarzy)

    val words = List(
      new Word("qui", INTERROGATIVE_PRONOUN),
      be,
      person
    )

    val obj = parser(words)
    obj.getRequestType shouldBe RequestType.QUESTION
    obj.action.getMainAction.getAction shouldBe ActionType.BE
    obj.what.getGroupType shouldBe GroupType.ADDITIONAL_PERSON
    val question = obj.asInstanceOf[QuestionObject]
    question.questionType shouldBe QuestionType.WHO
    question.what.asInstanceOf[AdditionalPerson].person.getValue shouldBe personName
  }



  "3" should "4" in {
    val  find = new Word("trouve", VERB)
    val toFind = new Verb("trouver", false, new Action(ActionType.FIND))
    val toFindConjug = new VerbConjugation(ConjugationTense.PRESENT, Form.IMPERATIVE, Verb.Pronoun.TU, "trouver", toFind)
    toFindConjug.setVerb(toFind)
    find.setVerbInfo(toFindConjug)
    val videos = new Word("vidéos", COMMON_NAME)
    videos.setNameInfo(new NameInfo("vidéos", "VIDEO"))
    val cats = new Word("chats", COMMON_NAME)
    cats.setNameInfo(new NameInfo("chats", "CAT"))
    val gutter = new Word("gouttière", COMMON_NAME)
    gutter.setNameInfo(new NameInfo("gouttière", "GUTTER"))

    val words = List(
      find,
      new Word("moi", TARGET_PRONOUN),
      new Word("des", INDEFINITE_ARTICLE, PREPOSITION_OF),
      videos,
      new Word("de", PREPOSITION_OF),
      cats,
      new Word("de", PREPOSITION_OF),
      gutter
    )

    val obj = parser(words)
    obj.getRequestType shouldBe RequestType.ORDER
  }
}
