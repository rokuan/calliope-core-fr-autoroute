import com.rokuan.autoroute.matchers.Route
import com.rokuan.calliopecore.fr.autoroute.data.InterpretationObjectConverter
import com.rokuan.calliopecore.fr.autoroute.sentence.Verb.ConjugationTense
import com.rokuan.calliopecore.fr.autoroute.sentence._
import com.rokuan.calliopecore.fr.autoroute.sentence.Word.WordType._
import com.rokuan.calliopecore.sentence.IAction.{ActionType, Form}
import com.rokuan.calliopecore.sentence.structure.InterpretationObject.RequestType
import com.rokuan.calliopecore.sentence.structure.QuestionObject
import com.rokuan.calliopecore.sentence.structure.QuestionObject.QuestionType
import com.rokuan.calliopecore.sentence.structure.data.nominal.AdditionalPerson
import com.rokuan.calliopecore.sentence.structure.data.nominal.NominalGroup.GroupType
import com.rokuan.calliopecore.sentence.structure.data.place.NamedPlaceObject
import com.rokuan.calliopecore.sentence.structure.data.place.PlaceAdverbial.{PlaceContext, PlaceType}
import com.rokuan.calliopecore.sentence.structure.data.place.PlaceObject.PlaceCategory
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
    val toGo = new Verb("aller", new Action(ActionType.GO))
    val toGoConjug = new VerbConjugation("aller", toGo, ConjugationTense.PRESENT, Form.INFINITIVE, null)
    val go = new Word("aller",
      wordInfo = new WordInfo("aller", COMMON_NAME),
      verbInfo = toGoConjug)
    val to = new Word("à",
      placePreposition = new PlacePreposition("à", PlaceContext.TO, PlaceType.NAMED_PLACE, PlaceType.CITY))
    val by = new Word("en",
      wayPreposition = new WayPreposition("en", WayContext.BY, WayType.TRANSPORT))
    val car = new Word("voiture",
      nameInfo = new NameInfo("voiture", "CAR"),
      transportInfo = new TransportInfo("voiture", TransportType.CAR))

    val words = List(
      new Word("comment", INTERROGATIVE_PRONOUN),
      go,
      to,
      new Word("la", DEFINITE_ARTICLE),
      new Word("Mairie",
        wordInfo = new WordInfo("Mairie", PROPER_NAME),
        placeInfo = new PlaceInfo("Mairie", PlaceCategory.LAWYER)),
      new Word("de", PREPOSITION_OF),
      new Word("Paris",
        wordInfo = new WordInfo("Paris", PROPER_NAME),
        cityInfo = new CityInfo("Paris", 0, 0)),
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
    val toBe = new Verb("être", true, new Action(ActionType.BE))
    val toBeConjug = new VerbConjugation("est", toBe, ConjugationTense.PRESENT, Form.INDICATIVE, Verb.Pronoun.IL_ELLE_ON)

    val be = new Word("est", verbInfo = toBeConjug)
    val personName = "Arnold Schwarzenegger"
    val person = new Word(personName,
      customPerson = new CustomPerson("Arnold Schwarzenegger", "SCHWARZY"))

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
    val toFind = new Verb("trouver", new Action(ActionType.FIND))
    val toFindConjug = new VerbConjugation("trouve", toFind, ConjugationTense.PRESENT, Form.IMPERATIVE, Verb.Pronoun.TU)
    val find = new Word("trouve", verbInfo = toFindConjug)
    val videos = new Word("vidéos", nameInfo = new NameInfo("vidéos", "VIDEO"))
    val cats = new Word("chats", nameInfo = new NameInfo("chats", "CAT"))
    val gutter = new Word("gouttière", nameInfo = new NameInfo("gouttière", "GUTTER"))

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
