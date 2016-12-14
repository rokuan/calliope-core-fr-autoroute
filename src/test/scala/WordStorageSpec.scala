import com.rokuan.calliopecore.fr.autoroute.parser.{SentenceParser, WordStorage}
import com.rokuan.calliopecore.fr.autoroute.sentence.Verb.ConjugationTense
import com.rokuan.calliopecore.fr.autoroute.sentence.Word.WordType
import com.rokuan.calliopecore.fr.autoroute.sentence.{CustomPerson, _}
import com.rokuan.calliopecore.sentence.IAction
import com.rokuan.calliopecore.sentence.IAction.ActionType
import com.rokuan.calliopecore.sentence.structure.data.place.PlaceAdverbial.{PlaceContext, PlaceType}
import com.rokuan.calliopecore.sentence.structure.data.way.TransportObject.TransportType
import com.rokuan.calliopecore.sentence.structure.data.way.WayAdverbial.{WayContext, WayType}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Christophe on 14/12/2016.
  */
class WordStorageSpec extends FlatSpec with Matchers {
  val wordsToMatch = List("comment", "aller", "à", "Paris", "pied")
  val db = new WordStorage {
    override def wordStartsWith(q: String): Boolean = wordsToMatch.exists(_.startsWith(q))
    override def findUnitInfo(q: String): UnitInfo = null
    override def findCustomPerson(q: String): CustomPerson = null
    override def findCityInfo(q: String): CityInfo =
      if(q == "Paris") {
        new CityInfo("Paris", 42, 8)
      } else {
        null
      }
    override def findWayPreposition(q: String): WayPreposition =
      if(q == "à"){
        new WayPreposition("à", WayContext.BY, WayType.TRANSPORT)
      } else {
        null
      }
    override def findPlaceInfo(q: String): PlaceInfo = null
    override def findLanguageInfo(q: String): LanguageInfo = null
    override def findCharacterInfo(q: String): CharacterInfo = null
    override def findCustomObject(q: String): CustomObject = null
    override def findPurposePreposition(q: String): PurposePreposition = null
    override def findTimePreposition(q: String): TimePreposition = null
    override def findColorInfo(q: String): ColorInfo = null
    override def findTransportInfo(q: String): TransportInfo =
      if(q == "pied"){
        new TransportInfo("pied", TransportType.WALK)
      } else {
        null
      }
    override def findConjugation(q: String): VerbConjugation =
      if(q == "aller"){
        new VerbConjugation("aller",
          new Verb("aller", false, new Action(ActionType.GO)),
          ConjugationTense.PRESENT,
          IAction.Form.INFINITIVE,
          null)
      } else {
        null
      }
    override def findNameInfo(q: String): NameInfo =
      if(q == "aller"){
        new NameInfo("aller", "go")
      } else if(q == "pied"){
        new NameInfo("pied", "FOOT")
      } else {
        null
      }
    override def findFirstnameInfo(q: String): FirstNameInfo = null
    override def findPlacePreposition(q: String): PlacePreposition =
      if(q == "à"){
        new PlacePreposition("à", PlaceContext.AT, PlaceType.CITY)
      } else {
        null
      }
    override def findCustomMode(q: String): CustomMode = null
    override def findWordInfo(q: String): WordInfo =
      if(q == "comment"){
        new WordInfo("comment", WordType.INTERROGATIVE_ADJECTIVE, WordType.INTERROGATIVE_PRONOUN)
      } else {
        null
      }
    override def findAdjectiveInfo(q: String): AdjectiveInfo = null
    override def findCountryInfo(q: String): CountryInfo = null
    override def findCustomPlace(q: String): CustomPlace = null
  }
  val parser = new SentenceParser(db)

  "1" should "2" in {
    parser.parseText("comment aller à Paris à pied")
  }
}
