import com.rokuan.autoroute.matchers.Route
import com.rokuan.calliopecore.fr.autoroute.data.PlaceConverter
import com.rokuan.calliopecore.fr.sentence.{CityInfo, CountryInfo, PlacePreposition, Word}
import com.rokuan.calliopecore.fr.sentence.Word.WordType._
import com.rokuan.calliopecore.sentence.structure.data.nominal.{CityObject, CountryObject}
import com.rokuan.calliopecore.sentence.structure.data.place.{LocationObject, NamedPlaceObject}
import com.rokuan.calliopecore.sentence.structure.data.place.PlaceAdverbial.{PlaceContext, PlaceType}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Christophe on 06/12/2016.
  */
class PlaceConversionSpec extends FlatSpec with Matchers {
  val parser = new Route(PlaceConverter.PlaceAdverbialRule)

  "1" should "2" in {
    val in = new Word("à", PLACE_PREPOSITION)
    in.setPlacePreposition(new PlacePreposition("à", PlaceContext.IN, PlaceType.CITY))
    val paris = new Word("Paris", CITY, PROPER_NAME)
    paris.setCityInfo(new CityInfo("Paris", 48.8564528, 2.3524282))
    val words = List(
      in,
      paris
    )
    val placeObject = parser(words)
    placeObject.getPlaceType shouldBe PlaceType.CITY
    placeObject.asInstanceOf[CityObject].city.getValue shouldBe "Paris"
  }

  "2" should "3" in {
    val in = new Word("en", PLACE_PREPOSITION, PREPOSITION_IN)
    in.setPlacePreposition(new PlacePreposition("en", PlaceContext.IN, PlaceType.COUNTRY))
    val france = new Word("France", COUNTRY)
    france.setCountryInfo(new CountryInfo("France", "FR"))
    val words = List(
      in,
      france
    )
    val placeObject = parser(words)
    placeObject.getPlaceType shouldBe COUNTRY
    val country = placeObject.asInstanceOf[CountryObject]
    country.country.getValue shouldBe "France"
    country.country.getCountryCode shouldBe "FR"
  }

  "3" should "4" in {
    val paris = new Word("Paris", CITY)
    val france = new Word("France", COUNTRY)
    val in1 = new Word("à", PREPOSITION_AT, PLACE_PREPOSITION)
    val in2 = new Word("en", PREPOSITION_IN, PLACE_PREPOSITION)

    paris.setCityInfo(new CityInfo("Paris", 48.8564528, 2.3524282))
    france.setCountryInfo(new CountryInfo("France", "FR"))
    in1.setPlacePreposition(new PlacePreposition("à", PlaceContext.IN, PlaceType.CITY))
    in2.setPlacePreposition(new PlacePreposition("en", PlaceContext.IN, PlaceType.COUNTRY))

    val words = List(
      in1,
      paris,
      in2,
      france
    )
    val placeObject = parser(words)
    placeObject.getPlaceType shouldBe PlaceType.LOCATION
    val state = placeObject.asInstanceOf[LocationObject]
    state.city.getValue shouldBe "Paris"
    state.country.getValue shouldBe "France"
  }

  "4" should "5" in {
    val at = new Word("à", PLACE_PREPOSITION)
    at.setPlacePreposition(new PlacePreposition("à", PlaceContext.AT, PlaceType.NAMED_PLACE))
    val words = List(
      at,
      new Word("la", DEFINITE_ARTICLE),
      new Word("Tour", PROPER_NAME),
      new Word("Eiffel", PROPER_NAME)
    )
    val placeObject = parser(words)
    val namedPlace = placeObject.asInstanceOf[NamedPlaceObject]
    namedPlace.name shouldBe "Tour Eiffel"
  }
}
