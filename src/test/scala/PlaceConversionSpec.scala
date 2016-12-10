import com.rokuan.autoroute.matchers.Route
import com.rokuan.calliopecore.fr.autoroute.data.PlaceConverter
import com.rokuan.calliopecore.fr.autoroute.sentence._
import com.rokuan.calliopecore.fr.autoroute.sentence.Word.WordType._
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
    val in = new Word("à",
      placePreposition = new PlacePreposition("à", PlaceContext.IN, PlaceType.CITY))
    val paris = new Word("Paris",
      wordInfo = new WordInfo("Paris", PROPER_NAME),
      cityInfo = new CityInfo("Paris", 48.8564528, 2.3524282))

    val words = List(
      in,
      paris
    )
    val placeObject = parser(words)
    placeObject.getPlaceType shouldBe PlaceType.CITY
    placeObject.asInstanceOf[CityObject].city.getValue shouldBe "Paris"
  }

  "2" should "3" in {
    val in = new Word("en",
      wordInfo = new WordInfo("en", PREPOSITION_IN),
      placePreposition = new PlacePreposition("en", PlaceContext.IN, PlaceType.COUNTRY))
    val france = new Word("France",
      countryInfo = new CountryInfo("France", "FR"))
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
    val paris = new Word("Paris", cityInfo = new CityInfo("Paris", 48.8564528, 2.3524282))
    val france = new Word("France", countryInfo = new CountryInfo("France", "FR"))
    val in1 = new Word("à",
      wordInfo = new WordInfo("à", PREPOSITION_AT),
      placePreposition = new PlacePreposition("à", PlaceContext.IN, PlaceType.CITY))
    val in2 = new Word("en",
      wordInfo = new WordInfo("en", PREPOSITION_IN),
      placePreposition = new PlacePreposition("en", PlaceContext.IN, PlaceType.COUNTRY))

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
    val at = new Word("à",
      placePreposition = new PlacePreposition("à", PlaceContext.AT, PlaceType.NAMED_PLACE))

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
