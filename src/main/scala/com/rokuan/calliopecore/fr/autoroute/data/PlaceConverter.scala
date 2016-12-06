package com.rokuan.calliopecore.fr.autoroute.data

import com.rokuan.calliopecore.fr.sentence.Word
import com.rokuan.calliopecore.fr.sentence.Word.WordType
import com.rokuan.calliopecore.sentence.{ICityInfo, ICountryInfo, ICustomPlace, IPlacePreposition}
import com.rokuan.calliopecore.sentence.structure.data.nominal.{AdditionalObject, CityObject, CountryObject}
import com.rokuan.calliopecore.sentence.structure.data.place._
import com.rokuan.calliopecore.sentence.structure.data.place.PlaceAdverbial.PlaceType

/**
  * Created by Christophe on 27/11/2016.
  */
object PlaceConverter {
  import com.rokuan.calliopecore.fr.sentence.Word.WordType._
  import com.rokuan.calliopecore.fr.autoroute.pattern.WordRules._
  import com.rokuan.calliopecore.fr.autoroute.pattern.PrepositionRules._
  import com.rokuan.autoroute.rules.Rule._

  val NumberTransformer = word(NUMBER) { _.getValue.toInt }
  val WordToStringTransformer = { wt: WordType =>
    word(wt) { _.getValue }
  }

  val PlacePrepositionTransformer = { p: PlaceType =>
    (prep(p) ~ opt(word(DEFINITE_ARTICLE))) {
      case List(preposition: Word, _) => preposition.getPlacePreposition
    } | prep(p, true) { _.getPlacePreposition }
  }

  val CustomPlaceTransformer = word(CUSTOM_PLACE) { _.getCustomPlace }
  val AdditionalPlacePreposition = PlacePrepositionTransformer(PlaceType.CUSTOM)
  val AdditionalPlaceRule = (AdditionalPlacePreposition ~ CustomPlaceTransformer) {
    case List(preposition: IPlacePreposition, place: ICustomPlace) =>
      new AdditionalPlace {
        place = place
        setPlacePreposition(preposition)
      }
  }

  val CityTransformer = word(CITY) { _.getCityInfo }
  val CountryTransformer = word(COUNTRY) { _.getCountryInfo }

  val CityPrepositionTransformer = (prep(PlaceType.CITY) ~ opt(word(DEFINITE_ARTICLE))) {
    case List(preposition: Word, _) => preposition.getPlacePreposition
  } | prep(PlaceType.CITY, true) { _.getPlacePreposition }
  val CountryPrepositionTransformer = (prep(PlaceType.COUNTRY) ~ opt(word(DEFINITE_ARTICLE))) {
    case List(preposition: Word, _) => preposition.getPlacePreposition
  } | prep(PlaceType.COUNTRY, true) { _.getPlacePreposition }

  val CityRule = (CityPrepositionTransformer ~ CityTransformer) {
    case List(prep: IPlacePreposition, c: ICityInfo) =>
      val city = new CityObject { city = c }
      city.setPlacePreposition(prep)
      city
  }
  val CountryRule = (CountryPrepositionTransformer ~ CountryTransformer) {
    case List(prep: IPlacePreposition, c: ICountryInfo) =>
      val country = new CountryObject { country = c }
      country.setPlacePreposition(prep)
      country
  }

  val AddressPreposition = PlacePrepositionTransformer(PlaceType.ADDRESS)
  val StreetAddressSuffix = (word("de") ~ opt(word("la"))) {
    case List(w1: Word, w2: Option[Word]) =>
      w1.getValue + w2.map(w => " " + w.getValue).getOrElse("")
  } | (word("du|des")) { _.getValue }
  val StreetAddressTransformer = (opt(WordToStringTransformer(NUMBER)) ~ WordToStringTransformer(STREET_TYPE) ~
    opt(StreetAddressSuffix) ~ (WordToStringTransformer(PROPER_NAME)+)) {
    case List(n: Option[String], t: String, suffix: Option[String], names: List[String]) =>
      val fullName = Some(t) :: suffix :: names.map(Some(_))
      new AddressObject {
        number = n.getOrElse("")
        streetType = t
        addressName = fullName.flatten.mkString(" ")
      }
  }
  val PlaceTypeTransformer = word(PLACE_TYPE) { w =>
    new PlaceObject {
      placeCategory = w.getPlaceInfo.getPlaceCategory
    }
  }
  val PlaceLinkTransformer = word(PREPOSITION_OF, CONTRACTED) {
    _.getValue
  } | (word(PREPOSITION_OF) ~ opt(word(DEFINITE_ARTICLE))) {
    case List(w1: Word, w2: Option[Word]) =>
      w1.getValue + w2.map(" " + _.getValue).getOrElse("")
  }
  val NamedPlaceTransformer = (word(PLACE_TYPE) ~ opt(PlaceLinkTransformer) ~ list(word(PROPER_NAME))) {
    case List(t: Word, additional: Option[String], names: List[Word]) =>
      new NamedPlaceObject {
        name = t.getValue + " " + additional.map(_ + " ").getOrElse("") + names.map(_.getValue).mkString(" ")
      }
  }
  val AddressRule = (AddressPreposition ~ StreetAddressTransformer) {
    case List(preposition: IPlacePreposition, address: AddressObject) =>
      address.setPlacePreposition(preposition)
      address
  }
  val PlaceTypeRule = (PlacePrepositionTransformer(PlaceType.PLACE_TYPE) ~ PlaceTypeTransformer){
    case List(preposition: IPlacePreposition, place: PlaceObject) =>
      place.setPlacePreposition(preposition)
      place
  }
  val NamedPlaceRule = (PlacePrepositionTransformer(PlaceType.NAMED_PLACE) ~ NamedPlaceTransformer) {
    case List(preposition: IPlacePreposition, place: NamedPlaceObject) =>
      place.setPlacePreposition(preposition)
      place
  }

  val WorldLocationRule =
    (CityPrepositionTransformer ~ CityTransformer ~ CountryPrepositionTransformer ~ CountryTransformer) {
      case List(_, city: ICityInfo, _, country: ICountryInfo) =>
        new LocationObject {
          city = city
          country = country
        }
    }
  val PlaceAdverbialRule = AdditionalPlaceRule | WorldLocationRule | CityRule |
    CountryRule | AddressRule | NamedPlaceRule | PlaceTypeRule
}
