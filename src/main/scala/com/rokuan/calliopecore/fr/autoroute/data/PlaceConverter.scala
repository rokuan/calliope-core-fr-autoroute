package com.rokuan.calliopecore.fr.autoroute.data

import com.rokuan.calliopecore.fr.sentence.Word
import com.rokuan.calliopecore.fr.sentence.Word.WordType
import com.rokuan.calliopecore.sentence.{ICityInfo, ICountryInfo, ICustomPlace, IPlacePreposition}
import com.rokuan.calliopecore.sentence.structure.data.nominal.{AdditionalObject, CityObject, CountryObject}
import com.rokuan.calliopecore.sentence.structure.data.place.{AdditionalPlace, AddressObject, LocationObject, PlaceObject}
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

  val CityRule = CityTransformer { c: ICityInfo =>
    new CityObject { city = c }
  }
  val CountryRule = CountryTransformer { c: ICountryInfo =>
    new CountryObject { country = c }
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
  val NamedPlaceTransformer = { /* TODO: */ }
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

  val WorldLocationRule =
    (CityPrepositionTransformer ~ CityTransformer ~ CountryPrepositionTransformer ~ CountryTransformer) {
      case List(_, city: ICityInfo, _, country: ICountryInfo) =>
        new LocationObject {
          city = city
          country = country
        }
    }
  val PlaceAdverbialRule = AdditionalPlaceRule | WorldLocationRule | CityRule |
    CountryRule | AddressRule /*| NamedPlaceRule*/ | PlaceTypeRule
}
