package com.rokuan.calliopecore.fr.autoroute.data

import com.rokuan.calliopecore.fr.sentence.Word
import com.rokuan.calliopecore.sentence.{ICityInfo, ICountryInfo}
import com.rokuan.calliopecore.sentence.structure.data.nominal.{CityObject, CountryObject}
import com.rokuan.calliopecore.sentence.structure.data.place.LocationObject
import com.rokuan.calliopecore.sentence.structure.data.place.PlaceAdverbial.PlaceType

/**
  * Created by Christophe on 27/11/2016.
  */
object PlaceConverter {
  import com.rokuan.calliopecore.fr.sentence.Word.WordType._
  import com.rokuan.calliopecore.fr.autoroute.pattern.WordRules._
  import com.rokuan.calliopecore.fr.autoroute.pattern.PrepositionRules._
  import com.rokuan.autoroute.rules.Rule._


  val CityTransformer = word(CITY) { _.getCityInfo }
  val CountryTransformer = word(COUNTRY) { _.getCountryInfo }
  val CityPrepositionTransformer = (prep(PlaceType.CITY) ~ opt(word(DEFINITE_ARTICLE))) {
    case List(preposition: Word, _) => preposition.getPlacePreposition
  } | prep(PlaceType.CITY, true) { _.getPlacePreposition }
  val CountryPrepositionTransformer = (prep(PlaceType.COUNTRY) ~ opt(word(DEFINITE_ARTICLE))) {
    case List(preposition: Word, _) => preposition.getPlacePreposition
  } | prep(PlaceType.COUNTRY, true) { _.getPlacePreposition }

  val City = CityTransformer { c =>
    new CityObject { city = c }
  }

  val Country = CountryTransformer { c =>
    new CountryObject { country = c }
  }

  val WorldLocation =
    (CityPrepositionTransformer ~ CityTransformer ~ CountryPrepositionTransformer ~ CountryTransformer) {
      case List(_, city: ICityInfo, _, country: ICountryInfo) =>
        new LocationObject {
          city = city
          country = country
        }
    }
}
