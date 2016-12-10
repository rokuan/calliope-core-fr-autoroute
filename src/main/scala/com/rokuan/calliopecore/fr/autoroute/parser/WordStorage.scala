package com.rokuan.calliopecore.fr.autoroute.parser

import com.rokuan.calliopecore.fr.autoroute.sentence._

/**
  * Created by Christophe on 09/12/2016.
  */
trait WordStorage {
  def wordStartsWith(q: String): Boolean

  def findWordInfo(q: String): WordInfo

  def findNameInfo(q: String): NameInfo
  def findAdjectiveInfo(q: String): AdjectiveInfo
  def findLanguageInfo(q: String): LanguageInfo
  def findColorInfo(q: String): ColorInfo
  def findCityInfo(q: String): CityInfo
  def findCountryInfo(q: String): CountryInfo
  def findTransportInfo(q: String): TransportInfo
  def findUnitInfo(q: String): UnitInfo
  def findFirstnameInfo(q: String): FirstNameInfo
  def findCharacterInfo(q: String): CharacterInfo
  def findPlaceInfo(q: String): PlaceInfo

  def findCustomObject(q: String): CustomObject
  def findCustomPlace(q: String): CustomPlace
  def findCustomMode(q: String): CustomMode
  def findCustomPerson(q: String): CustomPerson

  def findPlacePreposition(q: String): PlacePreposition
  def findTimePreposition(q: String): TimePreposition
  def findWayPreposition(q: String): WayPreposition
  def findPurposePreposition(q: String): PurposePreposition

  def findConjugation(q: String): VerbConjugation
}
