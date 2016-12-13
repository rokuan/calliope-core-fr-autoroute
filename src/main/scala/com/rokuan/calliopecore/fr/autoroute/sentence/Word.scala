package com.rokuan.calliopecore.fr.autoroute.sentence

import com.rokuan.calliopecore.fr.autoroute.sentence.Word.WordType
import com.rokuan.calliopecore.sentence._

/**
  * Created by Christophe on 09/12/2016.
  */
class Word(val value: String,
           val wordInfo: WordInfo = null,
           val timePreposition: TimePreposition = null,
           val placePreposition: PlacePreposition = null,
           val wayPreposition: WayPreposition = null,
           val purposePreposition: PurposePreposition = null,
           val customPlace: CustomPlace = null,
           val customObject: CustomObject = null,
           val customPerson: CustomPerson = null,
           val customMode: CustomMode = null,
           val languageInfo: LanguageInfo = null,
           val unitInfo: UnitInfo = null,
           val adjectiveInfo: AdjectiveInfo = null,
           val nameInfo: NameInfo = null,
           val placeInfo: PlaceInfo = null,
           val verbInfo: VerbConjugation = null,
           val firstNameInfo: FirstNameInfo = null,
           val characterInfo: CharacterInfo = null,
           val colorInfo: ColorInfo = null,
           val transportInfo: TransportInfo = null,
           val cityInfo: CityInfo = null,
           val countryInfo: CountryInfo = null) extends IWord {
  def this(value: String, ts: WordType.Value*) = this(value, new WordInfo(value, ts.toSet))
  val types: Set[WordType.Value] = getWordTypes().toSet

  private final def getWordTypes() = {
    val additionalTypes = new collection.mutable.HashSet[WordType.Value]()
    Option(wordInfo).foreach(i => additionalTypes ++= i.types)
    Option(timePreposition).foreach(_ => additionalTypes.add(WordType.TIME_PREPOSITION))
    Option(placePreposition).foreach(_ => additionalTypes.add(WordType.PLACE_PREPOSITION))
    Option(wayPreposition).foreach(_ => additionalTypes.add(WordType.WAY_PREPOSITION))
    Option(purposePreposition).foreach(_ => additionalTypes.add(WordType.PURPOSE_PREPOSITION))
    Option(customPlace).foreach(_ => additionalTypes.add(WordType.CUSTOM_PLACE))
    Option(customPerson).foreach(_ => additionalTypes.add(WordType.CUSTOM_PERSON))
    Option(customMode).foreach(_ => additionalTypes.add(WordType.CUSTOM_MODE))
    Option(customObject).foreach(_ => additionalTypes.add(WordType.CUSTOM_OBJECT))
    Option(languageInfo).foreach(_ => additionalTypes.add(WordType.LANGUAGE))
    Option(colorInfo).foreach(_ => additionalTypes.add(WordType.COLOR))
    Option(nameInfo).foreach(_ => additionalTypes.add(WordType.COMMON_NAME))
    Option(adjectiveInfo).foreach(_ => additionalTypes.add(WordType.ADJECTIVE))
    Option(placeInfo).foreach(_ => additionalTypes.add(WordType.PLACE_TYPE))
    Option(unitInfo).foreach(_ => additionalTypes.add(WordType.UNIT))
    Option(firstNameInfo).foreach(_ => additionalTypes.add(WordType.FIRSTNAME))
    Option(characterInfo).foreach(_ => additionalTypes.add(WordType.PERSON_TYPE))
    Option(cityInfo).foreach(_ => additionalTypes.add(WordType.CITY))
    Option(countryInfo).foreach(_ => additionalTypes.add(WordType.COUNTRY))
    Option(verbInfo).foreach { v =>
      additionalTypes.add(WordType.VERB)
      if(v.getVerb().isAuxiliary()){
        additionalTypes.add(WordType.AUXILIARY)
      }
    }
    Option(transportInfo).foreach(_ => additionalTypes.add(WordType.MEAN_OF_TRANSPORT))
    additionalTypes
  }

  def isOfType(t: WordType.Value) = types.contains(t)
  def getTypes = types

  override def getValue: String = value
  override def getPlacePreposition: IPlacePreposition = placePreposition
  override def getCustomMode: ICustomMode = customMode
  override def getLanguageInfo: ILanguageInfo = languageInfo
  override def getCustomPlace: ICustomPlace = customPlace
  override def getCustomObject: ICustomObject = customObject
  override def getWayPreposition: IWayPreposition = wayPreposition
  override def getUnitInfo: IUnitInfo = unitInfo
  override def getCityInfo: ICityInfo = cityInfo
  override def getTimePreposition: ITimePreposition = timePreposition
  override def getAdjectiveInfo: IAdjectiveInfo = adjectiveInfo
  override def getPurposePreposition: IPurposePreposition = purposePreposition
  override def getNameInfo: INameInfo = nameInfo
  override def getPlaceInfo: IPlaceInfo = placeInfo
  override def getVerbInfo: IAction = verbInfo
  override def getCharacterInfo: ICharacterInfo = characterInfo
  override def getColorInfo: IColorInfo = colorInfo
  override def getTransportInfo: ITransportInfo = transportInfo
  override def getCountryInfo: ICountryInfo = countryInfo
  override def getCustomPerson: ICustomPerson = customPerson
}

object Word {
  object WordType extends Enumeration {
    type WordType = Value
    val PROPER_NAME,
    COMMON_NAME,
    ADVERB,
    VERB,
    ADJECTIVE,
    ONOMATOPEIA,
    PREPOSITION,
    CONJUNCTION,
    AUXILIARY,
    DEMONSTRATIVE_PRONOUN,
    DEFINITE_ARTICLE,
    INDEFINITE_ADJECTIVE,
    INDEFINITE_PRONOUN,
    INTERROGATIVE_PRONOUN,
    RELATIVE_PRONOUN,
    NUMERICAL_ADJECTIVE,
    NUMBER,
    REAL,
    NUMERICAL_POSITION,
    DEMONSTRATIVE_ADJECTIVE,
    PERSONAL_PRONOUN,
    INDEFINITE_ARTICLE,
    POSSESSIVE_PRONOUN,
    POSSESSIVE_ADJECTIVE,
    EUPHONIOUS_LINK,
    INTERROGATIVE_ADJECTIVE,
    PREPOSITION_FROM,
    PREPOSITION_TO,
    DATE_MONTH,
    DATE,
    DATE_UNIT,
    TIME,
    // New
    QUANTITY,
    PREPOSITION_BETWEEN, // entre
    PREPOSITION_AND, // et
    PREPOSITION_AT, // �/au
    PREPOSITION_OF, // de
    PREPOSITION_IN, // en
    PREPOSITION_WITH, // avec
    CONJUGATION_LINK, // t
    SUPERLATIVE, // moins/plus

    //TARGET_PRONOUN,	// moi/toi/me/te/...
    SOURCE_PRONOUN, //
    TARGET_PRONOUN, // me/te/lui/nous/vous/leur
    REFLEXIVE_PRONOUN, // me/te/se/nous/vous/se

    DATE_UNIT_HOUR, // midi/minuit
    MEAN_OF_TRANSPORT, // pied/voiture/bus/avion/...
    PLACE_TYPE, // restaurant/cin�ma/...
    STREET_TYPE, //rue/avenue/boulevard/...
    PERSON_TYPE, // voisin/oncle/m�decin

    CITY,
    COUNTRY,
    FIRSTNAME,
    LANGUAGE,
    UNIT,
    PLACE,
    COLOR,
    CUSTOM_OBJECT,
    CUSTOM_PLACE,
    CUSTOM_MODE,
    CUSTOM_PERSON,
    ADDRESS,

    CONTRACTED,

    TIME_PREPOSITION,
    PLACE_PREPOSITION,
    WAY_PREPOSITION,
    PURPOSE_PREPOSITION,

    OTHER = Value
  }
}
