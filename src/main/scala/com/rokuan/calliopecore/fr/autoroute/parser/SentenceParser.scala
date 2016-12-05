package com.rokuan.calliopecore.fr.autoroute.parser

import com.rokuan.autoroute.matchers.Route
import com.rokuan.autoroute.{PNil, Producer}
import com.rokuan.calliopecore.fr.autoroute.data.InterpretationObjectConverter
import com.rokuan.calliopecore.fr.parser.WordDatabase
import com.rokuan.calliopecore.fr.sentence.Word
import com.rokuan.calliopecore.fr.sentence.Word.WordType
import com.rokuan.calliopecore.fr.sentence.Word.WordType._
import com.rokuan.calliopecore.parser.AbstractParser
import com.rokuan.calliopecore.sentence.structure.InterpretationObject
import collection.JavaConversions._

/**
  * Created by Christophe on 05/12/2016.
  */
class SentenceParser(val db: WordDatabase) extends AbstractParser {
  val parser = new Route(InterpretationObjectConverter.InterpretationObjectRule)

  override def parseText(s: String): InterpretationObject = {
    val words = lexSpeech(s)
    parser(words)
  }

  protected def lexSpeech(text: String): Producer[Word] = {

    PNil
  }

  protected def getWord(s: String): Word = {
    val fullTimeRegex = "".r
    val hoursOnlyRegex = "".r
    val realNumberRegex = "".r
    val numberRegex = "".r
    val properNameRegex = "".r

    s match {
      case fullTimeRegex(_, _) | hoursOnlyRegex(_)  => new Word(s, TIME)
      case realNumberRegex(_) => new Word(s, REAL)
      case numberRegex(_) => new Word(s, NUMBER)
      case words if words.split(" ").exists(_(0).isUpper) => getProperName(s)
      case _ => getCommonName(s)
    }
  }

  protected def getProperName(s: String): Word = {
    implicit val types = new collection.mutable.HashSet[WordType]()
    implicit val query = s
    types.add(PROPER_NAME)
    val word = new Word(s, types)

    updateWord(db.findCountryInfo, COUNTRY, word.setCountryInfo)
    updateWord(db.findCityInfo, CITY, word.setCityInfo)
    updateWord(db.findFirstnameInfo, FIRSTNAME, word.setFirstnameInfo)
    updateWord(db.findCustomPerson, CUSTOM_PERSON, word.setCustomPerson)
    updateWord(db.findCustomPlace, CUSTOM_PLACE, word.setCustomPlace)
    updateWord(db.findCustomObject, CUSTOM_OBJECT, word.setCustomObject)

    word
  }

  protected def getCommonName(s: String): Word = {
    implicit val types = new collection.mutable.HashSet[WordType]()
    implicit val query = s
    val result = new Word(s, types)
    val initialWord = db.findWord(s)

    updateWord(db.findNameInfo, COMMON_NAME, result.setNameInfo)
    updateWord(db.findAdjectiveInfo, ADJECTIVE, result.setAdjectiveInfo)
    updateWord(db.findLanguageInfo, LANGUAGE, result.setLanguageInfo)
    updateWord(db.findColorInfo, COLOR, result.setColorInfo)
    updateWord(db.findTransportInfo, MEAN_OF_TRANSPORT, result.setTransportInfo)
    updateWord(db.findUnitInfo, UNIT, result.setUnitInfo)
    updateWord(db.findCharacterInfo, PERSON_TYPE, result.setCharacterInfo)
    updateWord(db.findPlaceInfo, PLACE_TYPE, result.setPlaceInfo)
    updateWord(db.findCustomObject, CUSTOM_OBJECT, result.setCustomObject)
    updateWord(db.findCustomMode, CUSTOM_MODE, result.setCustomMode)
    updateWord(db.findCustomPlace, CUSTOM_PLACE, result.setCustomPlace)
    updateWord(db.findCustomPerson, CUSTOM_PERSON, result.setCustomPerson)
    updateWord(db.findPlacePreposition, PLACE_PREPOSITION, result.setPlacePreposition)
    updateWord(db.findTimePreposition, TIME_PREPOSITION, result.setTimePreposition)
    updateWord(db.findWayPreposition, WAY_PREPOSITION, result.setWayPreposition)
    updateWord(db.findPurposePreposition, PURPOSE_PREPOSITION, result.setPurposePreposition)

    Option(db.findConjugation(s)).foreach { c =>
      types.add(VERB)
      if(c.getVerb.isAuxiliary){
        types.add(AUXILIARY)
      }
      result.setVerbInfo(c)
    }

    if(types.isEmpty){
      initialWord
    } else {
      Option(initialWord).map(_.getTypes.foreach(result.addType))
      result
    }
  }

  private final def updateWord[T](getInfo: String => T, t: WordType, setInfo: T => Unit)(implicit query: String, types: collection.mutable.Set[WordType]) = {
    Option(getInfo(query)).foreach { i =>
      types.add(t)
      setInfo(i)
    }
  }
}
