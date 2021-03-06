package com.rokuan.calliopecore.fr.autoroute.parser

import com.rokuan.autoroute.matchers.Route
import com.rokuan.autoroute.{PNil, Producer}
import com.rokuan.calliopecore.fr.autoroute.data.InterpretationObjectConverter
import com.rokuan.calliopecore.fr.autoroute.sentence.{Word, WordInfo}
import com.rokuan.calliopecore.fr.autoroute.sentence.Word.WordType._
import com.rokuan.calliopecore.parser.AbstractParser
import com.rokuan.calliopecore.sentence.structure.InterpretationObject
import SentenceParser._
import Producer._

import scala.collection.mutable.ListBuffer

/**
  * Created by Christophe on 05/12/2016.
  */
class SentenceParser(val db: WordStorage) extends AbstractParser {
  val parser = new Route(InterpretationObjectConverter.InterpretationObjectRule)

  override def parseText(s: String): InterpretationObject = {
    val words: List[Word] = lexSpeech(s)
    parser(words)
  }

  protected def lexSpeech(text: String): Producer[Word] = {
    val parts = text.split("\"")
    val even = if (text.startsWith("\"")) { 0 } else { 1 }
    val buffer = new ListBuffer[Word]
    parts.zipWithIndex.foreach { p =>
      if (p._2 % 2 == even) {
        buffer += new Word(p._1, TEXT)
      } else {
        buffer ++= lexWords(p._1)
      }
    }
    buffer.toList
  }

  protected def lexWords(text: String): Producer[Word] = {
    val words = text.split(" ").toList
    def getNextWord(words: List[String]): Producer[Word] = {
      words match {
        case head :: tail =>
          val headWord = getWord(head)
          (headWord, db.wordStartsWith(head + " ")) match {
            case (w, false) if w != null => w :: getNextWord(tail)
            case (null, false) =>
              head match {
                case _ if head.indexOf('\'') >= 0 =>
                  val quoteIndex = head.indexOf('\'')
                  val leftPart = head.substring(0, quoteIndex)
                  val rightPart = head.substring(quoteIndex + 1, head.length)
                  val leftWord = Option(getWord(leftPart))
                    .getOrElse(new Word(leftPart, OTHER))
                  leftWord :: getNextWord(rightPart :: tail)
                case _ if head.indexOf('-') >= 0 =>
                  val dashIndex = head.indexOf('-')
                  val leftPart = head.substring(0, dashIndex)
                  val rightPart = head.substring(dashIndex + 1, head.length)
                  val leftWord = Option(getWord(leftPart))
                    .getOrElse(new Word(leftPart, OTHER))
                  leftWord :: getNextWord(rightPart :: tail)
                case _ => new Word(head, OTHER) :: getNextWord(tail)
              }
            case (null, true) =>
              tail.headOption.map { s =>
                getNextWord((head + " " + s) :: tail.tail)
              }.getOrElse(PNil)
            case (w, true) =>
              // TODO:
              var currentWord = w
              val buffer = new StringBuilder(currentWord.getValue)
              var realEnd = tail
              var toMatch = tail
              var shouldContinue = true

              while(shouldContinue && !toMatch.isEmpty){
                buffer.append(" " + toMatch.head)
                val newQuery = buffer.toString
                Option(getWord(newQuery)).foreach { result =>
                  currentWord = result
                  realEnd = toMatch
                }
                shouldContinue = db.wordStartsWith(newQuery + " ")
                toMatch = toMatch.tail
              }
              currentWord :: getNextWord(realEnd)
          }
        case Nil => PNil
      }
    }
    getNextWord(words)
  }

  protected def getWord(s: String): Word = {
    s match {
      case PositionRegex(_) => new Word(s, NUMERICAL_POSITION)
      case FullTimeRegex(_, _) | HoursOnlyRegex(_)  => new Word(s, TIME)
      case RealNumberRegex(_) => new Word(s, REAL)
      case NumberRegex(_) => new Word(s, NUMBER)
      case proper if proper(0).isUpper => getProperName(s) // TODO: what happens for names such as "SMS"
      case _ => getCommonName(s)
    }
  }

  protected def getProperName(s: String): Word = {
    new Word(s,
      wordInfo = new WordInfo(s, PROPER_NAME),
      countryInfo = db.findCountryInfo(s),
      cityInfo = db.findCityInfo(s),
      firstNameInfo = db.findFirstnameInfo(s),
      customPerson = db.findCustomPerson(s),
      customPlace = db.findCustomPlace(s),
      customObject = db.findCustomObject(s))
  }

  protected def getCommonName(s: String): Word = {
    val result = new Word(s,
      wordInfo = db.findWordInfo(s),
      nameInfo = db.findNameInfo(s),
      verbInfo = db.findConjugation(s),
      adjectiveInfo = db.findAdjectiveInfo(s),
      languageInfo = db.findLanguageInfo(s),
      colorInfo = db.findColorInfo(s),
      transportInfo = db.findTransportInfo(s),
      unitInfo = db.findUnitInfo(s),
      characterInfo = db.findCharacterInfo(s),
      placeInfo = db.findPlaceInfo(s),
      customObject = db.findCustomObject(s),
      customMode = db.findCustomMode(s),
      customPlace = db.findCustomPlace(s),
      customPerson = db.findCustomPerson(s),
      placePreposition = db.findPlacePreposition(s),
      timePreposition = db.findTimePreposition(s),
      wayPreposition = db.findWayPreposition(s),
      purposePreposition = db.findPurposePreposition(s))

    if(result.getTypes.isEmpty){
      null
    } else {
      result
    }
  }
}

object SentenceParser {
  val FullTimeRegex = "(\\d+)h(\\d+)".r
  val HoursOnlyRegex = "(\\d+)h".r
  val PositionRegex = "(\\d+)e".r
  val RealNumberRegex = "(\\d+\\.\\d+)".r
  val NumberRegex = "(\\d+)".r
}
