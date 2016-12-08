package com.rokuan.calliopecore.fr.autoroute.data

import com.rokuan.calliopecore.fr.sentence.Word
import com.rokuan.calliopecore.sentence.structure.content.{INominalObject, ISecondObject}
import com.rokuan.calliopecore.sentence.{ICustomObject, INameInfo}
import com.rokuan.calliopecore.sentence.structure.data.count.CountObject
import com.rokuan.calliopecore.sentence.structure.data.nominal.UnitObject.UnitType
import com.rokuan.calliopecore.sentence.structure.data.nominal._

/**
  * Created by Christophe on 27/11/2016.
  */
object NominalGroupConverter {
  import com.rokuan.autoroute.rules.Rule._
  import com.rokuan.calliopecore.fr.autoroute.pattern.WordRules._
  import com.rokuan.calliopecore.fr.sentence.Word.WordType._
  import com.rokuan.calliopecore.fr.{ sentence => sentence }


  val NumberTransformer = word(NUMBER) { _.getValue.toInt }
  val DoubleTransformer = word(NUMBER) { _.getValue.toDouble }
  val UnitTransformer = word(UNIT) { _.getUnitInfo.getUnitType }
  val CustomObjectTransformer = word(CUSTOM_OBJECT) { _.getCustomObject }

  val FirstName = list(word(FIRSTNAME))
  val LastName = list(word(PROPER_NAME))

  val CommonObjectTransformer = word(COMMON_NAME) { _.getNameInfo }

  val AdditionalObjectTransformer = (opt(CountConverter.CountRule) ~ CustomObjectTransformer) {
    case List(count: Option[CountObject], obj: ICustomObject) =>
      val o = new AdditionalObject {
        `object` = obj
      }
      count.foreach(o.count = _)
      o
  }
  val PhoneNumber = (word(DEFINITE_ARTICLE) ~ list(NumberTransformer)) {
    case List(_, numbers: List[Int]) =>
      // TODO: How to handle numbers such as "099" ?
      val phoneNumber = numbers.map {
        case n if n < 100 => "%2d".format(n)
        case n => n.toString
      }.mkString
      new PhoneNumberObject {
        number = phoneNumber
      }
  }
  val Quantity = (DoubleTransformer ~ UnitTransformer) {
    case List(quantity: Int, unit: UnitType) =>
      new QuantityObject {
        amount = quantity
        unitType = unit
      }
  }
  val Color = (word(DEFINITE_ARTICLE) ~ word(COLOR)) {
    case List(_, c: Word) =>
      new ColorObject { color = c.getColorInfo }
  }
  val PersonTransformer = (list(word(FIRSTNAME)) ~ opt(list(word(PROPER_NAME)))) {
    case List(firstNames: List[Word], lastNames: Option[List[Word]]) =>
      val names = firstNames.map(_.getValue) ++ lastNames.getOrElse(List()).map(_.getValue)
      new PersonObject(names.mkString(" "))
  }
  val AdditionalPersonTransformer = word(CUSTOM_PERSON) { w => new AdditionalPerson { person = w.getCustomPerson } }
  val PersonObjectRule = AdditionalPersonTransformer | PersonTransformer
  val Language = (word(DEFINITE_ARTICLE) ~ word(LANGUAGE)) {
    case List(_, l: Word) =>
      new LanguageObject { language = l.getLanguageInfo }
  }

  val PronounTransformer = word(PERSONAL_PRONOUN) { w => new PronounSubject(sentence.Pronoun.parseSubjectPronoun(w.getValue))}

  val SimpleObjectOnlyRule = word(COMMON_NAME) { w =>
    new NameObject { `object` = w.getNameInfo }
  } | word(CUSTOM_OBJECT) { w =>
    new AdditionalObject { `object` = w.getCustomObject }
  }

  val CommonObjectAuxRule = (opt(CountConverter.CountRule) ~ CommonObjectTransformer) {
    case List(count: Option[CountObject], commonName: INameInfo) =>
      val o = new NameObject {
        `object` = commonName
      }
      count.foreach(o.count = _)
      o
  }

  val SecondDirectObject = AdditionalObjectTransformer | CommonObjectAuxRule
  val LastDirectObject = PersonObjectRule
  val SecondDirectObjectRule = (word(PREPOSITION_OF) ~ SecondDirectObject) {
    case List(_, directObject: INominalObject with ISecondObject) => directObject
  }
  val LastDirectObjectRule = (word(PREPOSITION_OF) ~ LastDirectObject) {
    case List(_, directObject: INominalObject) => directObject
  }
  val SecondDirectObjectList = (list(SecondDirectObjectRule) ~ LastDirectObjectRule) {
    case List(seconds: List[INominalObject with ISecondObject], last: INominalObject) =>
      val first = seconds.head
      val lastSecond = seconds.tail.foldLeft(first) { case (acc, next) => acc.setNominalSecondObject(next); next }
      lastSecond.setNominalSecondObject(last)
      first
  } ~ list(SecondDirectObjectRule) { seconds =>
    val first = seconds.head
    seconds.tail.foldLeft(first) { case (acc, next) => acc.setNominalSecondObject(next); next }
    first
  }

  val CommonObjectRule = (opt(CountConverter.CountRule) ~ CommonObjectTransformer ~ opt(SecondDirectObjectList)) {
    case List(count: Option[CountObject], commonName: INameInfo, next: Option[INominalObject]) =>
      val o = new NameObject {
        `object` = commonName
      }
      count.foreach(o.count = _)
      next.foreach(o.setNominalSecondObject)
      o
  }

  val SubjectRule = AdditionalObjectTransformer | PersonObjectRule | Quantity | PhoneNumber | PlaceConverter.CityRule | PlaceConverter.CountryRule |
    PronounTransformer | DateConverter.FixedDate | Color | Language | CommonObjectRule
  val DirectObjectRule = AdditionalObjectTransformer | Quantity | CommonObjectRule | AdditionalPersonTransformer
}
