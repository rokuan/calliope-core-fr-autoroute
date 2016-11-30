package com.rokuan.calliopecore.fr.autoroute.data

import com.rokuan.calliopecore.fr.sentence.Word
import com.rokuan.calliopecore.sentence.INameInfo
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

  /*val AbstractTarget = word(POSSESSIVE_ADJECTIVE) {

  }*/
  val NumberTransformer = word(NUMBER) { _.getValue.toInt }
  val DoubleTransformer = word(NUMBER) { _.getValue.toDouble }
  val UnitTransformer = word(UNIT) { _.getUnitInfo.getUnitType }

  val FirstName = list(word(FIRSTNAME))
  val LastName = list(word(PROPER_NAME))

  val CommonObjectTransformer = word(COMMON_NAME) { _.getNameInfo }

  val SimpleObject = (CountConverter.CountRule ~ CommonObjectTransformer) {
    case List(count: CountObject, name: INameInfo) =>
      new NameObject {
        count = count
        `object` = name
      }
  }
  val PhoneNumber = (word(DEFINITE_ARTICLE) ~ list(NumberTransformer)) {
    case List(_, numbers: List[Int]) =>
      // TODO: How to handle numbers such as "099" ?
      val phoneNumber = numbers.map {
        case n if n < 100 => String.format("%2d", n)
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
  val AdditionalPerson = word(PERSON) { w => new AdditionalPerson { person = w.getCustomPerson } }
  val Language = (word(DEFINITE_ARTICLE) ~ word(LANGUAGE)) {
    case List(_, l: Word) =>
      new LanguageObject { language = l.getLanguageInfo }
  }
  val DirectObject = opt(CountConverter.CountRule) ~ word(COMMON_NAME)
  val Pronoun = word(PERSONAL_PRONOUN) { w => new PronounSubject(sentence.Pronoun.parseSubjectPronoun(w.getValue))}

  val SubjectRule = Quantity | PhoneNumber | PlaceConverter.City | PlaceConverter.Country |
    Pronoun | DateConverter.FixedDate | Color | Language | SimpleObject
}
