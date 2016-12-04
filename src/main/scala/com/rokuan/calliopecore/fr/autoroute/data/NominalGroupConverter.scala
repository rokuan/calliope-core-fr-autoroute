package com.rokuan.calliopecore.fr.autoroute.data

import com.rokuan.calliopecore.fr.sentence.Word
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

  /*val AbstractTarget = word(POSSESSIVE_ADJECTIVE) {

  }*/
  val NumberTransformer = word(NUMBER) { _.getValue.toInt }
  val DoubleTransformer = word(NUMBER) { _.getValue.toDouble }
  val UnitTransformer = word(UNIT) { _.getUnitInfo.getUnitType }
  val CustomObjectTransformer = word(OBJECT) { _.getCustomObject }

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
  val PersonTransformer = (list(word(FIRSTNAME)) ~ opt(list(word(PROPER_NAME)))) {
    case List(firstNames: List[Word], lastNames: Option[List[Word]]) =>
      val names = firstNames.map(_.getValue) ++ lastNames.getOrElse(List()).map(_.getValue)
      new PersonObject(names.mkString(" "))
  }
  val AdditionalPersonTransformer = word(PERSON) { w => new AdditionalPerson { person = w.getCustomPerson } }
  val PersonObjectRule = AdditionalPersonTransformer | PersonTransformer
  val Language = (word(DEFINITE_ARTICLE) ~ word(LANGUAGE)) {
    case List(_, l: Word) =>
      new LanguageObject { language = l.getLanguageInfo }
  }
  val CommonObjectRule = (opt(CountConverter.CountRule) ~ CommonObjectTransformer) {
    case List(count: Option[CountObject], commonName: INameInfo) =>
      val o = new NameObject {
        `object` = commonName
      }
      count.foreach(o.count = _)
      o
  }
  val PronounTransformer = word(PERSONAL_PRONOUN) { w => new PronounSubject(sentence.Pronoun.parseSubjectPronoun(w.getValue))}

  val SimpleObjectOnlyRule = word(COMMON_NAME) { w =>
    new NameObject { `object` = w.getNameInfo }
  } | word(OBJECT) { w =>
    new AdditionalObject { `object` = w.getCustomObject }
  }

  val SubjectRule = Quantity | PhoneNumber | PlaceConverter.CityRule | PlaceConverter.CountryRule |
    PronounTransformer | DateConverter.FixedDate | Color | Language | SimpleObject
  val DirectObjectRule = AdditionalObjectTransformer | Quantity | CommonObjectRule | AdditionalPersonTransformer
}
