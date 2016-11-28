package com.rokuan.calliopecore.fr.autoroute.data

import com.rokuan.calliopecore.fr.sentence.Word
import com.rokuan.calliopecore.sentence.structure.data.count.CountObject
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

  val FirstName = list(word(FIRSTNAME))
  val LastName = list(word(PROPER_NAME))

  val Color = word(DEFINITE_ARTICLE) ~ word(COLOR) {
    case List(_, c: Word) =>
      new ColorObject { color = c.getColorInfo }
  }

  val CommonName = CountConverter.CountRule ~ word(COMMON_NAME) {
    case List(count: CountObject, n: Word) =>
      new NameObject {
        count = count
        `object` = n.getNameInfo
      }
  }

  val AdditionalPerson = word(PERSON) { w => new AdditionalPerson { person = w.getCustomPerson } }

  val Language = word(DEFINITE_ARTICLE) ~ word(LANGUAGE) {
    case List(_, l: Word) =>
      new LanguageObject { language = l.getLanguageInfo }
  }

  val DirectObject = opt(CountConverter.CountRule) ~ word(COMMON_NAME)

  val Pronoun = word(PERSONAL_PRONOUN) { w => new PronounSubject(sentence.Pronoun.parseSubjectPronoun(w.getValue))}
}
