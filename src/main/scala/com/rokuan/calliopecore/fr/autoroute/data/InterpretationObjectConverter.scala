package com.rokuan.calliopecore.fr.autoroute.data

import com.rokuan.calliopecore.fr.structure.Question
import com.rokuan.calliopecore.sentence.ActionObject
import com.rokuan.calliopecore.sentence.structure.OrderObject
import com.rokuan.calliopecore.sentence.structure.QuestionObject.QuestionType
import com.rokuan.calliopecore.sentence.structure.content.INominalObject
import com.rokuan.calliopecore.sentence.structure.data.nominal.PronounSubject

/**
  * Created by Christophe on 01/12/2016.
  */
object InterpretationObjectConverter {
  import com.rokuan.calliopecore.fr.autoroute.pattern.WordRules._
  import com.rokuan.calliopecore.fr.sentence.Word.WordType._
  import com.rokuan.autoroute.rules.Rule._

  val QuestionObjectTransformer = {

  }
  val WhatTransformer = word("quel|quels|quelle|quelles", INTERROGATIVE_ADJECTIVE) { _ => QuestionType.WHAT }
  val HowManyTransformer = word("combien") { _ => QuestionType.HOW_MANY }
  val HowTransformer = word("comment") { _ => QuestionType.HOW }
  val WhenTransformer = (word("quand") ~ word("est") ~ word("ce") ~ word("que")) {
    case List(_, _, _, _) => QuestionType.WHEN
  } | word("quand") { _ => QuestionType.WHEN }
  val WhereTransformer = word("où") { _ => QuestionType.WHERE }

  val OrderTransformer = (VerbConverter.VerbTransformer ~ opt(VerbConverter.TargetPronounTransformer) ~ NominalGroupConverter.DirectObject) {
    case List(action: ActionObject, target: Option[PronounSubject], directObject: INominalObject) =>
      val order = new OrderObject
      target.foreach(t => order.target = t)
      order.what = directObject
      order
  }
  val InterrogativePrefixTransformer = word(INTERROGATIVE_PRONOUN) { q =>
    Question.parseInterrogativePronoun(q.getValue)
  } | word(INTERROGATIVE_ADJECTIVE) { q =>
    Question.parseInterrogativePronoun(q.getValue)
  }

  val HowQuestion = HowTransformer
  val HowManyQuestion = HowManyTransformer
  val WhatQuestion = WhatTransformer
  val WhenQuestion = WhenTransformer
  val WhereQuestion = WhereTransformer
  val InterrogativeTransformer = HowQuestion | HowManyQuestion |
    WhatQuestion | WhereQuestion /*| WhenQuestion*/
}
