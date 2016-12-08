package com.rokuan.calliopecore.fr.autoroute.data

import com.rokuan.calliopecore.fr.sentence.{Pronoun, Word}
import com.rokuan.calliopecore.fr.structure.Question
import com.rokuan.calliopecore.sentence.ActionObject
import com.rokuan.calliopecore.sentence.structure.{AffirmationObject, InterpretationObject, OrderObject, QuestionObject}
import com.rokuan.calliopecore.sentence.structure.QuestionObject.QuestionType
import com.rokuan.calliopecore.sentence.structure.content.{INominalObject, IPlaceObject, ITimeObject, IWayObject}
import com.rokuan.calliopecore.sentence.structure.data.nominal.{AbstractTarget, NominalGroup, PronounSubject}

/**
  * Created by Christophe on 01/12/2016.
  */

case class AdverbialsInfo(when: Option[ITimeObject], where: Option[IPlaceObject], how: Option[IWayObject])

object InterpretationObjectConverter {
  import com.rokuan.calliopecore.fr.autoroute.pattern.WordRules._
  import com.rokuan.calliopecore.fr.sentence.Word.WordType._
  import com.rokuan.autoroute.rules.Rule._
  import com.rokuan.calliopecore.fr.autoroute.pattern.VerbRules._

  private def applyAdverbials(o: InterpretationObject, adverbials: AdverbialsInfo) = {
    adverbials.where.foreach(o.where = _)
    adverbials.when.foreach(o.when = _)
    adverbials.how.foreach(o.how = _)
  }
  private def createQuestionObject(t: QuestionType, verb: ActionInfo, directObject: Option[NominalGroup], adverbials: AdverbialsInfo) = {
    val question = new QuestionObject {
      questionType = t
      action = verb.action
    }
    verb.subject.foreach(question.subject = _)
    directObject.foreach(question.what = _)
    verb.target.foreach(question.target = _)
    applyAdverbials(question, adverbials)
    question
  }

  val WhatTransformer = word("quel|quels|quelle|quelles", INTERROGATIVE_ADJECTIVE) { _ => QuestionType.WHAT }
  val HowManyTransformer = word("combien") { _ => QuestionType.HOW_MANY }
  val HowTransformer = word("comment") { _ => QuestionType.HOW }
  val WhenTransformer = (word("quand") ~ word("est") ~ word("ce") ~ word("qu|que")) {
    case List(_, _, _, _) => QuestionType.WHEN
  } | word("quand") { _ => QuestionType.WHEN }
  val WhereTransformer = word("où") { _ => QuestionType.WHERE }
  val WhoTransformer = word("qui") { _ => QuestionType.WHO }

  val AdverbialsTransformer = DateConverter.TimeAdverbialRule { timeAdverbial: ITimeObject =>
    AdverbialsInfo(Some(timeAdverbial), None, None)
  } | PlaceConverter.PlaceAdverbialRule { placeAdverbial: IPlaceObject =>
    AdverbialsInfo(None, Some(placeAdverbial), None)
  } | WayConverter.WayAdverbialRule { wayAdverbial: IWayObject =>
    AdverbialsInfo(None, None, Some(wayAdverbial))
  }
  val AdverbialListTransformer = (AdverbialsTransformer*) { adverbials: List[AdverbialsInfo] =>
    val timeAdverbial = adverbials.collectFirst { case as if as.when.isDefined => as.when.get }
    val placeAdverbial = adverbials.collectFirst { case as if as.where.isDefined => as.where.get }
    val wayAdverbial = adverbials.collectFirst { case as if as.how.isDefined => as.how.get }
    AdverbialsInfo(timeAdverbial, placeAdverbial, wayAdverbial)
  }

  val OrderTransformer = (VerbConverter.VerbTransformer ~ opt(VerbConverter.TargetPronounTransformer) ~ NominalGroupConverter.DirectObjectRule) {
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

  val HowQuestion = (HowTransformer ~ VerbConverter.InfinitiveVerbRule ~ opt(NominalGroupConverter.DirectObjectRule) ~ AdverbialListTransformer){
    case List(t: QuestionType, verb: ActionInfo, directObject: Option[NominalGroup], adverbials: AdverbialsInfo) =>
      createQuestionObject(t, verb, directObject, adverbials)
  }
  val HowManyQuestion = (HowManyTransformer ~ NominalGroupConverter.DirectObjectRule ~ VerbConverter.QuestionVerb ~ AdverbialListTransformer) {
    case List(t: QuestionType, directObject: NominalGroup, verb: ActionInfo, adverbials: AdverbialsInfo) =>
      createQuestionObject(t, verb, Some(directObject), adverbials)
  }
  val WhatQuestion = (WhatTransformer ~ NominalGroupConverter.SimpleObjectOnlyRule ~ VerbConverter.QuestionVerb ~ AdverbialListTransformer) {
    case List(t: QuestionType, directObject: NominalGroup, verb: ActionInfo, adverbials: AdverbialsInfo) =>
      createQuestionObject(t, verb, Some(directObject), adverbials)
  }
  val WhenQuestion = (WhenTransformer ~ VerbConverter.IndicativeQuestionVerb ~ NominalGroupConverter.DirectObjectRule ~ AdverbialListTransformer) {
    case List(t: QuestionType, verb: ActionInfo, directObject: NominalGroup, adverbials: AdverbialsInfo) =>
      createQuestionObject(t, verb, Some(directObject), adverbials)
  }
  val WhereQuestion = (WhereTransformer ~ VerbConverter.IndicativeQuestionVerb ~ NominalGroupConverter.DirectObjectRule ~ AdverbialListTransformer) {
    case List(t: QuestionType, verb: ActionInfo, directObject: NominalGroup, adverbials: AdverbialsInfo) =>
      createQuestionObject(t, verb, Some(directObject), adverbials)
  }
  val WhoQuestion = (WhoTransformer ~ verb("être") ~ NominalGroupConverter.SubjectRule ~ AdverbialListTransformer) {
    case List(t: QuestionType, v: Word, directObject: INominalObject, adverbials: AdverbialsInfo) =>
      val question = new QuestionObject {
        questionType = t
        action = new ActionObject(v.getVerbInfo.getTense, v.getVerbInfo)
        what = directObject
      }
      applyAdverbials(question, adverbials)
      question
  } | (WhoTransformer ~ VerbConverter.AffirmativeConjugatedVerb ~ opt(NominalGroupConverter.SubjectRule) ~ AdverbialListTransformer) {
    case List(t: QuestionType, verb: ActionInfo, subject: Option[INominalObject], adverbials: AdverbialsInfo) =>
      val question = new QuestionObject {
        questionType = t
        action = verb.action
      }
      verb.target.foreach(question.target = _)
      verb.subject.foreach(question.subject = _)
      subject.foreach(question.what = _)
      applyAdverbials(question, adverbials)
      question
  }

  val AssertionRule = (NominalGroupConverter.SubjectRule ~ VerbConverter.AffirmativeConjugatedVerb ~
    opt(NominalGroupConverter.DirectObjectRule) ~ AdverbialListTransformer) {
    case List(s: INominalObject, verb: ActionInfo, directObject: Option[NominalGroup], adverbials: AdverbialsInfo) =>
      val sentence = new AffirmationObject {
        subject = s
        action = verb.action
      }
      directObject.foreach(sentence.what = _)
      verb.target.foreach(sentence.target = _)
      applyAdverbials(sentence, adverbials)
      sentence
  }

  val OrderRule = (VerbConverter.ImperativeVerbTransformer ~ word(SOURCE_PRONOUN) ~ word(TARGET_PRONOUN) ~ AdverbialListTransformer) {
    case List(verb: ActionObject, s: Word, t: Word, adverbials: AdverbialsInfo) =>
      val order = new OrderObject {
        action = verb
        target = new AbstractTarget(Pronoun.parseTargetPronoun(t.getValue))
        what = new PronounSubject(Pronoun.parseDirectPronoun(s.getValue))
      }
      applyAdverbials(order, adverbials)
      order
  } | (VerbConverter.ImperativeVerbTransformer ~ word(TARGET_PRONOUN) ~
    NominalGroupConverter.DirectObjectRule ~ AdverbialListTransformer) {
    case List(verb: ActionObject, t: Word, source: INominalObject, adverbials: AdverbialsInfo) =>
      val order = new OrderObject {
        action = verb
        what = source
        target = new AbstractTarget(Pronoun.parseTargetPronoun(t.getValue))
      }
      applyAdverbials(order, adverbials)
      order
  } | (VerbConverter.ImperativeVerbTransformer ~ NominalGroupConverter.DirectObjectRule ~ AdverbialListTransformer) {
    case List(verb: ActionObject, directObject: INominalObject, adverbials: AdverbialsInfo) =>
      val order = new OrderObject {
        action = verb
        what = directObject
      }
      applyAdverbials(order, adverbials)
      order
  } | (VerbConverter.ImperativeVerbTransformer ~ word(SOURCE_PRONOUN) ~ AdverbialListTransformer) {
    case List(verb: ActionObject, source: Word, adverbials: AdverbialsInfo) =>
      val order = new OrderObject {
        action = verb
        what = new AbstractTarget(Pronoun.parseDirectPronoun(source.getValue))
      }
      applyAdverbials(order, adverbials)
      order
  }

  val QuestionRule = HowQuestion | HowManyQuestion |
    WhatQuestion | WhereQuestion | WhenQuestion | WhoQuestion

  val InterpretationObjectRule = QuestionRule | AssertionRule | OrderRule
}
