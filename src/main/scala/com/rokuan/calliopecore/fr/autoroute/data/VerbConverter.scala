package com.rokuan.calliopecore.fr.autoroute.data

import com.rokuan.calliopecore.fr.data.nominal.PronounObject
import com.rokuan.calliopecore.fr.sentence._
import com.rokuan.calliopecore.sentence.ActionObject
import com.rokuan.calliopecore.sentence.IAction.{ActionType, Tense}
import com.rokuan.calliopecore.sentence.IPronoun.PronounSource
import com.rokuan.calliopecore.sentence.structure.data.nominal.PronounSubject

/**
  * Created by Christophe on 01/12/2016.
  */
object VerbConverter {
  import com.rokuan.calliopecore.fr.sentence.Word.WordType._
  import com.rokuan.calliopecore.fr.autoroute.pattern.WordRules._
  import com.rokuan.autoroute.rules.Rule._
  import com.rokuan.calliopecore.fr.autoroute.pattern.VerbRules._

  val TargetPronounTransformer = word(TARGET_PRONOUN) { p =>
    new PronounSubject(Pronoun.parseTargetPronoun(p.getValue))
  }
  val ReflexivePronounTransformer = word(REFLEXIVE_PRONOUN) { p =>
    new PronounObject(p.getValue, p.getTypes)
  }
  val SourcePronounTransformer = word(SOURCE_PRONOUN) { p =>

  }
  val PersonalPronounTransformer = word(PERSONAL_PRONOUN) { p =>
    new PronounSubject(Pronoun.parseSubjectPronoun(p.getValue))
  }

  val PronounTransformer = ReflexivePronounTransformer
  val VerbTransformer = word(VERB) { v =>
    val verbInfo = v.getVerbInfo
    new ActionObject(verbInfo.getTense, verbInfo)
  }
  val AffirmativeVerbTransformer = (word(AUXILIARY) ~ word(VERB)){
    case List(aux: Word, verb: Word) =>
      val verbInfo = verb.getVerbInfo
      new ActionObject(Tense.PAST, verbInfo)
  } | VerbTransformer

  val AffirmativeConjugatedVerb = (opt(PronounTransformer) ~ AffirmativeVerbTransformer) {
    case List(pronoun: Option[PronounObject], action: ActionObject) =>
      // TODO:
  }

  val PastQuestionVerb = (opt(TargetPronounTransformer) ~ word(AUXILIARY) ~
    opt(word(CONJUGATION_LINK)) ~ PersonalPronounTransformer ~ VerbTransformer) {
    case List(target: Option[PronounSubject], _, _, subject: PronounSubject, verb: ActionObject) =>
      (subject, target, verb)
  }
  val IndicativeQuestionVerb = (opt(TargetPronounTransformer) ~ VerbTransformer ~
    opt(word(CONJUGATION_LINK)) ~ PersonalPronounTransformer){
    case List(target: Option[PronounSubject], verb: ActionObject, _, subject: PronounSubject) =>
      (subject, target, verb)
  }
  val IsThereQuestionVerb = (word("y") ~ verb("avoir") ~ opt(word(CONJUGATION_LINK)) ~
    word("il")) {
    case (_, v: Word,_, pronoun: Word) =>
      // TODO: find the right subject
      val conjugation = new VerbConjugation("y " + v.getValue, v.getVerbInfo.asInstanceOf[VerbConjugation],
        new Verb("y avoir", false, new Action(ActionType.THERE_IS)))
      val action = new ActionObject(conjugation.getTense, conjugation)
      val subject = new PronounSubject(new Pronoun("", PronounSource.UNDEFINED))
      (subject, Option.empty[PronounSubject], action)
  }
  val QuestionVerb = PastQuestionVerb | IndicativeQuestionVerb | IsThereQuestionVerb

}
