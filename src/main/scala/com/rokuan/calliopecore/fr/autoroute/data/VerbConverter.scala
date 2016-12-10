package com.rokuan.calliopecore.fr.autoroute.data

import com.rokuan.calliopecore.fr.autoroute.sentence._
import com.rokuan.calliopecore.sentence.ActionObject
import com.rokuan.calliopecore.sentence.IAction.{ActionType, Form, Tense}
import com.rokuan.calliopecore.sentence.IPronoun.PronounSource
import com.rokuan.calliopecore.sentence.structure.content.INominalObject
import com.rokuan.calliopecore.sentence.structure.data.nominal.{AbstractTarget, PronounSubject}

/**
  * Created by Christophe on 01/12/2016.
  */
case class ActionInfo(subject: Option[INominalObject], target: Option[INominalObject], action: ActionObject)

object ActionInfo {
  implicit def tupleToActionInfo(values: (Option[INominalObject], Option[INominalObject], ActionObject)) =
    ActionInfo(values._1, values._2, values._3)
}

object VerbConverter {
  import com.rokuan.calliopecore.fr.autoroute.sentence.Word.WordType._
  import com.rokuan.calliopecore.fr.autoroute.pattern.WordRules._
  import com.rokuan.autoroute.rules.Rule._
  import com.rokuan.calliopecore.fr.autoroute.pattern.VerbRules._

  val TargetPronounTransformer = word(TARGET_PRONOUN) { p =>
    new PronounSubject(TargetPronoun(p.getValue))
  }
  val ReflexivePronounTransformer = word(REFLEXIVE_PRONOUN) { p =>
    new PronounObject(p.getValue, p.getTypes)
  }
  val SourcePronounTransformer = word(SOURCE_PRONOUN) { p =>

  }
  val PersonalPronounTransformer = word(PERSONAL_PRONOUN) { p =>
    new PronounSubject(SubjectPronoun(p.getValue))
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
      val mainVerb = action.getMainAction.asInstanceOf[VerbConjugation]
        .getVerb
      val target = pronoun.collect {
        case _ if mainVerb.existsInReflexiveForm() =>
          // TODO: check that the subject is the same source as the pronoun
          mainVerb.valorizeReflexiveMode()
          None
        case p if p.isTarget && !p.isSource =>
          Some(new AbstractTarget(p.getTargetForm))
      }.getOrElse {
        None
      }
    ActionInfo(None, target, action)
  }

  val PastQuestionVerb = (opt(TargetPronounTransformer) ~ word(AUXILIARY) ~
    opt(word(CONJUGATION_LINK)) ~ PersonalPronounTransformer ~ VerbTransformer) {
    case List(target: Option[PronounSubject], _, _, subject: PronounSubject, verb: ActionObject) =>
      ActionInfo(Some(subject), target, verb)
  }
  val IndicativeQuestionVerb = (opt(TargetPronounTransformer) ~ VerbTransformer ~
    opt(word(CONJUGATION_LINK)) ~ PersonalPronounTransformer){
    case List(target: Option[PronounSubject], verb: ActionObject, _, subject: PronounSubject) =>
      ActionInfo(Some(subject), target, verb)
  }
  val ImperativeVerbTransformer = /*verb(Form.IMPERATIVE)*/ word(VERB) { v =>
    new ActionObject(Tense.PRESENT, v.getVerbInfo)
  }
  val InfinitiveVerbTransformer = (word("avoir") ~ VerbTransformer) {
    case List(aux: Word, verb: ActionObject) => verb // TODO:
      new ActionObject(Tense.PAST, verb.getMainAction, verb.getPrefixActions)
  } | verb(Form.INFINITIVE) { v =>
    new ActionObject(Tense.PRESENT, v.getVerbInfo)
  }
  val InfinitiveVerbRule = (opt(TargetPronounTransformer) ~ InfinitiveVerbTransformer) {
    case List(target: Option[PronounSubject], verb: ActionObject) =>
      val subject = new PronounSubject(new Pronoun("", PronounSource.UNDEFINED))
      ActionInfo(Some(subject), target, verb)
  }
  val IsThereQuestionVerb = (word("y") ~ verb("avoir") ~ opt(word(CONJUGATION_LINK)) ~
    word("il")) {
    case List(_, v: Word,_, pronoun: Word) =>
      // TODO: find the right subject
      val conjugation = new VerbConjugation("y " + v.getValue, v.getVerbInfo.asInstanceOf[VerbConjugation],
        new Verb("y avoir", new Action(ActionType.THERE_IS)))
      val action = new ActionObject(conjugation.getTense, conjugation)
      val subject = new PronounSubject(new Pronoun("", PronounSource.UNDEFINED))
      ActionInfo(Some(subject), Option.empty[PronounSubject], action)
  }
  val QuestionVerb = PastQuestionVerb | IndicativeQuestionVerb | IsThereQuestionVerb
  val HowQuestionVerb = PastQuestionVerb | InfinitiveVerbRule
}
