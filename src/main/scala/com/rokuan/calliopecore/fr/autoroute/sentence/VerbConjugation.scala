package com.rokuan.calliopecore.fr.autoroute.sentence

import com.rokuan.calliopecore.sentence.IAction
import com.rokuan.calliopecore.sentence.IAction.{ActionType, Form, Tense}

/**
  * Created by Christophe on 09/12/2016.
  */
case class VerbConjugation(name: String, verb: Verb, tense: Verb.ConjugationTense.Value,
                           form: IAction.Form, pronoun: Verb.Pronoun.Value) extends IAction {
  def this(n: String, c: VerbConjugation, v: Verb) = this(n, v, c.tense, c.form, c.pronoun)
  override def getForm: Form = form

  override def getAction: ActionType = verb.getAction().action

  override def getTense: Tense = {
    import Verb.ConjugationTense._
    tense match {
      case PRESENT => Tense.PRESENT
      case PAST | PERFECT | IMPERFECT | PAST_PERFECT | SIMPLE_PAST | PLUPERFECT => Tense.PAST
      case FUTURE | FUTURE_ANTERIOR => Tense.FUTURE
      case _ => Tense.PRESENT
    }
  }

  override def getState: String = verb.getAction().stateValue

  override def isStateBound: Boolean = verb.getAction().isStateBound()

  override def getBoundState: String = verb.getAction().state

  override def getValue: String = name

  override def isFieldBound: Boolean = verb.getAction().isFieldBound()

  override def getBoundField: String = verb.getAction().field

  def getVerb(): Verb = verb

  override def isTargetAction: Boolean = verb.getAction().isTargetAction()
}
