package com.rokuan.calliopecore.fr.autoroute.sentence

/**
  * Created by Christophe on 09/12/2016.
  */
case class Verb(infinitive: String, auxiliary: Boolean = false, initialAction: Action = null, reflexiveAction: Action = null) {
  def this(i: String, a: Action) = this(i, false, a, null)
  private var reflexiveMode = false
  def getValue(): String = infinitive
  def isAuxiliary(): Boolean = auxiliary
  def getAction(): Action = if(reflexiveMode) reflexiveAction else initialAction
  def valorizeReflexiveMode(): Unit = reflexiveMode = true
  def existsInReflexiveForm(): Boolean = reflexiveAction != null
}

object Verb {
  type ConjugationTense = ConjugationTense.Value
  type Form = Form.Value
  type Pronoun = Pronoun.Value

  object ConjugationTense extends Enumeration {
    /*type ConjugationTense = Value*/
    val PRESENT,
    PAST,
    PERFECT, //PASSE_COMPOSE,
    IMPERFECT, //IMPARFAIT,
    PAST_PERFECT, //PLUS_QUE_PARFAIT,
    SIMPLE_PAST, //PASSE_SIMPLE,
    PLUPERFECT, //PASSE_ANTERIEUR
    FUTURE,
    FUTURE_ANTERIOR //FUTUR_ANTERIEUR
    = Value
  }

  object Form extends Enumeration {
    val INDICATIVE,
    CONDITIONAL,
    SUBJUNCTIVE,
    IMPERATIVE,
    INFINITIVE,
    PARTICIPLE = Value
  }

  object Pronoun extends Enumeration {
    val JE,
    TU,
    IL_ELLE_ON,
    NOUS,
    VOUS,
    ILS_ELLES,
    UNDEFINED = Value
  }
}