package com.rokuan.calliopecore.fr.autoroute.sentence

import com.rokuan.calliopecore.sentence.IPronoun
import com.rokuan.calliopecore.sentence.IPronoun.PronounSource
import com.rokuan.calliopecore.sentence.structure.QuestionObject.QuestionType

/**
  * Created by Christophe on 09/12/2016.
  */
object Question {
  def apply(value: String) = value match {
    case _ if value.startsWith("quel") || value.startsWith("que") || value.startsWith("qu") => QuestionType.WHAT
    case "quand" => QuestionType.WHEN
    case "où" => QuestionType.WHERE
    case "pourquoi" => QuestionType.WHY
    case "qui" => QuestionType.WHO
    case "combien" => QuestionType.HOW_MANY
    case "comment" => QuestionType.HOW
    case _ if value.startsWith("lequel") || value.startsWith("laquelle") || value.startsWith("lesquel") => QuestionType.WHICH
    case _ => QuestionType.YES_NO
  }
}

object Pronoun {
  def parseSubjectPronoun(s: String) = s match {
    case "j" | "je" => PronounSource.I
    case "tu" => PronounSource.YOU
    case "il" => PronounSource.HE
    case "elle" => PronounSource.SHE
    case "on" | "nous" => PronounSource.WE
    case "vous" => PronounSource.YOU_
    case "ils" => PronounSource.HE_
    case "elles" => PronounSource.SHE_
    case _ => PronounSource.UNDEFINED
  }

  def parseDirectPronoun(s: String) = s match {
    case "m" | "me" => PronounSource.I
    case "t" | "te" => PronounSource.YOU
    case "le" | "la" => PronounSource.HE_SHE
    case "nous" => PronounSource.WE
    case "vous" => PronounSource.YOU_
    case "les" => PronounSource.THEY
    case _ => PronounSource.UNDEFINED
  }

  def parseTargetPronoun(s: String) = s match {
    case "m" | "me" | "moi" => PronounSource.I
    case "t" | "te" | "toi" => PronounSource.YOU
    case "s" | "lui" => PronounSource.HE_SHE
    case "nous" => PronounSource.WE
    case "vous" => PronounSource.YOU_
    case _ => PronounSource.UNDEFINED
  }

  def parseReflexivePronoun(s: String) = s match {
    case "m" | "me" => PronounSource.I
    case "t" | "te" => PronounSource.YOU
    case "s" | "se" => PronounSource.HE_SHE // TODO: what to do when "se" is related to ils/elles ?
    case "nous" => PronounSource.WE
    case "vous" => PronounSource.YOU_
    case _ => PronounSource.UNDEFINED
  }

  def parsePossessivePronoun(s: String) = s match {
    case "mon" | "ma" | "mes" => PronounSource.I
    case "ton" | "ta" | "tes" => PronounSource.YOU
    case "son" | "sa" | "ses" => PronounSource.HE_SHE
    case "notre" | "nos" => PronounSource.WE
    case "votre" | "vos" => PronounSource.YOU_
    case "leur" | "leurs" => PronounSource.THEY
    case _ => PronounSource.UNDEFINED
  }
}

class Pronoun(val value: String, val source: PronounSource) extends IPronoun {
  override def getSource: PronounSource = source
  override def getValue: String = value
}

case class SubjectPronoun(v: String) extends Pronoun(v, Pronoun.parseSubjectPronoun(v))
case class DirectPronoun(v: String) extends Pronoun(v, Pronoun.parseDirectPronoun(v))
case class TargetPronoun(v: String) extends Pronoun(v, Pronoun.parseTargetPronoun(v))
case class ReflexivePronoun(v: String) extends Pronoun(v, Pronoun.parseReflexivePronoun(v))
case class PossessivePronoun(v: String) extends Pronoun(v, Pronoun.parsePossessivePronoun(v))
