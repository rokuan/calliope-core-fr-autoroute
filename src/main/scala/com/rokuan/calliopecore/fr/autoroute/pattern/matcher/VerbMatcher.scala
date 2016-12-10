package com.rokuan.calliopecore.fr.autoroute.pattern.matcher

import com.rokuan.calliopecore.fr.autoroute.sentence.{VerbConjugation, Word}
import com.rokuan.calliopecore.pattern.WordMatcher
import com.rokuan.calliopecore.fr.autoroute.sentence.Word.WordType._
import com.rokuan.calliopecore.sentence.IAction.Form

/**
  * Created by Christophe on 09/12/2016.
  */
class VerbMatcher(val auxiliary: Boolean, val verbRegex: String, val conjugationRegex: String, val form: Form) extends WordMatcher[Word] {
  override def matches(w: Word): Boolean = {
    if(!w.isOfType(VERB)){
      false
    } else if(auxiliary && !w.isOfType(AUXILIARY)){
      false
    } else if(Option(conjugationRegex).map(r => !w.getValue.matches(r)).getOrElse(true)) {
      false
    } else if(Option(verbRegex).map(r => !w.getVerbInfo.asInstanceOf[VerbConjugation].getVerb().getValue().matches(r)).getOrElse(true)) {
      false
    } else if(Option(form).map(f => w.getVerbInfo.getForm != f).getOrElse(true)) {
      false
    } else {
      true
    }
  }
}

class VerbMatcherBuilder {
  private var auxiliary: Boolean = false
  private var infinitive: String = null
  private var conjugation: String = null
  private var form: Form = null

  def setRegex(r: String) = { infinitive = r; this }
  def setAuxiliary(a: Boolean) = { auxiliary = a; this }
  def setConjugationRegex(r: String) = { conjugation = r; this }
  def setForm(f: Form) = { form = f; this }
  def build() = new VerbMatcher(auxiliary, infinitive, conjugation, form)
}
