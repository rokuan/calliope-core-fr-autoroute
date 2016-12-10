package com.rokuan.calliopecore.fr.autoroute.sentence

import com.rokuan.calliopecore.fr.autoroute.sentence.Word.WordType
import PronounMasks._

/**
  * Created by Christophe on 09/12/2016.
  */
class PronounObject(val value: String, val types: Set[WordType]) {
  val typesMask = {
    val source = if(types.contains(WordType.SOURCE_PRONOUN)) SourceMask else 0
    val target = if(types.contains(WordType.TARGET_PRONOUN)) TargetMask else 0
    val reflexive = if(types.contains(WordType.REFLEXIVE_PRONOUN)) ReflexiveMask else 0
    source | target | reflexive
  }

  def this(v: String, ts: WordType*) = this(v, ts.toSet)

  def isSource() = (typesMask & SourceMask) != 0
  def isTarget() = (typesMask & TargetMask) != 0
  def isReflexive() = (typesMask & ReflexiveMask) != 0
  def getSourceForm() = DirectPronoun(value)
  def getTargetForm() = TargetPronoun(value)
  def getReflexiveForm() = ReflexivePronoun(value)
}

object PronounMasks {
  val SourceMask = 0x1
  val TargetMask = 0x10
  val ReflexiveMask = 0x100
}