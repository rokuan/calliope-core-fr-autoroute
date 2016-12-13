package com.rokuan.calliopecore.fr.autoroute.pattern.matcher

import com.rokuan.calliopecore.fr.autoroute.sentence.Word
import com.rokuan.calliopecore.fr.autoroute.sentence.Word.WordType
import com.rokuan.calliopecore.pattern.WordMatcher

/**
  * Created by Christophe on 09/12/2016.
  */
class SimpleWordMatcher(val types: Array[WordType.Value], val regex: String) extends WordMatcher[Word] {
  override def matches(w: Word): Boolean = {
    types.forall(w.isOfType) && Option(regex).map(w.getValue.matches).getOrElse(true)
  }
}

class SimpleWordMatcherBuilder {
  private var types: Array[WordType.Value] = Array()
  private var regex: String = null

  def setTypes(wordTypes: WordType.Value*) = { types = wordTypes.toArray; this }
  def setRegex(r: String) = { regex = r; this }
  def build() = new SimpleWordMatcher(types, regex)
}
