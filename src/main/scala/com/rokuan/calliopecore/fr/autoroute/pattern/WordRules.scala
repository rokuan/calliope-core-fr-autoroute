package com.rokuan.calliopecore.fr.autoroute.pattern

import com.rokuan.autoroute.rules.TerminalState
import com.rokuan.calliopecore.fr.pattern.SimpleWordMatcher
import com.rokuan.calliopecore.fr.sentence.Word
import com.rokuan.calliopecore.fr.sentence.Word.WordType

/**
  * Created by Christophe on 27/11/2016.
  */
object WordRules {
  def word(w: WordType*): TerminalState[Word] = new TerminalMatcher(SimpleWordMatcher.builder().setTypes(w: _*).build())

  def word(v: String): TerminalState[Word] = new TerminalMatcher(SimpleWordMatcher.builder().setWordRegex(v).build())

  def word(v: String, w: WordType*): TerminalState[Word] = new TerminalMatcher(SimpleWordMatcher.builder()
    .setTypes(w: _*)
    .setWordRegex(v)
    .build())
}
