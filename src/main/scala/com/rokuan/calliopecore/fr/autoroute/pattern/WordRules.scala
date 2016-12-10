package com.rokuan.calliopecore.fr.autoroute.pattern

import com.rokuan.autoroute.rules.TerminalState
import com.rokuan.calliopecore.fr.autoroute.pattern.matcher.SimpleWordMatcherBuilder
import com.rokuan.calliopecore.fr.autoroute.sentence.Word
import com.rokuan.calliopecore.fr.autoroute.sentence.Word.WordType

/**
  * Created by Christophe on 27/11/2016.
  */
object WordRules {
  def word(w: WordType*): TerminalState[Word] = new TerminalMatcher(new SimpleWordMatcherBuilder().setTypes(w: _*).build())

  def word(v: String): TerminalState[Word] = new TerminalMatcher(new SimpleWordMatcherBuilder().setRegex(v).build())

  def word(v: String, w: WordType*): TerminalState[Word] = new TerminalMatcher(new SimpleWordMatcherBuilder()
    .setTypes(w: _*)
    .setRegex(v)
    .build())
}
