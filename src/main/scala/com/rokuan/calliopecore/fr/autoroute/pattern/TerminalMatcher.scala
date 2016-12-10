package com.rokuan.calliopecore.fr.autoroute.pattern

import com.rokuan.autoroute.rules.TerminalState
import com.rokuan.calliopecore.fr.autoroute.sentence.Word
import com.rokuan.calliopecore.pattern.WordMatcher

/**
  * Created by Christophe on 27/11/2016.
  */
class TerminalMatcher(val m: WordMatcher[Word]) extends TerminalState[Word] {
  override def valueMatches(t: Word): Boolean = m.matches(t)
}
