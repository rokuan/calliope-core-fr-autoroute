package com.rokuan.calliopecore.fr.autoroute.pattern

import com.rokuan.autoroute.rules.TerminalState
import com.rokuan.calliopecore.fr.pattern.VerbMatcher.VerbMatcherBuilder
import com.rokuan.calliopecore.fr.sentence.Word

/**
  * Created by Christophe on 01/12/2016.
  */
object VerbRules {
  def verb(verb: String): TerminalState[Word] = new TerminalMatcher(new VerbMatcherBuilder()
    .setVerbRegex(verb)
    .build())
}