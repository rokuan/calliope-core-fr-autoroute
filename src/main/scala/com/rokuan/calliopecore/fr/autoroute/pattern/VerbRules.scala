package com.rokuan.calliopecore.fr.autoroute.pattern

import com.rokuan.autoroute.rules.TerminalState
import com.rokuan.calliopecore.fr.autoroute.pattern.matcher.VerbMatcherBuilder
import com.rokuan.calliopecore.fr.autoroute.sentence.Word
import com.rokuan.calliopecore.sentence.IAction.Form

/**
  * Created by Christophe on 01/12/2016.
  */
object VerbRules {
  def verb(verb: String): TerminalState[Word] = new TerminalMatcher(new VerbMatcherBuilder()
    .setRegex(verb)
    .build())
  def verb(form: Form): TerminalState[Word] = new TerminalMatcher(new VerbMatcherBuilder()
    .setForm(form)
    .build())
}
