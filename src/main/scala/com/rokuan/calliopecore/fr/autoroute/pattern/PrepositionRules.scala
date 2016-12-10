package com.rokuan.calliopecore.fr.autoroute.pattern

import com.rokuan.autoroute.rules.TerminalState
import com.rokuan.calliopecore.fr.autoroute.pattern.matcher.{PlacePrepositionMatcherBuilder, PurposePrepositionMatcherBuilder, TimePrepositionMatcherBuilder, WayPrepositionMatcherBuilder}
import com.rokuan.calliopecore.fr.autoroute.sentence.Word
import com.rokuan.calliopecore.sentence.structure.data.place.PlaceAdverbial.PlaceType
import com.rokuan.calliopecore.sentence.structure.data.purpose.PurposeAdverbial.PurposeType
import com.rokuan.calliopecore.sentence.structure.data.time.TimeAdverbial.TimeType
import com.rokuan.calliopecore.sentence.structure.data.way.WayAdverbial.WayType

/**
  * Created by Christophe on 27/11/2016.
  */
object PrepositionRules {
  def prep(t: PlaceType): TerminalState[Word] = prep(t, false)
  def prep(t: PlaceType, contracted: Boolean): TerminalState[Word] = new TerminalMatcher(new PlacePrepositionMatcherBuilder()
    .setPossibleFollowers(t)
    .setMatchContractedForm(contracted)
    .build())
  def prep(t: TimeType): TerminalState[Word] = prep(t, false)
  def prep(t: TimeType, contracted: Boolean): TerminalState[Word] = new TerminalMatcher(new TimePrepositionMatcherBuilder()
    .setPossibleFollowers(t)
    .setMatchContractedForm(contracted)
    .build())
  def prep(t: PurposeType): TerminalState[Word] = prep(t, false)
  def prep(t: PurposeType, contracted: Boolean): TerminalState[Word] = new TerminalMatcher(new PurposePrepositionMatcherBuilder()
    .setPossibleFollowers(t)
    .setMatchContractedForm(contracted)
    .build())
  def prep(t: WayType): TerminalState[Word] = prep(t, false)
  def prep(t: WayType, contracted: Boolean): TerminalState[Word] = new TerminalMatcher(new WayPrepositionMatcherBuilder()
    .setPossibleFollowers(t)
    .setMatchContractedForm(contracted)
    .build())
}

