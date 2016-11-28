package com.rokuan.calliopecore.fr.autoroute.pattern

import com.rokuan.autoroute.rules.{Rule, TerminalState}
import com.rokuan.calliopecore.fr.pattern._
import com.rokuan.calliopecore.fr.sentence.Word
import com.rokuan.calliopecore.sentence.structure.data.place.PlaceAdverbial.PlaceType
import com.rokuan.calliopecore.sentence.structure.data.purpose.PurposeAdverbial.PurposeType
import com.rokuan.calliopecore.sentence.structure.data.time.TimeAdverbial.TimeType
import com.rokuan.calliopecore.sentence.structure.data.way.WayAdverbial.WayType

/**
  * Created by Christophe on 27/11/2016.
  */
object PrepositionRules {
  def prep(t: PlaceType): TerminalState[Word] = prep(t, false)
  def prep(t: PlaceType, contracted: Boolean): TerminalState[Word] = new TerminalMatcher(new PlacePrepositionMatcher().getBuilder
    .setPossibleFollowers(Array(t))
    .setMatchContractedForm(contracted)
    .build())
  def prep(t: TimeType): TerminalState[Word] = prep(t, false)
  def prep(t: TimeType, contracted: Boolean): TerminalState[Word] = new TerminalMatcher(new TimePrepositionMatcher().getBuilder
    .setPossibleFollowers(Array(t))
    .setMatchContractedForm(contracted)
    .build())
  def prep(t: PurposeType): TerminalState[Word] = prep(t, false)
  def prep(t: PurposeType, contracted: Boolean): TerminalState[Word] = new TerminalMatcher(new PurposePrepositionMatcher().getBuilder
    .setPossibleFollowers(Array(t))
    .setMatchContractedForm(contracted)
    .build())
  def prep(t: WayType): TerminalState[Word] = prep(t, false)
  def prep(t: WayType, contracted: Boolean): TerminalState[Word] = new TerminalMatcher(new WayPrepositionMatcher().getBuilder
    .setPossibleFollowers(Array(t))
    .setMatchContractedForm(contracted)
    .build())

  /*def prep(t: PlaceType): TerminalState[Word] = prep(t, false)
  def prep(t: PlaceType, contracted: Boolean): TerminalState[Word] = prep[PlaceType](t, contracted, new PlacePrepositionMatcher())
  def prep(t: TimeType): TerminalState[Word] = prep(t, false)
  def prep(t: TimeType, contracted: Boolean): TerminalState[Word] = prep[TimeType](t, contracted, new TimePrepositionMatcher())
  def prep(t: PurposeType): TerminalState[Word] = prep(t, false)
  def prep(t: PurposeType, contracted: Boolean): TerminalState[Word] = prep[PurposeType](t, contracted, new PurposePrepositionMatcher())
  def prep(t: WayType): TerminalState[Word] = prep(t, false)
  def prep(t: WayType, contracted: Boolean): TerminalState[Word] = prep[WayType](t, contracted, new WayPrepositionMatcher())

  private def prep[FollowerType](t: FollowerType, contracted: Boolean, matcher: PrepositionMatcher[FollowerType]): TerminalState[Word] =
    new TerminalMatcher(matcher.getBuilder.setMatchContractedForm(contracted)
      .setPossibleFollowers(Array(t))
      .build())*/
}

