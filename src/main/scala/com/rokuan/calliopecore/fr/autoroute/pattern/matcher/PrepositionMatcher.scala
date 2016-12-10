package com.rokuan.calliopecore.fr.autoroute.pattern.matcher

import com.rokuan.calliopecore.fr.autoroute.sentence.Word.WordType
import com.rokuan.calliopecore.fr.autoroute.sentence._
import com.rokuan.calliopecore.pattern.WordMatcher
import com.rokuan.calliopecore.sentence.structure.data.place.PlaceAdverbial.PlaceType
import com.rokuan.calliopecore.sentence.structure.data.purpose.PurposeAdverbial.PurposeType
import com.rokuan.calliopecore.sentence.structure.data.time.TimeAdverbial.TimeType
import com.rokuan.calliopecore.sentence.structure.data.way.WayAdverbial.WayType

import scala.reflect.ClassTag

/**
  * Created by Christophe on 09/12/2016.
  */
abstract class PrepositionMatcher[FollowerType](val contracted: Boolean, val followers: List[FollowerType]) extends WordMatcher[Word] {
  override def matches(w: Word): Boolean = {
    if (contracted && !w.isOfType(WordType.CONTRACTED)) {
      false
    } else {
      val prep = getPreposition(w)
      Option(prep).map(p => followers.forall(p.canBeFollowedBy))
        .getOrElse(false)
    }
  }

  def getPreposition(w: Word): Preposition[_, FollowerType]
}

abstract class PrepositionMatcherBuilder[FollowerType] {
  private var contracted = false
  private var followers: List[FollowerType] = List[FollowerType]()

  def setMatchContractedForm(c: Boolean) = { contracted = c; this }
  def setPossibleFollowers(ts: FollowerType*) = { followers = ts.toList; this }
  def build(): PrepositionMatcher[FollowerType] = new PrepositionMatcher[FollowerType](contracted, followers) {
    override def getPreposition(w: Word): Preposition[_, FollowerType] = getBuilderPreposition(w)
  }
  def getBuilderPreposition(w: Word): Preposition[_, FollowerType]
}

class PlacePrepositionMatcherBuilder extends PrepositionMatcherBuilder[PlaceType] {
  override def getBuilderPreposition(w: Word): Preposition[_, PlaceType] = w.getPlacePreposition.asInstanceOf[PlacePreposition]
}

class TimePrepositionMatcherBuilder extends PrepositionMatcherBuilder[TimeType] {
  override def getBuilderPreposition(w: Word): Preposition[_, TimeType] = w.getTimePreposition.asInstanceOf[TimePreposition]
}

class WayPrepositionMatcherBuilder extends PrepositionMatcherBuilder[WayType] {
  override def getBuilderPreposition(w: Word): Preposition[_, WayType] = w.getWayPreposition.asInstanceOf[WayPreposition]
}

class PurposePrepositionMatcherBuilder extends PrepositionMatcherBuilder[PurposeType] {
  override def getBuilderPreposition(w: Word): Preposition[_, PurposeType] = w.getPurposePreposition.asInstanceOf[PurposePreposition]
}