package com.rokuan.calliopecore.fr.autoroute.sentence

import com.rokuan.calliopecore.sentence._
import com.rokuan.calliopecore.sentence.structure.data.place.PlaceAdverbial.{PlaceContext, PlaceType}
import com.rokuan.calliopecore.sentence.structure.data.purpose.PurposeAdverbial.{PurposeContext, PurposeType}
import com.rokuan.calliopecore.sentence.structure.data.time.TimeAdverbial.{TimeContext, TimeType}
import com.rokuan.calliopecore.sentence.structure.data.way.WayAdverbial.{WayContext, WayType}

/**
  * Created by Christophe on 09/12/2016.
  */
abstract class Preposition[ContextType, FollowerType](value: String, preposition: ContextType, followers: Set[FollowerType]) extends IPreposition[ContextType] {
  override def getContext(): ContextType = preposition
  override def getValue(): String = value
  def getFollowers(): Set[FollowerType] = followers
  def canBeFollowedBy(t: FollowerType) = followers.contains(t)
}

case class PlacePreposition(value: String, preposition: PlaceContext, followers: Set[PlaceType])
  extends Preposition[PlaceContext, PlaceType](value, preposition, followers) with IPlacePreposition {
  def this(v: String, p: PlaceContext, f: PlaceType*) = this(v, p, f.toSet)
}
case class WayPreposition(value: String, preposition: WayContext, followers: Set[WayType])
  extends Preposition[WayContext, WayType](value, preposition, followers) with IWayPreposition {
  def this(v: String, p: WayContext, f: WayType*) = this(v, p, f.toSet)
}
case class TimePreposition(value: String, preposition: TimeContext, followers: Set[TimeType])
  extends Preposition[TimeContext, TimeType](value, preposition, followers) with ITimePreposition {
  def this(v: String, p: TimeContext, f: TimeType*) = this(v, p, f.toSet)
}
case class PurposePreposition(value: String, preposition: PurposeContext, followers: Set[PurposeType])
  extends Preposition[PurposeContext, PurposeType](value, preposition, followers) with IPurposePreposition {
  def this(v: String, p: PurposeContext, f: PurposeType*) = this(v, p, f.toSet)
}