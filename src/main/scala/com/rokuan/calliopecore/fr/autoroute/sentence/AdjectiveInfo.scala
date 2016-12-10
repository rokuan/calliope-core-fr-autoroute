package com.rokuan.calliopecore.fr.autoroute.sentence

import com.rokuan.calliopecore.sentence.IAdjectiveInfo
import com.rokuan.calliopecore.sentence.IAdjectiveInfo.AdjectiveValue

/**
  * Created by Christophe on 09/12/2016.
  */
case class AdjectiveInfo(value: String, adjectiveType: AdjectiveValue, field: String, state: String, stateValue: String) extends IAdjectiveInfo {
  def this(v: String, f: FieldProperty, s: StateProperty) = this(v, null, Option(f).map(_.field).orNull,
    Option(s).map(_.state).orNull, Option(s).map(_.value).orNull)

  override def getAdjectiveType: AdjectiveValue = adjectiveType
  override def getValue: String = value
  override def getState: String = stateValue
  override def isStateBound: Boolean = state != null && stateValue != null
  override def getBoundState: String = state
  override def isFieldBound: Boolean = field != null
  override def getBoundField: String = field
}
