package com.rokuan.calliopecore.fr.autoroute.sentence

import com.rokuan.calliopecore.sentence.IAction.ActionType

/**
  * Created by Christophe on 09/12/2016.
  */
case class Action(action: ActionType, field: String, state: String, stateValue: String) {
  def this(a: ActionType, f: String) = this(a, f, null, null)
  def this(a: ActionType, f: FieldProperty, s: StateProperty) = this(a, Option(f).map(_.field).orNull, Option(s).map(_.state).orNull, Option(s).map(_.value).orNull)
  def this(a: ActionType) = this(a, null)
  def isFieldBound(): Boolean = field != null
  def isStateBound(): Boolean = state != null && stateValue != null
}

class FieldProperty(val field: String)
class StateProperty(val state: String, val value: String)
