package com.rokuan.calliopecore.fr.autoroute.sentence

import com.rokuan.calliopecore.sentence.IAction.ActionType
import com.rokuan.calliopecore.sentence.IValue

/**
  * Created by Christophe on 09/12/2016.
  */
case class Action(action: ActionType, field: String, state: String, stateValue: String, target: Boolean) extends IValue {
  def this(a: ActionType, f: String) = this(a, f, null, null, false)
  def this(a: ActionType, f: FieldProperty, s: StateProperty) = this(a, Option(f).map(_.field).orNull, Option(s).map(_.state).orNull, Option(s).map(_.value).orNull, false)
  def this(a: ActionType) = this(a, null)
  def isFieldBound(): Boolean = field != null
  def isStateBound(): Boolean = state != null && stateValue != null
  def isTargetAction(): Boolean = target
  override def getValue: String = action.name()
}

class FieldProperty(val field: String)
class StateProperty(val state: String, val value: String)
