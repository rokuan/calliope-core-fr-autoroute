package com.rokuan.calliopecore.fr.autoroute.sentence

import com.rokuan.calliopecore.sentence.IValue

/**
  * Created by Christophe on 09/12/2016.
  */
case class FirstNameInfo(value: String) extends IValue {
  override def getValue: String = value
}
