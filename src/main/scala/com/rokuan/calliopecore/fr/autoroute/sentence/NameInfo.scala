package com.rokuan.calliopecore.fr.autoroute.sentence

import com.rokuan.calliopecore.sentence.INameInfo

/**
  * Created by Christophe on 09/12/2016.
  */
case class NameInfo(value: String, tag: String) extends INameInfo {
  override def getNameTag: String = tag
  override def getValue: String = value
}
