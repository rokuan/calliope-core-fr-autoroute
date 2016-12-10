package com.rokuan.calliopecore.fr.autoroute.sentence

import com.rokuan.calliopecore.sentence.ILanguageInfo

/**
  * Created by Christophe on 09/12/2016.
  */
case class LanguageInfo(value: String, code: String) extends ILanguageInfo {
  override def getLanguageCode: String = code
  override def getValue: String = value
}
