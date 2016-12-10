package com.rokuan.calliopecore.fr.autoroute.sentence

import com.rokuan.calliopecore.sentence.ICountryInfo

/**
  * Created by Christophe on 09/12/2016.
  */
case class CountryInfo(value: String, code: String) extends ICountryInfo {
  override def getCountryCode: String = code
  override def getValue: String = value
}
