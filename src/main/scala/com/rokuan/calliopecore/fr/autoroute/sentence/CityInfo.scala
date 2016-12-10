package com.rokuan.calliopecore.fr.autoroute.sentence

import com.rokuan.calliopecore.sentence.ICityInfo
import com.rokuan.calliopecore.sentence.ICityInfo.Location

/**
  * Created by Christophe on 09/12/2016.
  */
case class CityInfo(value: String, latitude: Double, longitude: Double) extends ICityInfo {
  override def getLocation: Location = new Location(latitude, longitude)
  override def getValue: String = value
}
