package com.rokuan.calliopecore.fr.autoroute.sentence

import com.rokuan.calliopecore.sentence.ITransportInfo
import com.rokuan.calliopecore.sentence.structure.data.way.TransportObject.TransportType

/**
  * Created by Christophe on 09/12/2016.
  */
case class TransportInfo(value: String, transportType: TransportType) extends ITransportInfo {
  override def getTransportType: TransportType = transportType
  override def getValue: String = value
}
