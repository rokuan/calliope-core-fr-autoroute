package com.rokuan.calliopecore.fr.autoroute.data

import com.rokuan.calliopecore.fr.sentence.Word
import com.rokuan.calliopecore.sentence.IWayPreposition
import com.rokuan.calliopecore.sentence.structure.data.nominal.{ColorObject, LanguageObject}
import com.rokuan.calliopecore.sentence.structure.data.way.{AdditionalMode, TransportObject}
import com.rokuan.calliopecore.sentence.structure.data.way.WayAdverbial.WayType

/**
  * Created by Christophe on 04/12/2016.
  */
object WayConverter {
  import com.rokuan.calliopecore.fr.autoroute.pattern.WordRules._
  import com.rokuan.calliopecore.fr.autoroute.pattern.PrepositionRules._
  import com.rokuan.calliopecore.fr.sentence.Word.WordType._

  val WayPrepositionTransformer = { p: WayType =>
    prep(p) { _.getWayPreposition }
  }
  val LanguageTransformer = word(LANGUAGE) { l =>
    new LanguageObject { language = l.getLanguageInfo }
  }
  val ColorTransformer = word(COLOR) { c =>
    new ColorObject { color = c.getColorInfo }
  }
  val AdditionalModeTransformer = word(CUSTOM_MODE) { m =>
    new AdditionalMode { mode = m.getCustomMode }
  }

  val MeansOfTransportRule = (WayPrepositionTransformer(WayType.TRANSPORT) ~ word(MEAN_OF_TRANSPORT)) {
    case List(preposition: IWayPreposition, mean: Word) =>
      val transport = new TransportObject {
        transportType = mean.getTransportInfo.getTransportType
      }
      transport.setWayPreposition(preposition)
      transport
  }
  val LanguageRule = (WayPrepositionTransformer(WayType.LANGUAGE) ~ LanguageTransformer) {
    case List(preposition: IWayPreposition, language: LanguageObject) =>
      language.setWayPreposition(preposition)
      language
  }
  val ColorRule = (WayPrepositionTransformer(WayType.COLOR) ~ ColorTransformer) {
    case List(preposition: IWayPreposition, color: ColorObject) =>
      color.setWayPreposition(preposition)
      color
  }
  val ModeRule = (word(WAY_PREPOSITION) ~ AdditionalModeTransformer){
    case List(preposition: Word, mode: AdditionalMode) =>
      mode.setWayPreposition(preposition.getWayPreposition)
      mode
  }
  val WayAdverbialRule = MeansOfTransportRule | LanguageRule | ModeRule | ColorRule
}
