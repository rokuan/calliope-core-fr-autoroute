import com.rokuan.autoroute.matchers.Route
import com.rokuan.calliopecore.fr.autoroute.data.NominalGroupConverter
import com.rokuan.calliopecore.fr.autoroute.sentence.{CustomObject, NameInfo, Word}
import com.rokuan.calliopecore.fr.autoroute.sentence.Word.WordType._
import com.rokuan.calliopecore.sentence.structure.data.nominal.{AdditionalObject, NameObject}
import com.rokuan.calliopecore.sentence.structure.data.nominal.NominalGroup.GroupType
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Christophe on 06/12/2016.
  */
class NominalGroupConversionSpec extends FlatSpec with Matchers {
  val directObjectParser = new Route(NominalGroupConverter.DirectObjectRule)
  val subjectParser = new Route(NominalGroupConverter.SubjectRule)

  "1" should "2" in {
    val cat = new Word("chat", nameInfo = new NameInfo("chat", "CAT"))
    val words = List(
      new Word("le", DEFINITE_ARTICLE),
      cat
    )
    val nominal = directObjectParser(words)
    nominal.getGroupType shouldBe GroupType.COMMON_NAME
    val complement = nominal.asInstanceOf[NameObject]
    complement.`object`.getValue shouldBe "chat"
  }

  "2" should "3" in {
    val objectName = "QR code"
    val qr = new Word(objectName, customObject = new CustomObject(objectName, "QR_CODE"))

    val words = List(
      new Word("le", DEFINITE_ARTICLE),
      qr
    )
    val nominal = directObjectParser(words)
    nominal.getGroupType shouldBe GroupType.OBJECT
    nominal.asInstanceOf[AdditionalObject].`object`.getValue shouldBe objectName
  }
}
