import com.rokuan.autoroute.matchers.Route
import com.rokuan.calliopecore.fr.autoroute.data.CountConverter
import org.scalatest.{FlatSpec, Matchers}
import com.rokuan.autoroute.Producer._
import com.rokuan.calliopecore.fr.autoroute.sentence.Word
import com.rokuan.calliopecore.sentence.structure.data.count.{CountObject, FixedItemObject, LimitedItemsObject}
import com.rokuan.calliopecore.fr.autoroute.sentence.Word.WordType._

/**
  * Created by Christophe on 05/12/2016.
  */
class CountConversionSpec extends FlatSpec with Matchers {
  val parser = new Route(CountConverter.CountRule)

  "The parsing" should "produce a FixedItemObject" in {
    val words = List(
      new Word("le", DEFINITE_ARTICLE),
      new Word("premier", NUMERICAL_POSITION)
    )

    val count = parser(words)
    count.asInstanceOf[FixedItemObject].position shouldBe 1
  }

  "The parsing" should "produce a LimitedItemsObject with FIRST range" in {
    val words = List(
      new Word("les", DEFINITE_ARTICLE),
      new Word("5", NUMBER),
      new Word("premiers", NUMERICAL_POSITION)
    )

    val count = parser(words)
    val limited = count.asInstanceOf[LimitedItemsObject]
    limited.range shouldBe CountObject.Range.FIRST
    limited.count shouldBe 5
  }

  "The parsing" should "produce a LimitedItemsObject with LAST range" in {
    val words = List(
      new Word("les", DEFINITE_ARTICLE),
      new Word("7", NUMBER),
      new Word("derniers", NUMERICAL_POSITION)
    )

    val count = parser(words)
    val limited = count.asInstanceOf[LimitedItemsObject]
    limited.range shouldBe CountObject.Range.LAST
    limited.count shouldBe 7
  }
}
