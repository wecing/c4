package c4.util

import c4.messaging.IllegalSourceException
import c4.util.legacy.{Located => L}
import org.scalatest._

class TextUtilsTest extends FlatSpec with Matchers {
  "TextUtils.fromStrReprQ" should "evaluate C string literals correctly" in {
    eval(raw"") should be ("")
    eval(raw"1") should be ("1")
    eval(raw"\n") should be ("\n")
    eval(raw"1\n") should be ("1\n")
    eval(raw"\n1") should be ("\n1")
    eval(raw"\n\n") should be ("\n\n")
    eval(raw"\r\n") should be ("\r\n")

    eval(raw"\'") should be ("'")
    eval("\\\"") should be ("\"")
    eval(raw"\?") should be ("\u003f")
    eval(raw"\\") should be ("\\")

    eval(raw"\0") should be ("\u0000")
    eval(raw"\00") should be ("\u0000")
    eval(raw"\000") should be ("\u0000")
    eval(raw"\0000") should be ("\u00000")

    an [IllegalSourceException] should be thrownBy eval(raw"\")
    an [IllegalSourceException] should be thrownBy eval(raw"\x")

    eval(raw"\xa") should be ("\u000a")
    eval(raw"\xaa") should be ("\u00aa")
    eval(raw"\xadF") should be ("\u0adf")
    eval(raw"\x9946") should be ("\u9946")
    eval(raw"\x9946z") should be ("\u9946z")
    eval(raw"\x9946\n") should be ("\u9946\n")
    an [IllegalSourceException] should be thrownBy eval(raw"\x9946F")
    an [IllegalSourceException] should be thrownBy eval(raw"\x9946F\n")
  }

  def eval(s: String): String = {
    TextUtils.fromStrReprQ(L((-1, -1), "\"" + s + "\""))
  }
}
