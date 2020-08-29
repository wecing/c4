package c4.io

import java.net.URL

import c4.messaging.{SimpleMessage, IllegalSourceException, Message}
import c4.util.{Located => L, TestUtil}
import org.scalatest._
import flatspec._
import matchers._

import scala.collection.mutable.ArrayBuffer

class SourcePhase7ReaderTest extends AnyFlatSpec with should.Matchers {
  it should "pass tcc's pp tests" in {
    // TODO: ## not yet fully implemented
    // checkPP("01.c", "01.expect")
    checkPP("02.c", "02.expect")
    checkPP("03.c", "03.expect")
    checkPP("04.c", "04.expect")
    checkPP("05.c", "05.expect")
    checkPP("06.c", "06.expect")
    checkPP("07.c", "07.expect")
    checkPP("08.c", "08.expect")
    checkPP("09.c", "09.expect")
    checkPP("10.c", "10.expect")
  }

  it should "pass our own tests" in {
    read("42.5") should be(Seq(TokDouble(42.5)))
    read("42.5e0") should be(Seq(TokDouble(42.5)))
    read("42.5e1") should be(Seq(TokDouble(425)))
    read("42.5e+1") should be(Seq(TokDouble(425)))
    read("42.5e-1") should be(Seq(TokDouble(4.25)))

    read("'a'") should be(Seq(TokChar('a')))

    read("#define t(x,y) x ## y\nt(1,2.0)") should be(Seq(TokDouble(12.0)))
    read("#define t(x,y) x ## y\nt(1 , 2.0)") should be(Seq(TokDouble(12.0)))
    read("#define t(x,y,z) x ## y ## z\nt(1,2,3.0)") should be(
      Seq(TokDouble(123.0))
    )
    read("#define t(x,y,z) x ## y ## z\nt(,4,5.0)") should be(
      Seq(TokDouble(45.0))
    )

    read("#define t(x,y) x ## y\nt(1,2)") should be(
      Seq(TokInteger(12, Int32, true))
    )
  }

  // TODO: more Phase7Reader specific tests here?

  def checkPP(srcPath: String, expectedPath: String): Unit = {
    val srcWarnings: ArrayBuffer[Message] = ArrayBuffer.empty
    val expectedWarnings: ArrayBuffer[Message] = ArrayBuffer.empty

    val pathPrefix = "/pp_test_data/"

    val srcUrl: URL = getClass.getResource(pathPrefix + srcPath)
    val tempSrcPath: String = TestUtil.createTempFile(srcUrl)

    val expUrl: URL = getClass.getResource(pathPrefix + expectedPath)
    val tempExpPath: String = TestUtil.createTempFile(expUrl)

    try {
      val srcReader = new SourcePhase7Reader(srcWarnings, tempSrcPath)
      val expReader = new SourcePhase7Reader(expectedWarnings, tempExpPath)

      var quit = false
      while (!quit) {
        val srcTok: Option[L[Tok]] = srcReader.get()
        val expTok: Option[L[Tok]] = expReader.get()
        (srcTok, expTok) match {
          case (Some(srcT), Some(expT)) =>
            if (expT.value != srcT.value) {
              fail(s"${srcT.loc}: expected ${expT.value}, got ${srcT.value}")
            }
          case (None, None) =>
            quit = true
          case (Some(srcT), None) =>
            fail(s"unexpected token at ${srcT.loc}")
          case (None, Some(expT)) =>
            fail(s"missing expected token ${expT.value} at ${expT.loc}")
        }
      }

      srcWarnings.size should be(0)
      expectedWarnings.size should be(0)
    } catch {
      case e @ IllegalSourceException(msg: SimpleMessage) =>
        val fileName =
          if (msg.fileName == tempSrcPath) {
            srcPath
          } else if (msg.fileName == tempExpPath) {
            expectedPath
          } else {
            msg.fileName
          }
        val errMsg: String = msg.copy(fileName = fileName).toString
        fail(s"unexpected IllegalArgumentException: $errMsg", e)
      case e: IllegalSourceException =>
        fail(s"unexpected IllegalArgumentException: ${e.msg.toString}", e)
    }
  }

  def read(content: String): Seq[Tok] = {
    val srcUrl = TestUtil.createTempFile(content + "\n")
    val srcWarnings: ArrayBuffer[Message] = ArrayBuffer.empty
    val srcReader = new SourcePhase7Reader(srcWarnings, srcUrl)

    val toks: ArrayBuffer[Tok] = ArrayBuffer.empty
    while (srcReader.get().map(t => toks += t.value).isDefined) {
      // loop
    }
    toks.toSeq
  }
}
