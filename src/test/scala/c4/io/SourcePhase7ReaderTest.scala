package c4.io

import java.net.URL

import c4.messaging.{SimpleMessage, IllegalSourceException, Message}
import c4.util.{Located => L, TestUtil}
import org.scalatest._

import scala.collection.mutable.ArrayBuffer

class SourcePhase7ReaderTest extends FlatSpec with Matchers {
  it should "pass tcc's pp tests" in {
    // TODO: ## not yet fully implemented
    // checkPP("/pp/01.c", "/pp/01.expect")
    checkPP("/pp/02.c", "/pp/02.expect")
    checkPP("/pp/03.c", "/pp/03.expect")
    checkPP("/pp/04.c", "/pp/04.expect")
    // TODO: ## not yet fully implemented
    // checkPP("/pp/05.c", "/pp/05.expect")
    checkPP("/pp/06.c", "/pp/06.expect")
    checkPP("/pp/07.c", "/pp/07.expect")
    checkPP("/pp/08.c", "/pp/08.expect")
    // TODO: is "a##b##c" allowed in c89?
    // checkPP("/pp/09.c", "/pp/09.expect")
    checkPP("/pp/10.c", "/pp/10.expect")
  }

  // TODO: more Phase7Reader specific tests here?

  def checkPP(srcPath: String, expectedPath: String): Unit = {
    val srcWarnings: ArrayBuffer[Message] = ArrayBuffer.empty
    val expectedWarnings: ArrayBuffer[Message] = ArrayBuffer.empty

    val srcUrl: URL = getClass.getResource(srcPath)
    val tempSrcPath: String = TestUtil.createTempFile(srcUrl)

    val expUrl: URL = getClass.getResource(expectedPath)
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
      case e@IllegalSourceException(msg: SimpleMessage) =>
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
}
