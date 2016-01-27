package c4.io

import c4.messaging.Message
import c4.util.TestUtil
import org.scalatest._

import scala.collection.mutable.ArrayBuffer

class PPReaderTest extends FlatSpec with Matchers {
  it should "expand the example macros in c89 spec correctly" in {
    val fileName: String = TestUtil.createTempFile(
      """#define x    3
        |#define f(a) f(x * (a))
        |#undef  x
        |#define x    2
        |#define g    f
        |#define z    z[0]
        |#define h    g(~
        |#define m(a) a(w)
        |#define w    0,1
        |#define t(a) a
        |
        |f(y+1) + f(f(z)) % t(t(g)(0) + t)(1);
        |g(x+(3,4)-w) | h 5) & m
        |         (f)^m(m);
        |""".stripMargin)
    val warnings: ArrayBuffer[Message] = ArrayBuffer.empty
    val tokens: String =
      PPReader.read(warnings, fileName)
        .map(_.value)
        .filterNot(_.isInstanceOf[PPTokWhiteSpc])
        .map(_.raw)
        .mkString
    tokens should be (
      "f(2*(y+1))+f(2*(f(2*(z[0]))))%f(2*(0))+t(1);" +
        "f(2*(2+(3,4)-0,1))|f(2*(~5))&f(2*(0,1))^m(0,1);")
  }
}
