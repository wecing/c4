package c4.io

import c4.messaging.{IllegalSourceException, Message}
import c4.util.TestUtil
import org.scalatest._

import scala.collection.mutable.ArrayBuffer

class SourcePhase2ReaderTest extends FlatSpec with Matchers {
  it should "replace single trigraphs" in {
    checkReader("??=\n", Seq(("#", (1, 1)), ("\n", (1, 4))))
    checkReader("??(\n", Seq(("[", (1, 1)), ("\n", (1, 4))))
    checkReader("??/_\n", Seq(("\\", (1, 1)), ("_\n", (1, 4))))
    checkReader("??)\n", Seq(("]", (1, 1)), ("\n", (1, 4))))
    checkReader("??'\n", Seq(("^", (1, 1)), ("\n", (1, 4))))
    checkReader("??<\n", Seq(("{", (1, 1)), ("\n", (1, 4))))
    checkReader("??!\n", Seq(("|", (1, 1)), ("\n", (1, 4))))
    checkReader("??>\n", Seq(("}", (1, 1)), ("\n", (1, 4))))
    checkReader("??-\n", Seq(("~", (1, 1)), ("\n", (1, 4))))
  }

  it should "replace consecutive trigraphs" in {
    val input: String = "??=??(??/??)??'??<??!??>?????-\n"
    val expected: Seq[(String, (Int, Int))] = Seq(
      ("#", (1, 1)),
      ("[", (1, 4)),
      ("\\", (1, 7)),
      ("]", (1, 10)),
      ("^", (1, 13)),
      ("{", (1, 16)),
      ("|", (1, 19)),
      ("}", (1, 22)),
      ("???~", (1, 25)),
      ("\n", (1, 31))
    )
    checkReader(input, expected)
  }

  it should "handle '??/' correctly" in {
    checkReader("??/nice\n", Seq(("\\", (1, 1)), ("nice\n", (1, 4))))
    checkReader("??/\nnice\n", Seq(("nice\n", (2, 1))))
  }

  it should "preserve extra '?' before trigraphs" in {
    checkReader("?????=\n", Seq(("???#", (1, 1)), ("\n", (1, 7))))
    checkReader("???/nice\n", Seq(("?\\", (1, 1)), ("nice\n", (1, 5))))

  }

  it should "recognize -> as an operator" in {
    checkReader("memcpy(&sin, ai->ai_addr, sizeof sin);\n", Seq(
      ("memcpy", (1, 1)),
      ("(", (1, 7)),
      ("&", (1, 8)),
      ("sin", (1, 9)),
      (",", (1, 12)),
      (" ", (1, 13)),
      ("ai", (1, 14)),
      ("->", (1, 16)),
      ("ai_addr", (1, 18)),
      (",", (1, 25)),
      (" ", (1, 26)),
      ("sizeof", (1, 27)),
      (" ", (1, 33)),
      ("sin", (1, 34)),
      (")", (1, 37)),
      (";", (1, 38)),
      ("\n", (1, 39))
    ))

  }

  it should "remove backslash followed by a newline" in {
    val input: String = "such \t \\\nscala\\\nvery\\ \nsyntax\n"
    val expected: Seq[(String, (Int, Int))] = Seq(
      ("such \t ", (1, 1)),
      ("scala", (2, 1)),
      ("very\\ \n", (3, 1)), // no warnings!
      ("syntax\n", (4, 1))
    )
    checkReader(input, expected, 0)
  }

  // a simple, naive test
  it should "still work when things get more complicated" in {
    val input: String = Seq(
      "??=include <stdio.h>",
      "nice\\", // JetBrain's scala plugin doesn't support raw string :(
      "to meet??/",
      "you",
      "bye"
    ).mkString("\n") + "\n"

    val expected: Seq[(String, (Int, Int))] = Seq(
      ("#", (1, 1)),
      ("include <stdio.h>\n", (1, 4)),
      ("nice", (2, 1)),
      ("to meet", (3, 1)),
      ("you\n", (4, 1)),
      ("bye\n", (5, 1))
    )

    checkReader(input, expected)
  }

  it should "reject files that do not end with newline" in {
    val e: IllegalSourceException = intercept[IllegalSourceException] {
      checkReader("hi", Seq(("hi", (1, 1))))
    }
    e.msg.location should be (1, 2)
  }

  it should "reject files that end with \\ followed by newline" in {
    val e1: IllegalSourceException = intercept[IllegalSourceException] {
      checkReader("hi\\\n", Seq(("hi", (1, 1))))
    }
    e1.msg.location should be (1, 3)
    val e2: IllegalSourceException = intercept[IllegalSourceException] {
      checkReader("hi??/\n", Seq(("hi", (1, 1))))
    }
    e2.msg.location should be (1, 3)
    val e3: IllegalSourceException = intercept[IllegalSourceException] {
      checkReader("hi\n\\\n", Seq(("hi\n", (1, 1))))
    }
    e3.msg.location should be (2, 1)
    val e4: IllegalSourceException = intercept[IllegalSourceException] {
      checkReader("hi\n??/\n", Seq(("hi\n", (1, 1))))
    }
    e4.msg.location should be (2, 1)
  }

  def checkReader(input: String,
                  expected: Seq[(String, (Int, Int))],
                  expectedWarningsCount: Int = 0): Unit = {
    val warnings: ArrayBuffer[Message] = ArrayBuffer.empty
    val tempFilePath: String = TestUtil.createTempFile(input)
    val reader = new SourcePhase2Reader(warnings, tempFilePath)

    var restExpected: Seq[(String, (Int, Int))] = expected
    while (restExpected.nonEmpty) {
      val head: (String, (Int, Int)) = restExpected.head
      reader.read should be (Some((head._1.charAt(0), head._2)))

      if (1 < head._1.length) {
        restExpected = (head._1.substring(1), (head._2._1, head._2._2 + 1)) +:
          restExpected.tail
      } else {
        restExpected = restExpected.tail
      }
    }

    reader.read() should be (None)
    warnings.length should be (expectedWarningsCount)

    reader.close()
  }
}
