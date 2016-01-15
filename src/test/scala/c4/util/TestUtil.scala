package c4.util

import java.io.{File, FileOutputStream}

object TestUtil {
  /**
   * Creates a temporary file with given content, returns its absolute path.
   */
  def createTempFile(input: String): String = {
    val file = File.createTempFile("c4_test_", ".tmp.c")
    val stream = new FileOutputStream(file)
    stream.write(input.getBytes)
    stream.close()

    file.getAbsolutePath
  }
}
