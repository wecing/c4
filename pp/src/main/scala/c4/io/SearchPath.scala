package c4.io

import java.nio.file.Paths

object SearchPath {
  def find(curFile: String, file: String): String = {
    if (Paths.get(file).isAbsolute()) {
      return file
    }

    val parentPath =
      Option(Paths.get(curFile).getParent()).map(_.toString()).getOrElse("")
    parentPath + "/" + file
  }
}
