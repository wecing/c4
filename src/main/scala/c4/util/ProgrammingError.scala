package c4.util

object ProgrammingError {
  def apply(): Nothing = {
    throw new ProgrammingError
  }
}

final class ProgrammingError extends RuntimeException
