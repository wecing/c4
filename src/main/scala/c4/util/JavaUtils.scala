package c4.util

object JavaUtils {
  def some[T](t: T): Option[T] = Some(t)
  def none[T](): Option[T] = None

  def pair[T, P](t: T, p: P) = (t, p)

  def seq[T](): Seq[T] = Seq.empty
  def seq[T](t1: T) = Seq(t1)
  def seq[T](t1: T, t2: T) = Seq(t1, t2)

  def prepend[T](x: T, xs: Seq[T]) = x +: xs
  def append[T](xs: Seq[T], x: T) = xs :+ x

  def left[A, B](a: A): Either[A, B] = Left(a)
  def right[A, B](b: B): Either[A, B] = Right(b)
}
