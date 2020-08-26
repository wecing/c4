package c4.util

import c4.ast._

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

  def fst[A, B](t: (A, B)): A = t._1
  def snd[A, B](t: (A, B)): B = t._2

  def head[T](xs: Seq[T]): T = xs.head
  def last[T](xs: Seq[T]): T = xs.last

  def checkTypedef(
      dss: Seq[Located[DeclarationSpecifier]],
      decls: Seq[(Located[Declarator], Option[Located[Initializer]])],
      scanner: C4Scanner
  ): Unit = {
    val hasTypedef = dss map { _.value } exists {
      case DeclarationSpecifierStorageClass(Located(_, Typedef)) => true
      case _                                                     => false
    }
    if (hasTypedef) {
      def getName(dd: DirectDeclarator): String = {
        dd match {
          case DirectDeclaratorId(x)                     => x.value.id
          case DirectDeclaratorDeclarator(Located(_, d)) => getName(d.dd.value)
          case DirectDeclaratorArray(dd1, _)             => getName(dd1.value)
          case DirectDeclaratorFuncTypes(dd1, _, _)      => getName(dd1.value)
          case DirectDeclaratorIdsList(dd1, _)           => getName(dd1.value)
        }
      }
      decls
        .map(decl => getName(decl._1.value.dd.value))
        .foreach(scanner.addTypedefName)
    }
  }
}
