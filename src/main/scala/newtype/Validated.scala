package newtype

import cats.Applicative
import cats.kernel.Semigroup

private[newtype] object ValidatedImpl {
  private[newtype] trait Tag extends Any
  type Type[E, A] <: Either[E, A] with Tag

  def fromEither[E, A](e: Either[E, A]): Type[E, A] =
    e.asInstanceOf[Type[E, A]]

  def toEither[E, A](v: Type[E, A]): Either[E, A] =
    v.asInstanceOf[Either[E, A]]

  def Valid[E, A](a: A): Type[E, A] = fromEither(Right(a))

  def Invalid[E, A](e: E): Type[E, A] = fromEither(Left(e))

  implicit def applicativeV[E: Semigroup]: Applicative[Type[E, ?]] = new Applicative[Type[E, ?]] {
    def pure[A](x: A): Type[E, A] = fromEither(Right(x))

    def ap[A, B](ff: Type[E, A => B])(fa: Type[E, A]): Type[E, B] = toEither(fa) match {
      case e @ Left(_) => e.asInstanceOf[Type[E, B]]
      case Right(a) => toEither(ff) match {
        case e @ Left(_) => e.asInstanceOf[Type[E, B]]
        case Right(f) => Valid(f(a))
      }
    }
  }
}
