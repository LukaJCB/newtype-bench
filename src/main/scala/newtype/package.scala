package object newtype {
  type V[E, A] = ValidatedImpl.Type[E, A]
  val V = ValidatedImpl

  type Nest[F[_], G[_], A] = NestedImpl.Type[F, G, A]
  val Nest = NestedImpl
}
