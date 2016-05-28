package cats
package arrow

import simulacrum.typeclass

/**
 * Compose represents a high level abstraction for the compose operations for arrows.
 */
@typeclass trait Compose[F[_, _]] extends Serializable { self =>
  
  /** 
    * composes a `F[B, C]` with a `F[A, B]` to form a `F[A, C]`
    */
  @simulacrum.op("<<<", alias=true)
  def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C]

  /**
    * composes a `F[A, B]` with a `F[B, C]` to form a `F[A, C]` 
    */
  @simulacrum.op(">>>", alias=true)
  def andThen[A, B, C](f: F[A, B], g: F[B, C]): F[A, C] =
    compose(g, f)

  def algebraK: SemigroupK[λ[α => F[α, α]]] =
    new SemigroupK[λ[α => F[α, α]]] {
      def combineK[A](f1: F[A, A], f2: F[A, A]): F[A, A] = self.compose(f1, f2)
    }

  def algebra[A]: Semigroup[F[A, A]] =
    new Semigroup[F[A, A]] {
      def combine(f1: F[A, A], f2: F[A, A]): F[A, A] = self.compose(f1, f2)
    }
}


