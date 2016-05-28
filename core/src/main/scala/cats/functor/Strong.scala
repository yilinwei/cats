package cats
package functor

import simulacrum.typeclass

/**
 * Must obey the laws defined in cats.laws.StrongLaws.
 */
@typeclass trait Strong[F[_, _]] extends Profunctor[F] {
  def first[A, B, C](fa: F[A, B]): F[(A, C), (B, C)]
  def second[A, B, C](fa: F[A, B]): F[(C, A), (C, B)]
}


