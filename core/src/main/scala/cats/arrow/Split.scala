package cats
package arrow

import simulacrum.typeclass

@typeclass trait Split[F[_, _]] extends Compose[F] { self =>

  /**
    * split the argument `(A, C)` between two arrows which take in `A` and `C` and combine the output.
    */
  @simulacrum.op("***", alias=true)
  def split[A, B, C, D](f: F[A, B], g: F[C, D]): F[(A,  C), (B, D)]
  
}
