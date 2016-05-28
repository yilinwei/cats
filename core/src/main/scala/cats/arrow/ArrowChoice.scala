package cats
package arrow

import cats.data.Xor

import simulacrum.typeclass

@typeclass trait ArrowChoice[F[_, _]] extends Arrow[F] with Choice[F] {
  /**
    *  Given an arrow turn it into a left arrow
    */
  def leftArrow[A, B, C](fab: F[A, B]): F[A Xor C, B Xor C] =
    choice(andThen(fab, lift(Xor.left[B, C])), andThen(id[C], lift(Xor.right[B, C])))

  /**
    *  Given an arrow turn it into a right arrow
    */
  def rightArrow[A, B, C](fab: F[A, B]): F[C Xor A, C Xor B] =
    choice(andThen(id[C], lift(Xor.left[C, B])), andThen(fab, lift(Xor.right[C, B])))

  @simulacrum.op("+++", alias=true)
  def choiceSplit[A, B, C, D](fab: F[A, B], fcd: F[C, D]): F[A Xor C, B Xor D] =
    choice(leftArrow(fab), rightArrow(fcd))


}
