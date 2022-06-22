package me.viluon.tinyc
package ir

import cats.Monad
import cats.syntax.all._

/**
 * A continuation, the terminator of a basic block.
 *
 * @tparam B The binder type
 * @tparam L The label type
 */
sealed trait Continuation[B, L] {
  import ir.Continuation.Target

  def callees: Set[L] = targets.map(_.callee)
  def targets: Set[Target[B, L]]
  def mapTargets[F[_]](f: Target[B, L] => F[Target[B, L]])(implicit ev: Monad[F]): F[Continuation[B, L]]
}

object Continuation {
  case class Target[B, L](callee: L, args: List[B])

  case class Branch[B, L](condition: B, consequent: Target[B, L], alternative: Target[B, L])
    extends Continuation[B, L] {
    override def targets: Set[Target[B, L]] = Set(consequent, alternative)

    override def mapTargets[F[_]](f: Target[B, L] => F[Target[B, L]])(implicit ev: Monad[F]): F[Continuation[B, L]] = for {
      con <- f(consequent)
      alt <- f(alternative)
    } yield copy(consequent = con, alternative = alt)
  }

  case class Unconditional[B, L](next: Target[B, L]) extends Continuation[B, L] {
    override def targets: Set[Target[B, L]] = Set(next)

    override def mapTargets[F[_]](f: Target[B, L] => F[Target[B, L]])(implicit ev: Monad[F]): F[Continuation[B, L]] =
      f(next).map(Unconditional(_))
  }

  case class Halt[B, L]() extends Continuation[B, L] {
    override def targets: Set[Target[B, L]] = Set()

    override def mapTargets[F[_]](f: Target[B, L] => F[Target[B, L]])(implicit ev: Monad[F]): F[Continuation[B, L]] =
      (this: Continuation[B, L]).pure
  }
}
