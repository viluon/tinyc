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

  def callees: List[L] = targets.map(_.callee)
  def targets: List[Target[B, L]]
  def mapTargets[F[_]](f: Target[B, L] => F[Target[B, L]])(implicit ev: Monad[F]): F[Continuation[B, L]]
}

object Continuation {
  // TODO shouldn't the args be just types?
  case class Target[B, L](callee: L, args: List[B])

  case class Branch[B, L](condition: B, consequent: Target[B, L], alternative: Target[B, L])
    extends Continuation[B, L] {
    override def targets: List[Target[B, L]] = List(consequent, alternative)

    override def mapTargets[F[_]](f: Target[B, L] => F[Target[B, L]])(implicit ev: Monad[F]): F[Continuation[B, L]] = for {
      con <- f(consequent)
      alt <- f(alternative)
    } yield copy(consequent = con, alternative = alt)
  }

  case class Unconditional[B, L](next: Target[B, L]) extends Continuation[B, L] {
    override def targets: List[Target[B, L]] = List(next)
    override def mapTargets[F[_]](f: Target[B, L] => F[Target[B, L]])(implicit ev: Monad[F]): F[Continuation[B, L]] =
      f(next).map(Unconditional(_))
  }

  /**
   * The return address is deliberately not a target, because
   * 1) there can only be one argument
   * 2) we do not statically know all the callsites
   *
   * FIXME bs, we do need extensible argument lists, how else will we access upvalues (variables, temporaries)?
   * the trouble is that
   * - this requires multiple return values
   * - the return target is scoped to the fn block, i.e. its arguments refer to fn's params,
   *   NOT to the params in the current block
   *
   * so maybe we can fix this? Can we keep the return address here but the params in [[Continuation.Return]]?
   *
   * The rabbit hole goes deeper, my dear Alice.
   * Return values are *weird* in our framework. What we want for the return target of a call continuation is
   * a parameter list _from the caller_, not the callee. But the return value comes from the callee!
   * Hence, for
   *   void A() {
   *     int x = 4;
   *     int y = 2;
   *     print(B(x) + y);
   *   }
   *   int B(int x) {
   *     int z = 2 * x;
   *     return z;
   *   }
   *
   * we have:
   * +-----------------+
   * | block A1()      |
   * | int x = 4       |
   * | int y = 2       |
   * | call B(x)       | ---> +-----------------+
   * +-----------------+      | block B(x)      |
   *                          | int z = 2 * x   |
   * +-----------------+ <--- | return z        |
   * | block A2(r, y)  |      +-----------------+
   * | print(r + y)    | // y is defined in A1, it needs to be passed by the call continuation
   * +-----------------+
   *
   * so both the caller and the callee have parameters to pass to the return target.
   */
  case class Call[B, L](fn: Target[B, L], ret: Target[B, L]) extends Continuation[B, L] {
    override def targets: List[Target[B, L]] = List(fn, ret)
    override def mapTargets[F[_]](f: Target[B, L] => F[Target[B, L]])(implicit ev: Monad[F]): F[Continuation[B, L]] =
      for {
        t <- f(fn)
        r <- f(ret)
      } yield Call(t, r)
  }

  // TODO should take an args list, but for now only one return value is supported
  case class Return[B, L](value: B) extends Continuation[B, L] {
    override def targets: List[Target[B, L]] = List()
    override def mapTargets[F[_]](f: Target[B, L] => F[Target[B, L]])(implicit ev: Monad[F]): F[Continuation[B, L]] =
      ev.pure(this)
  }

  case class Halt[B, L]() extends Continuation[B, L] {
    override def targets: List[Target[B, L]] = List()
    override def mapTargets[F[_]](f: Target[B, L] => F[Target[B, L]])(implicit ev: Monad[F]): F[Continuation[B, L]] =
      ev.pure(this)
  }
}
