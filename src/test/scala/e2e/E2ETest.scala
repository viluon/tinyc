package me.viluon.tinyc
package e2e

import org.scalatest.flatspec.AnyFlatSpec
import tiny86.{Program, Target}

abstract class E2ETest extends AnyFlatSpec {
  def exec(pipe: Pipeline): Int = {
    val target = new Target()
    target.execute(pipe.run.get match {
      case p: Program => p
      case _ => ???
    })
    0
  }
}
