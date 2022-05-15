package me.viluon.tinyc
package e2e

import org.scalatest.flatspec.AnyFlatSpec
import tiny86.{Program, Target}

abstract class E2ETest extends AnyFlatSpec {
  def exec(pipe: Pipeline): Long = {
    import scala.jdk.CollectionConverters._

    val target = new Target()
    val res = target.execute(pipe.run.get match {
      case p: Program => p
      case _ => ???
    }, true)

    println(res.getStats)
    val snapshots = List.from(res.getSnapshots.asScala.map(_.asScala).map(Map.from(_)))
    (snapshots.head :: snapshots).sliding(2).zipWithIndex.foreach { case (List(bef, aft), i) =>
      println(s"--- step $i ---")
      println(aft
        .toList
        .sortBy(_._1)
        .map { case (k, v) => ".%8s".formatted(k) + " := " + v + (if (bef(k) != v) " <- changed" else "") }
        .mkString("\n")
      )
    }
    snapshots.last("Reg0")
  }
}
