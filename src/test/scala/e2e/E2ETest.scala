package me.viluon.tinyc
package e2e

import org.intellij.lang.annotations.Language
import org.scalatest.flatspec.AnyFlatSpec
import tiny86.{Program, Register, Target}

abstract class E2ETest extends AnyFlatSpec {
  def pipe(@Language(value = "JAVA", prefix = "class Foo { ", suffix = " }") code: String): Pipeline = Pipeline(code)

  def exec(pipe: Pipeline): Long = {
    import scala.jdk.CollectionConverters._

    val target = new Target()
    val (program, outputReg) = pipe.run.get match {
      case (p: Program, reg: Register) => p -> reg
      case _ => ???
    }
    val res = target.execute(program, true)

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
    snapshots.last(outputReg.toString)
  }
}
