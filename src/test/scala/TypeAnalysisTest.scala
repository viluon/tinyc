package me.viluon.tinyc

import org.intellij.lang.annotations.Language
import org.scalatest.flatspec.AnyFlatSpec
import tinyc.{AST, Frontend}

class TypeAnalysisTest extends AnyFlatSpec {
  lazy val frontend = new Frontend()
  def parse(@Language(value = "JAVA", prefix = "class Foo { ", suffix = " }") code: String): AST = frontend.parse(code)

  "Type analysis" should "understand integers" in {
    TypeAnalysis.analyse(parse(
      """void main() {
        |  int x;
        |  return x + 1;
        |}
        |""".stripMargin
    ))
  }
}
