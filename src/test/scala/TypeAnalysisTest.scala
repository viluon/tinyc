package me.viluon.tinyc

import org.intellij.lang.annotations.Language
import org.scalatest.flatspec.AnyFlatSpec
import tinyc.{AST, Frontend, Symbol}
import cats.syntax.show._

class TypeAnalysisTest extends AnyFlatSpec {
  import TypeAnalysis.TypeError
  import TypeAnalysis.TypeErrorShow

  lazy val frontend = new Frontend()
  def parse(@Language(value = "JAVA", prefix = "class Foo { ", suffix = " }") code: String): AST = frontend.parse(code)

  def shouldFailWith(analysisResult: Either[List[TypeError], Map[Symbol, Type]], err: String): Unit = {
    val res = analysisResult.left.map(_.map(_.show))
    assert(res match {
      case Left(errs) => errs.exists(_.toLowerCase.contains(err.toLowerCase))
      case Right(_) => false
    }, s"Could not find the appropriate error message in $res")
  }

  "Type analysis" should "understand integers" in {
    val res = TypeAnalysis.analyse(parse(
      """void main() {
        |  int x = 3;
        |  return x + 1;
        |}
        |""".stripMargin
    ))
    print(Console.RESET)
    shouldFailWith(res, "expected void, got int")
  }
}
