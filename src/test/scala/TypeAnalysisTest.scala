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

  implicit class AnalysisResultOps(analysisResult: Either[List[TypeError], Map[Symbol, Type]]) {
    def shouldFailWith(err: String): Unit = {
      val res = analysisResult.left.map(_.map(_.show))
      res match {
        case Left(errs) => assert(
          errs.exists(_.toLowerCase.contains(err.toLowerCase)),
          s"Could not find the appropriate error message in $errs"
        )
        case Right(v) => fail(s"Unexpected success ($v)")
      }
    }
  }

  "Type analysis" should "understand integers" in {
    TypeAnalysis analyse parse(
      """void main() {
        |  int x = 3;
        |  return x + 1;
        |}
        |""".stripMargin
    ) shouldFailWith "expected void, got int"
  }
}
