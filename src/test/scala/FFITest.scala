package me.viluon.tinyc

import org.scalatest.flatspec.AnyFlatSpec

class FFITest extends AnyFlatSpec {
  "FFI" should "support parsing" in {
    val frontend = new tinyc.Frontend()
    val ast = frontend.parse("void main() {}")
    println(ast.toString)
  }

  "JVM" should "not crash" in {
    val ast = new tinyc.Frontend().parse(
      """void main() {
        |  int x;
        |  return x + 1;
        |}
        |""".stripMargin
    )
    println(ast)
  }
}
