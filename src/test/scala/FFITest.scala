package me.viluon.tinyc

import org.scalatest.flatspec.AnyFlatSpec

class FFITest extends AnyFlatSpec {
  "FFI" should "support parsing" in {
    val frontend = new tinyc.Frontend()
    val ast = frontend.parse("void main() {}")
    println(ast.toString)
  }
}
